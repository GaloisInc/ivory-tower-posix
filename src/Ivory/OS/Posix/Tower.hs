{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Posix.Tower (
  compileTowerPosix
) where

import Control.Monad (forM_)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Ivory.Artifact
import Ivory.Compile.C.CmdlineFrontend (runCompiler)
import Ivory.Language
import Ivory.OS.Posix.Tower.EventLoop
import Ivory.Stdlib.Control
import Ivory.Tower
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Options
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.Emitter

data MonitorCode = MonitorCode
  { monitorGenCode :: ModuleDef
  , monitorUserCode :: ModuleDef
  }

instance Monoid MonitorCode where
  mempty = MonitorCode mempty mempty
  mappend a b = MonitorCode
    { monitorGenCode = monitorGenCode a >> monitorGenCode b
    , monitorUserCode = monitorUserCode a >> monitorUserCode b
    }

data EmitterCode = EmitterCode
  { emitterInit :: forall eff. Ivory eff ()
  , emitterDeliver :: forall eff. Ivory eff ()
  , emitterRecipients :: [Unique]
  , emitterCode :: ModuleDef
  }

data PosixBackend = PosixBackend

instance TowerBackend PosixBackend where
  newtype TowerBackendCallback PosixBackend a = PosixCallback (forall s. (Def ('[ConstRef s a] :-> ()), ModuleDef))
  newtype TowerBackendEmitter PosixBackend = PosixEmitter (Maybe EmitterCode)
  data TowerBackendHandler PosixBackend a = PosixHandler
    { handlerAST :: AST.Handler
    , handlerRecipients :: [Unique]
    , handlerProc :: forall s. Def ('[ConstRef s a] :-> ())
    , handlerCode :: MonitorCode
    }
  newtype TowerBackendMonitor PosixBackend = PosixMonitor
    (Dependencies -> Map.Map Unique Module -> (Map.Map Unique Module, [Module]))
  newtype TowerBackendOutput PosixBackend = PosixOutput [TowerBackendMonitor PosixBackend]

  callbackImpl _ ast f = PosixCallback $
    let p = proc (showUnique ast) $ \ r -> body $ noReturn $ f r
    in (p, incl p)

  emitterImpl _ _ [] = (Emitter $ const $ return (), PosixEmitter Nothing)
  emitterImpl _ ast handlers =
    ( Emitter $ call_ $ eproc messageAt
    , PosixEmitter $ Just EmitterCode
        { emitterInit = store (addrOf messageCount) 0
        , emitterDeliver = do
            mc <- deref (addrOf messageCount)
            forM_ (zip messages [0..]) $ \ (m, index) ->
              when (fromInteger index <? mc) $
                forM_ handlers $ \ h ->
                  call_ (handlerProc h) (constRef (addrOf m))
        , emitterRecipients = map (AST.handler_name . handlerAST) handlers
        , emitterCode = do
            incl $ eproc messageAt
            private $ do
              mapM_ defMemArea messages
              defMemArea messageCount
        }
    )
    where
    max_messages = AST.emitter_bound ast - 1
    messageCount :: MemArea (Stored Uint32)
    messageCount = area (named "message_count") Nothing

    messages = [ area (named ("message_" ++ show d)) Nothing
               | d <- [0..max_messages] ]

    messageAt idx = foldl aux dflt (zip messages [0..])
      where
      dflt = addrOf (messages !! 0) -- Should be impossible.
      aux basecase (msg, midx) =
        (fromInteger midx ==? idx) ? (addrOf msg, basecase)

    eproc :: IvoryArea b => (Uint32 -> Ref s b) -> Def ('[ConstRef s' b] :-> ())
    eproc mAt = voidProc (named "emit") $ \ msg -> body $ do
      mc <- deref (addrOf messageCount)
      when (mc <=? fromInteger max_messages) $ do
        store (addrOf messageCount) (mc + 1)
        storedmsg <- assign (mAt mc)
        refCopy storedmsg msg

    named suffix = showUnique (AST.emitter_name ast) ++ "_" ++ suffix

  handlerImpl _ ast emitters callbacks = h
    where
    ems = [ e | PosixEmitter (Just e) <- emitters ]
    h = PosixHandler
      { handlerAST = ast
      , handlerRecipients = concatMap emitterRecipients ems
      , handlerProc = voidProc ("handler_run_" ++ AST.handlerName ast) $ \ msg -> body $ do
          mapM_ emitterInit ems
          forM_ callbacks $ \ (PosixCallback (cb, _)) -> call_ cb msg
          mapM_ emitterDeliver ems
      , handlerCode = MonitorCode
          { monitorUserCode = forM_ callbacks $ \ (PosixCallback (_, d)) -> d
          , monitorGenCode = do
              mapM_ emitterCode ems
              incl $ handlerProc h
          }
      }

  monitorImpl _ ast handlers moddef = PosixMonitor fromModuleMap
    where
    monitorRecipients = concat [ handlerRecipients h | SomeHandler h <- handlers ]
    mods = mconcat [ handlerCode h | SomeHandler h <- handlers ]
    -- FIXME: limit to handlers that take an ITime so the map has a consistent type.
    -- this allows everything except SyncChan, which we wouldn't have used with threadLoopRunHandlers anyway.
    -- chanMap = Map.fromListWith (++)
    --   [ (AST.handler_chan $ handlerAST h, [h]) | SomeHandler h <- handlers ]

    fromModuleMap deps moduleMap = (thisModuleMap, [userMod, genMod])
      where
      thisModuleMap = Map.fromList
        [ (AST.handler_name $ handlerAST h, genMod) | SomeHandler h <- handlers ]
      otherModuleMap = moduleMap Map.\\ thisModuleMap
      genMod = package ("tower_gen_" ++ AST.monitorName ast) $ do
        depend userMod
        mapM_ depend $ dependencies_depends deps
        mapM_ depend $ mapMaybe (flip Map.lookup otherModuleMap) monitorRecipients
        monitorGenCode mods
      userMod = package ("tower_user_" ++ AST.monitorName ast) $ do
        depend genMod
        mapM_ depend $ dependencies_depends deps
        private moddef
        monitorUserCode mods

  towerImpl _ _ monitors = PosixOutput monitors

compileTowerPosix :: (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerPosix makeEnv twr = do
  (copts, topts) <- towerGetOpts
  env <- makeEnv topts
  let (_ast, PosixOutput monitors, deps, _sigs) = runTower PosixBackend twr env

  let moduleMap = Map.unions moduleMaps
      (moduleMaps, monitorModules) = unzip
        [ m deps moduleMap | PosixMonitor m <- monitors ]

  -- TODO: get handler graph roots
  {-
  let initHandlers :: [Def ('[ConstRef s (Stored ITime)] :-> ())]
      initHandlers = []
  let periodicHandlers :: [(AST.Period, [Def ('[ConstRef s (Stored ITime)] :-> ())])]
      periodicHandlers = []

  let periodicCallbacks = map (second makeCallback) periodicHandlers
        where
        makeCallback :: [Def ('[ConstRef s (Stored ITime)] :-> ())] -> Def ('[Ref s2 ('Struct "ev_loop"), Ref s3 (Struct "ev_timer"), Uint32] ':-> ())
        makeCallback hs = proc "callback" $ \ main_loop _watcher _revents -> body $ do
          now <- call ev_now main_loop
          t_ptr <- local $ ival $ fromIMicroseconds (castDefault $ now * 1e6 :: Sint64)
          mapM_ (flip call_ $ constRef t_ptr) hs
  -}

  let entryProc = proc "main" $ body $ do
        main_loop <- call ev_default_loop 0

        {-
        forM_ periodicCallbacks $ \ (p, cb) -> do
          watcher <- local izero
          let dtime t = (fromInteger $ toMicroseconds t) / 1.0e6
          call_ ev_timer_init watcher (procPtr cb) (dtime $ AST.period_phase p) (dtime $ AST.period_dt p)
          call_ ev_timer_start main_loop watcher

        now <- call ev_now main_loop
        t_ptr <- local $ ival $ fromIMicroseconds (castDefault $ now * 1e6 :: Sint64)
        mapM_ (flip call_ $ constRef t_ptr) initHandlers
        -}

        call_ ev_run main_loop 0
        ret (0 :: Sint32)

  let initModule = package "tower_init" $ do
        mapM_ depend $ Map.elems moduleMap
        uses_libev
        -- private $ mapM_ (incl . snd) periodicCallbacks
        incl entryProc

  let mods = initModule : concat monitorModules ++ dependencies_modules deps
  let artifacts = makefile mods : dependencies_artifacts deps

  runCompiler mods artifacts copts

makefile :: [Module] -> Located Artifact
makefile modules = Root $ artifactString "Makefile" $ unlines
  [ "CC = gcc"
  , "CFLAGS = -Wall -std=c99 -Og -g -I. -DIVORY_TEST"
  , "LDLIBS = -lm -lev"
  , "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ]
  , moduleName (head modules) ++ ": $(OBJS)"
  , "clean:"
  , "\t-rm -f $(OBJS)"
  , ".PHONY: clean"
  ]
