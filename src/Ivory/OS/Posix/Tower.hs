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
import Ivory.Tower.Types.SignalCode

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
  , emitterRecipients :: [String]
  , emitterCode :: ModuleDef
  }

data PosixBackend = PosixBackend

instance TowerBackend PosixBackend where
  newtype TowerBackendCallback PosixBackend a = PosixCallback (forall s. (Def ('[ConstRef s a] :-> ()), ModuleDef))
  newtype TowerBackendEmitter PosixBackend = PosixEmitter (Maybe EmitterCode)
  data TowerBackendHandler PosixBackend a = PosixHandler
    { handlerChan :: AST.Chan
    , handlerRecipients :: [String]
    , handlerProcName :: String
    , handlerProc :: forall s. Def ('[ConstRef s a] :-> ())
    , handlerCode :: MonitorCode
    }
  newtype TowerBackendMonitor PosixBackend = PosixMonitor
    (Dependencies -> Map.Map String Module -> (Map.Map String Module, Map.Map AST.Chan [String], [Module]))
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
        , emitterRecipients = map handlerProcName handlers
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
      { handlerChan = AST.handler_chan ast
      , handlerRecipients = concatMap emitterRecipients ems
      , handlerProcName = "handler_run_" ++ AST.handlerName ast
      , handlerProc = voidProc (handlerProcName h) $ \ msg -> body $ do
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
    chanMap = Map.fromListWith (++)
      [ (handlerChan h, [handlerProcName h]) | SomeHandler h <- handlers ]

    fromModuleMap deps moduleMap = (thisModuleMap, chanMap, [userMod, genMod])
      where
      thisModuleMap = Map.fromList
        [ (handlerProcName h, genMod) | SomeHandler h <- handlers ]
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
  let (_ast, PosixOutput monitors, deps, sigs) = runTower PosixBackend twr env

  let moduleMap = Map.unions moduleMaps
      (moduleMaps, chanMaps, monitorModules) = unzip3
        [ m deps moduleMap | PosixMonitor m <- monitors ]

  let chanMap = Map.unionsWith (++) chanMaps

  let itimeStub :: String -> Def ('[ConstRef s (Stored ITime)] :-> ())
      itimeStub name = proc name $ const $ body $ return ()

  let callHandlers main_loop names = do
        now <- call ev_now main_loop
        t_ptr <- fmap constRef $ local $ ival $
          fromIMicroseconds (castDefault $ now * 1e6 :: Sint64)
        forM_ names $ \ name -> call_ (itimeStub name) t_ptr

  let signalChannels = Map.fromAscList
        [ (AST.signal_name s, xs)
        | (AST.ChanSignal s, xs) <- Map.toAscList chanMap
        ]

  let signalHandlers = Map.elems $ Map.intersectionWith (,) signalChannels $ signalcode_signals sigs

  let periodicHandlers = do
        (AST.ChanPeriod p, names) <- Map.toList chanMap

        let cbname = "elapsed_" ++ prettyTime (AST.period_dt p) ++
              if toMicroseconds (AST.period_phase p) == 0
              then ""
              else "_phase_" ++ prettyTime (AST.period_phase p)

        return (p, proc cbname $ \ l _ _ -> body $ callHandlers l names)

  let entryProc = proc "main" $ body $ do
        main_loop <- call ev_default_loop 0

        forM_ periodicHandlers $ \ (p, cb) -> do
          watcher <- local izero
          let dtime t = (fromInteger $ toMicroseconds t) / 1.0e6
          call_ ev_timer_init watcher (procPtr cb)
            (dtime $ AST.period_phase p)
            (dtime $ AST.period_dt p)
          call_ ev_timer_start main_loop watcher
          comment "Don't let this periodic thread keep the main loop running."
          call_ ev_unref main_loop

        signalcode_init sigs
        maybe (return ()) (callHandlers main_loop) $ Map.lookup (AST.ChanInit AST.Init) chanMap

        call_ ev_run main_loop 0
        ret (0 :: Sint32)

  let initModule = package "tower_init" $ do
        mapM_ depend $ Map.elems moduleMap
        uses_libev
        incl entryProc
        private $ do
          forM_ signalHandlers $ \ (handlers, code) -> unGeneratedSignal code $ do
            main_loop <- call ev_default_loop 0
            callHandlers main_loop handlers
          mapM_ (incl . snd) periodicHandlers

  let mods = initModule : concat monitorModules ++ dependencies_modules deps
  let artifacts = makefile mods : dependencies_artifacts deps

  runCompiler mods artifacts copts

makefile :: [Module] -> Located Artifact
makefile modules = Root $ artifactString "Makefile" $ unlines
  [ "CC = gcc"
  , "CFLAGS = -Wall -std=gnu99 -O3 -g -I. -DIVORY_TEST"
  , "LDLIBS = -lm -lev"
  , "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ]
  , moduleName (head modules) ++ ": $(OBJS)"
  , "clean:"
  , "\t-rm -f $(OBJS)"
  , ".PHONY: clean"
  ]
