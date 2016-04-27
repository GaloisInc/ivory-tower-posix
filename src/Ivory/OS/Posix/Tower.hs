{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.OS.Posix.Tower (
  compileTowerPosix,
  compileTowerPosixWithOpts,
  parseTowerPosix,
  parseTowerPosixWithOpts
) where

import Prelude ()
import Prelude.Compat

import Control.Monad (forM_)
import Data.List (intercalate)
import MonadLib (put)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe
import Ivory.Artifact
import Ivory.Compile.C.CmdlineFrontend (runCompiler)
import Ivory.Language
import Ivory.OS.Posix.Tower.EventLoop
import Ivory.OS.Posix.Tower.Pthread
import Ivory.OS.Posix.Tower.Monitor
import Ivory.Stdlib.Control
import Ivory.Tower
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Options
import Ivory.Tower.Types.Backend
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.SignalCode

import qualified Ivory.Language.Module as Mod
import qualified Ivory.Language.Monad as Mon
import qualified Ivory.Language.Syntax.AST as IAST
import qualified Ivory.Language.Syntax.Names as IAST
import qualified Ivory.Language.Syntax.Type as TIAST
import Ivory.Language.MemArea (primAddrOf)
import Ivory.Language.Proc (initialClosure, genVar)
import Ivory.Language.MemArea (makeArea)
import Ivory.Language.Ref
import Ivory.Language.Ptr
import qualified Ivory.Stdlib as I


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

instance TowerBackendTypes PosixBackend where 
  newtype TowerBackendCallback PosixBackend a = PosixCallback (forall s. (Def ('[ConstRef s a] ':-> ()), ModuleDef))
  newtype TowerBackendEmitter PosixBackend = PosixEmitter (Maybe EmitterCode)
  data TowerBackendHandler PosixBackend a = PosixHandler
    { handlerChan :: AST.Chan
    , handlerRecipients :: [String]
    , handlerProcName :: String
    , handlerProc :: forall s. Def ('[ConstRef s a] ':-> ())
    , handlerCode :: MonitorCode
    }
  newtype TowerBackendMonitor PosixBackend = PosixMonitor
    (Dependencies -> Map.Map String Module -> (Map.Map String Module, Map.Map AST.Chan [String], [Module]))
  newtype TowerBackendOutput PosixBackend = PosixOutput [TowerBackendMonitor PosixBackend]

instance TowerBackend PosixBackend where
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
    messageCount :: MemArea ('Stored Uint32)
    messageCount = area (named "message_count") Nothing

    messages = [ area (named ("message_" ++ show d)) Nothing
               | d <- [0..max_messages] ]

    messageAt idx = foldl aux dflt (zip messages [0..])
      where
      dflt = addrOf (messages !! 0) -- Should be impossible.
      aux basecase (msg, midx) =
        (fromInteger midx ==? idx) ? (addrOf msg, basecase)

    eproc :: IvoryArea b => (Uint32 -> Ref s b) -> Def ('[ConstRef s' b] ':-> ())
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


--------------------
-- TOP DOWN ANALYSIS
--------------------

callbackImplTD :: Unique -> IAST.Proc -> (IAST.Proc, ModuleDef)
callbackImplTD ast f = 
  let p = f {IAST.procSym = (showUnique ast)} in
  let inclp = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Public p }) in
  (p, inclp)

emitterImplTD :: AST.Tower -> AST.Monitor -> AST.Emitter -> Maybe EmitterCode
emitterImplTD tow mon ast =
  let handlers = map (handlerImplTD tow mon) $ subscribedHandlers in
  emitterCodeTD ast [ h | (h, _, _, _) <- handlers ]

  where
    subscribedHandlers = filter (\x -> isListening $ AST.handler_chan x) allHandlers
    -- dont know why it works

    allHandlers = concat $ map (AST.monitor_handlers) (AST.tower_monitors tow)

    isListening (AST.ChanSync sc) = sc == (AST.emitter_chan ast)
    isListening _ = False

emitterCodeTD :: AST.Emitter 
              -> [IAST.Proc]
              -> Maybe EmitterCode
emitterCodeTD _ast [] = Nothing
emitterCodeTD ast sinks = Just $ EmitterCode
  { emitterInit = store (addrOf messageCount) 0
  , emitterDeliver = do
      mc <- deref (addrOf messageCount)
      forM_ (zip messages [0..]) $ \ (m, index) ->
        I.when (fromInteger index <? mc) $
          forM_ sinks $ \ p ->
            let sym = (IAST.NameSym (IAST.procSym p)) in
            let param = TIAST.Typed (emitter_type) $ IAST.ExpAddrOfGlobal (IAST.areaSym m) in
            Mon.emit (IAST.Call (IAST.procRetTy p) Nothing sym [param])

  , emitterRecipients = map IAST.procSym sinks
  , emitterCode = do
      incleproc
      mapM_ (\a -> put (mempty { IAST.modAreas = Mod.visAcc Mod.Private a })) messages
      private $ defMemArea messageCount
  }
  where
  emitter_type :: TIAST.Type
  emitter_type = TIAST.tType $ head $ IAST.procArgs $ head sinks

  emitter_type_unconst :: TIAST.Type
  emitter_type_unconst = 
    let (TIAST.TyConstRef tt) = emitter_type in
    case tt of 
      TIAST.TyArr{} -> tt
      _             -> TIAST.TyRef tt 

  emitter_type_unconst_withref :: TIAST.Type
  emitter_type_unconst_withref = 
    let (TIAST.TyConstRef tt) = emitter_type in
    TIAST.TyRef tt 
  emitter_type_unconst_unref :: TIAST.Type
  emitter_type_unconst_unref = 
    let (TIAST.TyConstRef tt) = emitter_type in tt 

  max_messages = AST.emitter_bound ast - 1
  messageCount :: MemArea ('Stored Uint32)
  messageCount = area (named "message_count") Nothing
  
  messages :: [IAST.Area]
  messages = [makeArea (named $ "message_" ++ show d) False emitter_type_unconst_unref IAST.zeroInit | d <- [(0::Integer)..max_messages] ]

  messageAt mc = foldl aux dflt (zip messages [(0::Integer)..])
    where
    dflt = IAST.ExpAddrOfGlobal $ IAST.areaSym (messages !! 0) -- Should be impossible.
    aux basecase (msg, midx) = 
      IAST.ExpOp IAST.ExpCond 
        [booleanCond,IAST.ExpAddrOfGlobal $ IAST.areaSym $ msg,basecase]
      where
        booleanCond = IAST.ExpOp (IAST.ExpEq (TIAST.TyWord TIAST.Word32)) [fromIntegral midx, IAST.ExpVar mc]
  incleproc = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Public eproc })

  eproc :: IAST.Proc
  eproc = 
    IAST.Proc { IAST.procSym      = (named "emit")
              , IAST.procRetTy    = TIAST.TyVoid
              , IAST.procArgs     = [TIAST.Typed emitter_type var]
              , IAST.procBody     = eprocblock
              , IAST.procRequires = []
              , IAST.procEnsures  = []
              }
    where 
    (var,_) = genVar initialClosure
    eprocblock = 
      [IAST.Deref (TIAST.TyWord TIAST.Word32) mc (primAddrOf messageCount),
      IAST.IfTE (IAST.ExpOp (IAST.ExpLt True $ TIAST.TyWord TIAST.Word32) [IAST.ExpVar mc, IAST.ExpLit $ IAST.LitInteger $ fromInteger max_messages]) 
        [IAST.Store (TIAST.TyWord TIAST.Word32) (primAddrOf messageCount) (IAST.ExpOp IAST.ExpAdd [IAST.ExpVar mc, IAST.ExpLit $ IAST.LitInteger $ (1::Integer)]),
        IAST.Assign (emitter_type_unconst_withref) r (messageAt mc),
        IAST.RefCopy (emitter_type_unconst) (IAST.ExpVar r) (IAST.ExpVar var)] 
        [] --nothing else
      ]
      where
      mc=IAST.VarName ("deref"++ show (0::Integer))
      r=IAST.VarName ("let"++ show (1::Integer))

  named suffix = showUnique (AST.emitter_name ast) ++ "_" ++ suffix


-- returns : the IAST.Proc, the initial handler, the moncode, list of handlers subscribed to the emitters.
handlerImplTD :: AST.Tower -> AST.Monitor -> AST.Handler -> (IAST.Proc, AST.Handler, MonitorCode, [String])
handlerImplTD tow m ast = (h,ast,moncode,concatMap emitterRecipients ems)
    where
    emitters::[Maybe EmitterCode]
    emitters = map (emitterImplTD tow m) $ AST.handler_emitters ast
    callbacks::(NE.NonEmpty (IAST.Proc, ModuleDef)) 
    callbacks = NE.map (\(x,y) -> callbackImplTD x y) (NE.zip (AST.handler_callbacks ast) (AST.handler_callbacksAST ast))
    ems = [e | Just e <- emitters]
    (cbs, cbdefs) = NE.unzip $ callbacks
    h = IAST.Proc { IAST.procSym      = "handler_run_" ++ AST.handlerName ast
                  , IAST.procRetTy    = TIAST.TyVoid
                  , IAST.procArgs     = [TIAST.Typed (TIAST.tType $ head $ IAST.procArgs $ NE.head cbs) var]
                  , IAST.procBody     = blocBody
                  , IAST.procRequires = blocReq
                  , IAST.procEnsures  = blocEns
                  }
      where
        (var,_) = genVar initialClosure -- initial closure is ok until we have one argument per function
        ((_,emitterscodeinit),n1) = Mon.primRunIvory 0 $ mapM_ emitterInit ems
        ((_,monitorlockproc),n2) = Mon.primRunIvory n1 $ monitorLockProc m ast
        ((_,monitorunlockproc),n3) = Mon.primRunIvory n2 $ monitorUnlockProc m ast
        ((_,emittersdeliver),_n4) = Mon.primRunIvory n3 $ mapM_ emitterDeliver ems
        blocReq = Mon.blockRequires emitterscodeinit ++
          (Mon.blockRequires monitorlockproc) ++
          (Mon.blockRequires monitorunlockproc) ++
          (Mon.blockRequires emittersdeliver)
        blocEns = Mon.blockEnsures emitterscodeinit ++
          (Mon.blockEnsures monitorlockproc) ++
          (Mon.blockEnsures monitorunlockproc) ++
          (Mon.blockEnsures emittersdeliver)
        blocBody = 
          [IAST.Comment $ IAST.UserComment "init emitters"] ++ 
          (Mon.blockStmts $ emitterscodeinit) ++ 
          [IAST.Comment $ IAST.UserComment "take monitor lock(s)"] ++
          (Mon.blockStmts $ monitorlockproc) ++       
          [IAST.Comment $ IAST.UserComment "run callbacks"] ++
          (NE.toList $ NE.map (\ cb -> (IAST.Call (IAST.procRetTy cb) Nothing (IAST.NameSym $ IAST.procSym cb) [TIAST.Typed (TIAST.tType $ head $ IAST.procArgs $ NE.head cbs) $ IAST.ExpVar var])) cbs )++
          [IAST.Comment $ IAST.UserComment "release monitor lock(s)"] ++
          (Mon.blockStmts $ monitorunlockproc) ++ 
          [IAST.Comment $ IAST.UserComment "deliver emitters"] ++
          (Mon.blockStmts $ emittersdeliver)
    moncode = MonitorCode
      { monitorUserCode = sequence_ cbdefs
      , monitorGenCode = do
          let inclh = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Public h })
          mapM_ emitterCode ems
          inclh
      }

monitorImplTD :: AST.Tower -> AST.Monitor -> TowerBackendMonitor PosixBackend
monitorImplTD tow ast = 
  PosixMonitor fromModuleMap
    where
    (moddef::ModuleDef) = put $ AST.monitor_moduledef ast 
    handlers = map (handlerImplTD tow ast) $ AST.monitor_handlers ast
    monitorRecipients = concat [ recipient | (_,_,_,recipient) <- handlers ]
    mods = mconcat [ code | (_,_,code,_) <- handlers ]
    chanMap = Map.fromListWith (++)
      [ (AST.handler_chan hast, ["handler_run_" ++ AST.handlerName hast]) | (_,hast,_,_) <- handlers ]

    fromModuleMap deps moduleMap = (thisModuleMap, chanMap, [userMod, genMod])
      where
      thisModuleMap = Map.fromList
        [ ("handler_run_" ++ (AST.handlerName hast), genMod) | (_,hast,_,_) <- handlers ]
      otherModuleMap = moduleMap Map.\\ thisModuleMap
      genMod = package (monitorGenModName ast) $ do
        depend userMod
        mapM_ depend $ dependencies_depends deps
        mapM_ depend $ mapMaybe (flip Map.lookup otherModuleMap) monitorRecipients
        monitorGenCode mods
        uses_libpthread
        monitorGenPackage ast
      userMod = package (monitorUserModName ast) $ do
        depend genMod
        mapM_ depend $ dependencies_depends deps
        private moddef
        monitorUserCode mods


--------



compileTowerPosix :: (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerPosix makeEnv twr = compileTowerPosixWithOpts makeEnv twr []

compileTowerPosixWithOpts :: (TOpts -> IO e) -> Tower e () -> [AST.Tower -> IO AST.Tower] -> IO ()
compileTowerPosixWithOpts makeEnv twr optslist = do
  (copts, topts) <- towerGetOpts
  env <- makeEnv topts
  (ast, _tempMonitors, deps, sigs) <- runTower PosixBackend twr env optslist
--  let PosixOutput monitors = towerImpl PosixBackend ast tempMonitors 
  let PosixOutput monitors = towerImpl PosixBackend ast (map (monitorImplTD ast) $ AST.tower_monitors ast)
  let moduleMap = Map.unions moduleMaps
      (moduleMaps, chanMaps, monitorModules) = unzip3
        [ m deps moduleMap | PosixMonitor m <- monitors ]

  let chanMap = Map.unionsWith (++) chanMaps

  let itimeStub :: String -> Def ('[Ref s ('Stored Sint64)] ':-> ())
      itimeStub name = proc name $ \ _ -> body $ retVoid

--  let periods = sort $ map fst $ Map.toList chanMap

  let callHandlers main_loop names = do
--        let (Just prio) = elemIndex (AST.ChanPeriod per) periods
        now <- call ev_now main_loop
        t_ptr <- fmap constRef $ local $ ival $
          fromIMicroseconds (castDefault $ now * 1e6 :: Sint64)

        comment "creating attribute"
        (pthreadattribute::Ref ('Stack s1) ('Stored PthreadAttr)) <- local (inewtype)
        attrinitReturn <- call pthread_attr_init pthreadattribute
        assert (attrinitReturn ==? 0)

        comment "setting the policy scheduling to Round Robin (SCHED_RR)"
        attrsetschpolReturn <- call pthread_attr_setschedpolicy pthreadattribute sched_RR
        assert (attrsetschpolReturn ==? 0)

        comment "allocating pthread_t"
        pthreads <- mapM (\_ -> local (inewtype)) names

        comment "creating threads"
        forM_ (zip pthreads names) $ \ (thr,name) -> do
          itimeStubvoidptr <- call pthread_cast_voidfunc_fromint (procPtr $ itimeStub name)
          pthreadCreateReturn <- call pthread_create thr pthreadattribute 
                                      (itimeStubvoidptr) (Ref (getConstRef t_ptr))
          assert (pthreadCreateReturn ==? 0)

        comment "joining threads"
        forM_ (pthreads) $ \ (thr) -> do
          curthr <- deref thr
          pthreadJoinReturn <- call pthread_join curthr (ptrToRef nullPtr)
          assert (pthreadJoinReturn ==? 0)

        --(forM_ names $ \ name -> call_ (itimeStub name) t_ptr)
        comment "destroying attribute"
        attrdestroyReturn <- call pthread_attr_destroy pthreadattribute
        assert (attrdestroyReturn ==? 0)

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
        comment "Initializing monitors"
        forM_ (AST.tower_monitors ast) $ \ mon -> do
          call_ (monitorInitProc mon)

        comment "Initializing periods"
        forM_ periodicHandlers $ \ (p, cb) -> do
          watcher <- local izero
          let dtime t = (fromInteger $ toMicroseconds t) / 1.0e6
          call_ ev_periodic_init watcher (procPtr cb)
            (dtime $ AST.period_phase p)
            (dtime $ AST.period_dt p)
            (null_Ptr_Resch)
          call_ ev_periodic_start main_loop watcher


        comment "Initializing signals"
        signalcode_init sigs
        maybe (return ()) (callHandlers main_loop) $ Map.lookup (AST.ChanInit AST.Init) chanMap

        comment "Starting the main loop"
        call_ ev_run main_loop 0
        ret (0 :: Sint32)

  let initModule = package "tower_init" $ do
        mapM_ depend $ Map.elems moduleMap
        uses_libev
        uses_libpthread
        incl entryProc
        private $ do
          forM_ signalHandlers $ \ (handlers, code) -> unGeneratedSignal code $ do
            main_loop <- call ev_default_loop 0
            callHandlers main_loop handlers
          mapM_ (incl . snd) periodicHandlers

  let mods = initModule : concat monitorModules ++ dependencies_modules deps
  let artifacts = makefile mods : dependencies_artifacts deps

  runCompiler mods artifacts copts

parseTowerPosix :: (TOpts -> IO e) -> Tower e () -> IO AST.Tower
parseTowerPosix makeEnv twr = parseTowerPosixWithOpts makeEnv twr []

parseTowerPosixWithOpts :: (TOpts -> IO e) -> Tower e () -> [AST.Tower -> IO AST.Tower] -> IO AST.Tower
parseTowerPosixWithOpts makeEnv twr optslist = do
  (_, topts) <- towerGetOpts
  env <- makeEnv topts
  (ast, _, _, _) <- runTower PosixBackend twr env optslist
  return ast


makefile :: [Module] -> Located Artifact
makefile modules = Root $ artifactString "Makefile" $ unlines
  [ "CC = gcc"
  , "CFLAGS = -Wall -std=gnu99 -O2 -g -I. -DIVORY_TEST"
  , "LDLIBS = -lm -lev -pthread -lwiringPi"
  , "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ]
  , moduleName (head modules) ++ ": $(OBJS)"
  , "clean:"
  , "\t-rm -f $(OBJS)"
  , ".PHONY: clean"
  ]
