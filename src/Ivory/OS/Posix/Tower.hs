{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Posix.Tower (
  posix
) where

import Data.List
import qualified Data.Map as Map
import Ivory.Artifact
import Ivory.Language
import Ivory.OS.Posix.Tower.EventLoop
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Codegen.Handler
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Time
import qualified Ivory.Tower.Types.TowerPlatform as T

posix :: e -> T.TowerPlatform e
posix e = T.TowerPlatform
  { T.threadModules = threadModules
  , T.monitorModules = monitorModules
  , T.systemModules = systemModules
  , T.systemArtifacts = systemArtifacts
  , T.platformEnv = e
  }

gcDeps :: GeneratedCode -> ModuleDef
gcDeps = mapM_ depend . generatedcode_depends

threadMonitorDeps :: AST.Tower -> AST.Thread -> (AST.Monitor -> String) -> ModuleDef
threadMonitorDeps twr t mname = sequence_
  [ depend $ package (mname m) $ return ()
  | (m,_) <- AST.threadHandlers (AST.messageGraph twr) t ]

threadModules :: GeneratedCode -> AST.Tower-> [Module]
threadModules gc twr = concatMap pertask $ Map.elems $ generatedcode_threads gc
  where
  pertask tc = [threadUserModule, threadGenModule]
    where
    threadUserModule = package (AST.threadUserCodeModName $ threadcode_thread tc) $ do
      gcDeps gc
      depend threadGenModule
      threadMonitorDeps twr (threadcode_thread tc) monitorStateModName
      threadcode_user tc
    threadGenModule = package (AST.threadGenCodeModName $ threadcode_thread tc) $ do
      gcDeps gc
      depend threadUserModule
      threadModdef gc twr $ threadcode_thread tc
      threadcode_gen tc

threadLoopRunHandlers :: (GetAlloc eff ~ Scope s)
                      => AST.Tower -> AST.Thread
                      -> IDouble -> Ivory eff ()
threadLoopRunHandlers twr thr t = do
  t_ptr <- local $ ival $ fromIMicroseconds (castDefault $ t * 1e6 :: Sint64)
  sequence_
    [ call_ (hproc h) (constRef t_ptr)
    | (_m,h) <- AST.towerChanHandlers twr (AST.threadChan thr) ]
  where
  hproc :: AST.Handler -> Def ('[ConstRef s (Stored ITime)] :-> ())
  hproc h = proc (handlerProcName h thr) (const (body (return ())))

threadModdef :: GeneratedCode -> AST.Tower -> AST.Thread -> ModuleDef
threadModdef _gc twr thd@(AST.PeriodThread p) = do
  uses_libev
  let cb :: Def ('[Ref s2 (Struct "ev_loop"), Ref s3 (Struct "ev_timer"), Uint32] :-> ())
      cb = proc "callback" $ \ loop _watcher _revents -> body $ do
        now <- call ev_now loop
        threadLoopRunHandlers twr thd now
  private $ incl cb
  incl $ proc (AST.threadLoopProcName thd) $ body $ do
    loop <- call ev_default_loop 0
    watcher <- local izero
    let dtime t = (fromInteger $ toMicroseconds t) / 1.0e6
    call_ ev_timer_init watcher (procPtr cb) (dtime $ AST.period_phase p) (dtime $ AST.period_dt p)
    call_ ev_timer_start loop watcher
    retVoid
threadModdef gc twr thd@(AST.SignalThread s) = do
  uses_libev
  let cb :: Def ('[] :-> ())
      cb = proc (AST.threadLoopProcName thd) $ body $ do
        loop <- call ev_default_loop 0
        now <- call ev_now loop
        threadLoopRunHandlers twr thd now
  incl cb
  unGeneratedSignal (generatedCodeForSignal s gc) $ call_ cb
threadModdef gc twr thd@(AST.InitThread _) = do
  incl $ proc (AST.threadLoopProcName thd) $ body $ do
    generatedcode_init gc
    threadLoopRunHandlers twr thd 0
    retVoid

monitorStateModName :: AST.Monitor -> String
monitorStateModName mon = "tower_state_monitor_" ++ AST.monitorName mon

monitorModules :: GeneratedCode -> AST.Tower -> [Module]
monitorModules gc _twr = map permon $ Map.toList $ generatedcode_monitors gc
  where
  permon (ast, code) = package (monitorStateModName ast) $ do
    gcDeps gc
    monitorcode_moddef code

systemModules :: AST.Tower -> [Module]
systemModules twr = [initModule]
  where
  initModule = package "tower_init" $ do
    mapM_ (depend . stubPkg . AST.threadGenCodeModName) $ AST.towerThreads twr
    uses_libev
    incl entryProc

  stubPkg name = package name $ return ()
  stub name = proc name $ body retVoid

  entryProc = proc "tower_entry" $ body $ do
    mapM_ (call_ . stub . AST.threadLoopProcName) $ AST.towerThreads twr
    loop <- call ev_default_loop 0
    call_ ev_run loop 0
    retVoid

systemArtifacts :: AST.Tower -> [Module] -> [Artifact]
systemArtifacts _twr modules = [makefile]
  where
  makefile = artifactString "Makefile" $ unlines
    [ "CC = gcc"
    , "CFLAGS = -Wall -std=c99 -Og -g -I. -DIVORY_TEST"
    , "LDLIBS = -lm"
    , "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ]
    , "main: $(OBJS)"
    , "clean:"
    , "\t-rm -f $(OBJS)"
    , ".PHONY: clean"
    ]
