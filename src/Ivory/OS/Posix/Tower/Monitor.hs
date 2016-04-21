{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.Posix.Tower.Monitor
  ( monitorUserModName,
    monitorGenModName,
    monitorGenPackage,
    monitorInitProc,
    monitorUnlockProc,
    monitorLockProc
  ) where

import qualified Ivory.Tower.AST as AST

import Ivory.Language
import Data.Foldable (traverse_)
import Control.Monad (forM_)
import Data.List
import Ivory.Tower.Types.Opts
import Ivory.OS.Posix.Tower.Pthread

monitorUserModName :: AST.Monitor -> String
monitorUserModName mon = "tower_user_" ++ AST.monitorName mon

monitorGenModName :: AST.Monitor -> String
monitorGenModName mon = "tower_gen_" ++ AST.monitorName mon

monitorGenPackage :: AST.Monitor -> Ivory.Language.ModuleDef
monitorGenPackage mon = do
  private (
    if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      -- create one lock per cluster of ressources as optimized by the AST transformer
      let (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon in 
        traverse_ defMemArea $ map (monitorLockAreaWithCoarsening mon) [1..(length $ globmon)]
    else
      -- create only one lock for all ressources
      defMemArea (monitorLockArea mon)
    )
  incl (monitorInitProc mon)



monitorLockName :: AST.Monitor -> String
monitorLockName mon = "lock_"  ++ AST.monitorName mon

monitorLockArea :: AST.Monitor -> MemArea ('Stored PthreadMutex)
monitorLockArea mon = area (monitorLockName mon) (Just inewtype)

monitorLock :: AST.Monitor -> Ref 'Global ('Stored PthreadMutex)
monitorLock mon = addrOf (monitorLockArea mon)


monitorLockAreaWithCoarsening :: AST.Monitor -> Int -> MemArea ('Stored PthreadMutex)
monitorLockAreaWithCoarsening mon lockId = area ("lock" ++ (show lockId) ++ "_"  ++ AST.monitorName mon) (Just inewtype)

monitorLockWithCoarsening :: AST.Monitor -> Int -> Ref 'Global ('Stored PthreadMutex)
monitorLockWithCoarsening mon lockId = addrOf (monitorLockAreaWithCoarsening mon lockId)

monitorInitProc :: AST.Monitor -> Def('[]':->())
monitorInitProc mon = 
  if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      monitorInitProcLockCoarsening mon
    else
      monitorInitProcRaw mon

monitorInitProcRaw :: AST.Monitor -> Def('[]':->())
monitorInitProcRaw mon = proc n $ body $ do
  pthreadattr <- local inewtype
  returnAttrInit <- call pthread_mutexattr_init pthreadattr
  assert (returnAttrInit ==? 0)
  returnInit <- call pthread_mutex_init (monitorLock mon) pthreadattr
  assert (returnInit ==? 0)
  where
  n = "monitor_init_" ++ AST.monitorName mon

monitorInitProcLockCoarsening :: AST.Monitor -> Def('[]':->())
monitorInitProcLockCoarsening mon = proc n $ body $ do
  if (length globmon /= 0)
    then do
      pthreadattr <- local inewtype
      returnAttrInit <- call pthread_mutexattr_init pthreadattr
      assert (returnAttrInit ==? 0)
      forM_ (map (monitorLockWithCoarsening mon) [1..(length $ globmon)]) $ \x -> do
        returnInit <- call pthread_mutex_init x pthreadattr
        assert (returnInit ==? 0)
    else
      return ()
  where
  (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
  n = "monitor_init_" ++ AST.monitorName mon


monitorUnlockProc :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorUnlockProc mon h =
  if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      monitorUnlockProcLockCoarsening mon h
    else
      monitorUnlockProcRaw mon


monitorUnlockProcRaw :: AST.Monitor -> Ivory eff ()
monitorUnlockProcRaw mon = do
  retValueLock <- call pthread_mutex_unlock (monitorLock mon)
  assert (retValueLock ==? 0)

monitorUnlockProcLockCoarsening :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorUnlockProcLockCoarsening mon h = 
  forM_ (map (monitorLockWithCoarsening mon) locksToTake) $ \lock -> do
    retValueLock <- call pthread_mutex_unlock lock
    assert (retValueLock ==? 0)
  where
    (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
    (Just (LockCoarsening (OptHandler globhan))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h
    locksToTake :: [Int]
    locksToTake = sort $ map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ globmon) $ globhan


monitorLockProc :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorLockProc mon h =
  if (LockCoarsening OptVoid `elemOpt` (AST.monitor_transformers mon))
    then 
      monitorLockProcLockCoarsening mon h
    else
      monitorLockProcRaw mon

monitorLockProcRaw :: AST.Monitor -> Ivory eff ()
monitorLockProcRaw mon = do
  retValueLock <- call pthread_mutex_lock (monitorLock mon)
  assert (retValueLock ==? 0)

monitorLockProcLockCoarsening :: AST.Monitor -> AST.Handler -> Ivory eff ()
monitorLockProcLockCoarsening mon h = 
  forM_ (map (monitorLockWithCoarsening mon) locksToTake) $ \lock -> do
    retValueLock <- call pthread_mutex_lock lock
    assert (retValueLock ==? 0)
  where    
    (Just (LockCoarsening (OptMonitor globmon))) = getOpt (LockCoarsening OptVoid) $ AST.monitor_transformers mon
    (Just (LockCoarsening (OptHandler globhan))) = getOpt (LockCoarsening OptVoid) $ AST.handler_transformers h
    locksToTake :: [Int]
    locksToTake = sort $ map succ $ nub $ concat $ map (\x -> findIndices (\list -> elem x list) $ globmon) $ globhan

