{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Posix.Tower.Signal where

import Ivory.Language
import Ivory.Tower
import Ivory.OS.Posix.Tower.EventLoop

data Watcher = Watcher
  { watcherName :: String
  , watcherDefs :: (forall s. Ivory (AllocEffects s) ()) -> ModuleDef
  , watcherInit :: forall eff. Ivory eff ()
  }

instance Signalable Watcher where
  signalName = watcherName
  signalHandler = watcherDefs
  signalInit = watcherInit
  signalNumber _ = 42424242

data IOEventMask
  = ReadOnly
  | WriteOnly
  | ReadWrite

watchIO :: String
        -> Sint32
        -> IOEventMask
        -> (ChanOutput ('Stored ITime) -> Ref 'Global ('Struct "ev_io") -> Monitor e ())
        -> Tower e ()
watchIO name fd eventmask mon = do
  sig <- signal (Watcher name defs $ call_ start $ procPtr $ cb $ return ()) (us 0)
  towerModule m

  monitor name $ do
    monitorModuleDef $ depend m

    mon sig $ addrOf global_watcher
  where
  cb :: (forall s1. Ivory (AllocEffects s1) ()) -> Def ('[Ref s2 ('Struct "ev_loop"), Ref s3 ('Struct "ev_io"), Sint32] ':-> ())
  cb action = proc (name ++ "_callback") $ \ _loop _watcher _events -> body $
    noReturn action

  global_watcher = area (name ++ "_watcher") Nothing

  start = voidProc (name ++ "_start") $ \ cbPtr -> body $ do
    call_ ev_io_init (addrOf global_watcher) cbPtr fd $
      case eventmask of
        ReadOnly -> ev_READ
        WriteOnly -> ev_WRITE
        ReadWrite -> ev_READ .| ev_WRITE
    default_loop <- call ev_default_loop 0
    call_ ev_io_start default_loop (addrOf global_watcher)

  m = package name $ do
    uses_libev
    defMemArea global_watcher
    incl start
    -- FIXME: fd might not be an `extern` expression
    inclSym fd

  defs :: (forall s. Ivory (AllocEffects s) ()) -> ModuleDef
  defs action = do
    depend m
    private $ do
      incl $ cb action
