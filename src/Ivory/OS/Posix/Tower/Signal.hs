{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Posix.Tower.Signal where

import Ivory.Language
import Ivory.Tower.Tower
import Ivory.OS.Posix.Tower.EventLoop

data Watcher = Watcher
  { watcherName :: String
  , watcherDefs :: (forall eff. Ivory eff ()) -> ModuleDef
  , watcherInit :: forall eff. Ivory eff ()
  }

instance Signalable Watcher where
  signalName = watcherName
  signalHandler = watcherDefs
  signalInit = watcherInit

data IOEventMask
  = ReadOnly
  | WriteOnly
  | ReadWrite

watchIO :: String -> Sint32 -> IOEventMask -> Watcher
watchIO name fd eventmask = Watcher name defs $ call_ start
  where
  global_watcher = area (name ++ "_watcher") Nothing

  start = proc (name ++ "_start") $ body $ do
    call_ ev_io_init (addrOf global_watcher) (procPtr $ callback $ return ()) fd $ case eventmask of
      ReadOnly -> ev_READ
      WriteOnly -> ev_WRITE
      ReadWrite -> ev_READ .| ev_WRITE
    default_loop <- call ev_default_loop 0
    call_ ev_io_start default_loop (addrOf global_watcher)
    retVoid

  callback :: (forall eff. Ivory eff ()) -> Def ('[Ref s2 (Struct "ev_loop"), Ref s3 (Struct "ev_io"), Sint32] :-> ())
  callback action = proc (name ++ "_callback") $ \ _loop _watcher _events -> body action

  defs :: (forall eff. Ivory eff ()) -> ModuleDef
  defs action = do
    uses_libev
    incl start
    private $ do
      defMemArea global_watcher
      incl $ callback action
