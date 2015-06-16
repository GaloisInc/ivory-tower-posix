{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Posix.Tower.IO where

import Ivory.Language
import Ivory.Tower
import Ivory.OS.Posix.Tower.EventLoop
import Ivory.OS.Posix.Tower.Signal

type FD = Sint32

stdin, stdout, stderr :: FD
stdin = extern "STDIN_FILENO" "unistd.h"
stdout = extern "STDOUT_FILENO" "unistd.h"
stderr = extern "STDERR_FILENO" "unistd.h"

readFD :: String -> FD -> Tower e (ChanOutput (Stored Uint8))
readFD name fd = do
  (sink, source) <- channel

  watchIO name fd ReadOnly $ \ sig global_watcher -> do
    let unix_read :: Def ('[FD, Ref s (CArray (Stored Uint8)), Uint32] :-> Sint32)
        unix_read = importProc "read" "unistd.h"

    monitorModuleDef $ do
      uses_libev
      incl unix_read

    handler sig "readable" $ do
      target <- emitter sink 512
      callback $ const $ do
        buf <- local (izero :: Init (Array 512 (Stored Uint8)))
        got <- call unix_read fd (toCArray buf) (arrayLen buf)
        ifte_ (got <=? 0)
          (do
            loop <- call ev_default_loop 0
            call_ ev_io_stop loop global_watcher
          ) (do
            for (toIx got) $ \ i -> do
              emit target $ constRef buf ! i
          )

  return source
