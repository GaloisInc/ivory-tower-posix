{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.OS.Posix.Tower.IO where

import Ivory.Language
import Ivory.Tower
import Ivory.OS.Posix.Tower.EventLoop
import Ivory.OS.Posix.Tower.Signal

type FD = Sint32

stdin, stdout, stderr :: FD
stdin = extern "STDIN_FILENO"
stdout = extern "STDOUT_FILENO"
stderr = extern "STDERR_FILENO"

readFD :: String -> FD -> Tower e (ChanOutput (Stored Uint8))
readFD name fd = do
  (sink, source) <- channel
  sig <- signal (watchIO name fd ReadOnly) (us 0)

  monitor name $ do
    let read :: Def ('[FD, Ref s (Array 512 (Stored Uint8)), Uint32] :-> Sint32)
        read = importProc "read" "unistd.h"
    monitorModuleDef $ incl read

    handler sig "readable" $ do
      target <- emitter sink 512
      callback $ const $ do
        buf <- local (izero :: Init (Array 512 (Stored Uint8)))
        got <- call read fd buf (arrayLen buf)
        ifte_ (got <=? 0)
          (do
            loop <- call ev_default_loop 0
            call_ ev_break loop $ extern "EVBREAK_ALL"
          ) (do
            for (toIx got) $ \ i -> do
              emit target $ constRef buf ! i
          )

  return source
