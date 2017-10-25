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

open :: Def ('[IString, Uint32] ':-> FD)
open = importProc "open" "fcntl.h"

o_RDONLY, o_WRONLY, o_RDWR :: Uint32
o_RDONLY = extern "O_RDONLY" "fcntl.h"
o_WRONLY = extern "O_WRONLY" "fcntl.h"
o_RDWR = extern "O_RDWR" "fcntl.h"

readFD :: String -> FD -> Tower e (ChanOutput ('Stored Uint8))
readFD name fd = do
  (sink, source) <- channel

  watchIO name fd ReadOnly $ \ sig global_watcher -> do
    let unix_read :: Def ('[FD, Ref s ('CArray ('Stored Uint8)), Uint32] ':-> Sint32)
        unix_read = importProc "read" "unistd.h"

    monitorModuleDef $ do
      uses_libev
      incl unix_read

    handler sig "readable" $ do
      -- this used to be a 512 byte chunk, but the generated emitter delivery
      -- had too many nested parens for clang to compile without a special
      -- option. so instead lets just make it work on smaller chunks.
      target <- emitter sink 128
      callback $ const $ do
        buf <- local (izero :: Init ('Array 128 ('Stored Uint8)))
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

-- open additional filedescriptors in sequence specifed by paths
--
-- returns list of filedescriptors in the order as specified paths
--
-- allows opening additional serialIOs with
-- [fd, _] <- getFDs ["/dev/ttyUSB0"]
-- (ostream, istream) <- serialIOFD fd
--
getFDs :: [IString] -> Tower e ([Sint32])
getFDs paths = do
  flip mapM_ (zip paths [fdbase .. ]) $ \(path, expect_fd) -> monitor "fdmon" $ do
    monitorModuleDef $ do
      incl open
      inclSym o_RDWR
    handler systemInit "fdopen" $ do
      callback $ const $ do
        fd <- call open path o_RDWR
        assert $ fd ==? (fromIntegral expect_fd :: Sint32)

  return $ map fromIntegral [fdbase .. fdbase + length paths]
  where
  fdbase = 5
