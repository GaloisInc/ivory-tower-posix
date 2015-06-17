{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.Posix.Tower.Serial (serialIO) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.OS.Posix.Tower.EventLoop
import Ivory.OS.Posix.Tower.IO
import Ivory.OS.Posix.Tower.Signal

[ivory| abstract struct termios "termios.h" |]

cfmakeraw :: Def ('[Ref s (Struct "termios")] :-> ())
cfmakeraw = importProc "cfmakeraw" "termios.h"

type BaudRate = Uint32

cfsetispeed :: Def ('[Ref s (Struct "termios"), BaudRate] :-> ())
cfsetispeed = importProc "cfsetispeed" "termios.h"

cfsetospeed :: Def ('[Ref s (Struct "termios"), BaudRate] :-> ())
cfsetospeed = importProc "cfsetospeed" "termios.h"

b115200 :: BaudRate
b115200 = extern "B115200" "termios.h"

tcsaNow :: Sint32
tcsaNow = extern "TCSANOW" "termios.h"

tcgetattr :: Def ('[FD, Ref s (Struct "termios")] :-> ())
tcgetattr = importProc "tcgetattr" "termios.h"

tcsetattr :: Def ('[FD, Sint32, ConstRef s (Struct "termios")] :-> ())
tcsetattr = importProc "tcsetattr" "termios.h"

unix_write :: Def ('[FD, ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint32)
unix_write = importProc "write" "unistd.h"

serialIO :: IvoryString s
         => Tower e ( BackpressureTransmit s (Stored IBool)
                    , ChanOutput (Stored Uint8))
serialIO = do
  reader <- readFD "serial_in" stdin

  (newSource, new) <- channel
  (status, statusSink) <- channel

  watchIO "serial_out" stdout WriteOnly $ \ ready watcher -> do
    monitorModuleDef $ do
      uses_libev
      incl tcgetattr
      incl cfmakeraw
      incl cfsetispeed
      incl cfsetospeed
      incl tcsetattr
      incl unix_write
      inclSym b115200
      inclSym tcsaNow
      inclSym stdin

    handler systemInit "serial_setup" $ do
      callback $ const $ do
        termset <- local izero
        call_ tcgetattr stdin termset
        call_ cfmakeraw termset
        call_ cfsetispeed termset b115200
        call_ cfsetospeed termset b115200
        call_ tcsetattr stdin tcsaNow $ constRef termset

        default_loop <- call ev_default_loop 0
        call_ ev_io_stop default_loop watcher

    str <- state "serial_current_output"

    handler new "serial_start_write" $ do
      callback $ \ new_str -> do
        refCopy str new_str
        comment "defer to next main-loop iteration to break call-graph cycles"
        default_loop <- call ev_default_loop 0
        call_ ev_io_start default_loop watcher

    handler ready "serial_can_write" $ do
      sendStatus <- emitter status 1
      callback $ const $ do
        let buf = constRef $ toCArray $ str ~> stringDataL
        len <- fmap signCast $ deref $ str ~> stringLengthL
        written <- call unix_write stdout buf len
        -- TODO: handle partial writes
        assert (signCast written ==? len)

        default_loop <- call ev_default_loop 0
        call_ ev_io_stop default_loop watcher
        emitV sendStatus true

  return (BackpressureTransmit newSource statusSink, reader)
