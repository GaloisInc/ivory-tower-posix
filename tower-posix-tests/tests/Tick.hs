{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ivory.Tower
import Ivory.Stdlib
import Ivory.Language
import Ivory.OS.Posix.Tower

printf :: Def ('[IString, Uint8] ':-> Sint32)
printf = importProc "printf" "stdio.h"

app :: Tower e ()
app = do
  towerModule appMod
  towerDepends appMod

  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> do
        s += 1
        emit e (constRef (s :: Ref 'Global ('Stored Uint8)))

  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m -> do
        refCopy s m
        s' <- deref s
        call_ printf "Val: %i\n" s'

appMod :: Module
appMod = package "appMod" $ incl printf

main :: IO ()
main = compileTowerPosix (const $ return ()) app
