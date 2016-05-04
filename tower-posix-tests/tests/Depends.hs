{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Tower
import Ivory.Language
import Ivory.OS.Posix.Tower

[ivory|
struct Foo { foo :: Stored Uint8 }
|]

fooMod :: Module
fooMod = package "foo" (defStruct (Proxy :: Proxy "Foo"))

app :: Tower e ()
app = do
  towerModule fooMod
  towerDepends fooMod

  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> emit e (constRef (s :: Ref 'Global ('Struct "Foo")))

  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m ->
        refCopy s m

main :: IO ()
main = do
  compileTowerPosix (const $ return ()) app
