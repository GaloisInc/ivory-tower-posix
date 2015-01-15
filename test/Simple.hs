{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ivory.Tower
import Ivory.Tower.Compile
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend
import Ivory.OS.Posix.Tower

simpleTower :: Tower e ()
simpleTower = do
  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> emit e (constRef (s :: Ref Global (Stored Uint8)))

  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m ->
        refCopy s m

--------------------------------------------------------------------------------
-- Example with dependencies

[ivory|
struct Foo { foo :: Stored Uint8 }
|]

fooMod :: Module
fooMod = package "foo" (defStruct (Proxy :: Proxy "Foo"))

simpleTower2 :: Tower e ()
simpleTower2 = do
  towerModule fooMod
  towerDepends fooMod

  (c1in, c1out) <- channel
  per <- period (Microseconds 1000)
  monitor "m1" $ do
    s <- state "local_st"
    handler per "tick" $ do
      e <- emitter c1in 1
      callback $ \_ -> emit e (constRef (s :: Ref Global (Struct "Foo")))

  monitor "m2" $ do
    s <- state "last_m2_chan1_message"
    handler c1out "chan1msg" $ do
      callback $ \m ->
        refCopy s m

--------------------------------------------------------------------------------

main :: IO ()
main = do
  runTowerCompile simpleTower (posix ())
    initialOpts { outDir = Just "simpletower-out" }
  runTowerCompile simpleTower2 (posix ())
    initialOpts { outDir = Just "simpletower2-out" }
