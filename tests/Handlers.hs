{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.IO

printf_2char :: Def ('[IString, Uint8, Uint8] :-> ())
printf_2char = importProc "printf" "stdio.h"

app :: Tower e ()
app = do
  inChan <- readFD "stdin" stdin
  dump <- period $ Milliseconds 100

  monitor "accumulate" $ do
    monitorModuleDef $ do
      incl printf_2char
      inclSym stdin

    minChar <- stateInit "minChar" $ ival maxBound
    maxChar <- stateInit "maxChar" $ ival minBound

    handler inChan "reader" $ do
      callbackV $ \ ch -> do
        lastMin <- deref minChar
        when (ch <? lastMin) $ store minChar ch
        lastMax <- deref maxChar
        when (ch >? lastMax) $ store maxChar ch

    handler dump "dumper" $ do
      callback $ const $ do
        lastMin <- deref minChar
        lastMax <- deref maxChar
        call_ printf_2char "range '%c' - '%c'\n" lastMin lastMax
        store minChar maxBound
        store maxChar minBound

main :: IO ()
main = compileTowerPosix (const $ return ()) app
