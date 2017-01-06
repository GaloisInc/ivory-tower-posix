{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.IO

printf_2 :: Def ('[IString, Uint8, Uint8] ':-> ())
printf_2 = importProc "printf" "stdio.h"

app :: Tower e ()
app = do
  inChan <- readFD "stdin" stdin
  dump   <- period $ Milliseconds 1000

  monitor "accumulate" $ do
    monitorModuleDef $ do
      incl printf_2

    minVal <- stateInit "minChar" $ ival 109
    maxVal <- stateInit "maxChar" $ ival 109

    handler inChan "reader" $ do
      callbackV $ \ ch -> do
        lastMin <- deref minVal
        -- Ignore ASCII 10 (return)
        when (ch <? lastMin .&& ch /=? 10) $ store minVal ch
        lastMax <- deref maxVal
        -- Ignore ASCII 10 (return)
        when (ch >? lastMax .&& ch /=? 10) $ store maxVal ch

    handler dump "dumper" $ do
      callback $ const $ do
        lastMin <- deref minVal
        lastMax <- deref maxVal
        call_ printf_2 "range '%c' - '%c'\n" lastMin lastMax

main :: IO ()
main = compileTowerPosix (const $ return ()) app
