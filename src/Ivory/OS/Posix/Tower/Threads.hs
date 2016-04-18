{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.Posix.Tower.Threads where

import Ivory.Language
import Ivory.Language.Type

threads_header :: String
threads_header = "threads.h"


type VoidType =  NewType "void"

instance IvoryType VoidType where
  ivoryType _ = mkNewType (Proxy :: Proxy "void")

type ThreadType =  NewType "thrd_t"

instance IvoryType ThreadType where
  ivoryType _ = mkNewType (Proxy :: Proxy "thrd_t")

type ThreadPtr s = ProcPtr ('[Ref s ('Stored VoidType)] ':-> ())

thrd_create :: Def ('[Ref s ('Stored ThreadType), ThreadPtr s2, Ref s ('Stored VoidType)] ':-> Sint32)
thrd_create = importProc "thrd_create" threads_header

uses_libthreads :: ModuleDef
uses_libthreads = do
  incl thrd_create
