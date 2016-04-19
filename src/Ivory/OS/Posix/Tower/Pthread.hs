{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.Posix.Tower.Pthread where

import Ivory.Language
import Ivory.Language.Type

pthread_header :: String
pthread_header = "pthread.h"


type VoidType =  NewType "void"

instance IvoryType VoidType where
  ivoryType _ = mkNewType (Proxy :: Proxy "void")

type ThreadAttribute =  NewType "pthread_attr_t"

instance IvoryType ThreadAttribute where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_attr_t")

type ThreadType =  NewType "pthread_t"

instance IvoryType ThreadType where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_t")

type ThreadPtr s = ProcPtr ('[Ref s ('Stored VoidType)] ':-> ())

pthread_create :: Def ('[Ref s ('Stored ThreadType), Ref s ('Stored ThreadAttribute), ThreadPtr s2, Ref s ('Stored VoidType)] ':-> Sint32)
pthread_create = importProc "pthread_create" pthread_header

uses_libpthread :: ModuleDef
uses_libpthread = do
  incl pthread_create
