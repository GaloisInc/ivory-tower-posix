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


sched_header :: String
sched_header = "sched.h"

-------------
-- TYPES FOR 32 BITS
-------------

type CSize_t = Uint32
type CInt    = Sint32

-- Void type
type VoidType =  NewType "void"

instance IvoryType VoidType where
  ivoryType _ = mkNewType (Proxy :: Proxy "void")

-- other structs impoted in pthread.h

[ivory|
abstract struct sched_param "sched.h"
abstract struct timespec "time.h"
|]

-- Pthread types, defined in sys/types.h

type PthreadAttr =  NewType "pthread_attr_t"

instance IvoryType PthreadAttr where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_attr_t")



type PthreadCond =  NewType "pthread_cond_t"

instance IvoryType PthreadCond where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_cond_t")


type PthreadCondAttr =  NewType "pthread_condattr_t"

instance IvoryType PthreadCondAttr where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_condattr_t")


type PthreadKey =  NewType "pthread_key_t"

instance IvoryType PthreadKey where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_key_t")


type PthreadMutex =  NewType "pthread_mutex_t"

instance IvoryType PthreadMutex where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_mutex_t")

instance IvoryVar PthreadMutex where
  wrapVar    = wrapVarExpr
  unwrapExpr = getNewType

instance IvoryExpr PthreadMutex where
  wrapExpr = NewType

instance IvoryStore PthreadMutex

instance IvoryZeroVal PthreadMutex where
  izeroval = inewtype

type PthreadMutexAttr =  NewType "pthread_mutexattr_t"

instance IvoryType PthreadMutexAttr where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_mutexattr_t")


type PthreadOnce =  NewType "pthread_once_t"

instance IvoryType PthreadOnce where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_once_t")


type PthreadRWLock =  NewType "pthread_rwlock_t"

instance IvoryType PthreadRWLock where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_rwlock_t")


type PthreadRWLockAttr =  NewType "pthread_rwlockattr_t"

instance IvoryType PthreadRWLockAttr where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_rwlockattr_t")


type PthreadType =  NewType "pthread_t"

instance IvoryType PthreadType where
  ivoryType _ = mkNewType (Proxy :: Proxy "pthread_t")

instance IvoryVar PthreadType where
  wrapVar    = wrapVarExpr
  unwrapExpr = getNewType

instance IvoryExpr PthreadType where
  wrapExpr = NewType

instance IvoryStore PthreadType

type ThreadFuncPtr s = ProcPtr ('[Ref s ('Stored VoidType)] ':-> Ref s ('Stored VoidType))

type CleanupFuncPtr s = ProcPtr ('[Ref s ('Stored VoidType)] ':-> ())

type OnceFuncPtr s = ProcPtr ('[] ':-> ())


sched_FIFO, sched_RR, sched_OTHER, sched_DEADLINE :: CInt
sched_FIFO     = extern "SCHED_FIFO" sched_header
sched_RR       = extern "SCHED_RR" sched_header
sched_OTHER    = extern "SCHED_OTHER" sched_header
sched_DEADLINE = extern "SCHED_DEADLINE" sched_header



pthread_CANCEL_ASYNCHRONOUS, pthread_CANCEL_ENABLE, pthread_CANCEL_DEFERRED, pthread_CANCEL_DISABLE, pthread_CANCELED :: CInt
pthread_COND_INITIALIZER, pthread_CREATE_DETACHED, pthread_CREATE_JOINABLE, pthread_EXPLICIT_SCHED, pthread_INHERIT_SCHED :: CInt
pthread_MUTEX_DEFAULT, pthread_MUTEX_ERRORCHECK, pthread_MUTEX_NORMAL, pthread_MUTEX_RECURSIVE :: CInt
pthread_ONCE_INIT, pthread_PRIO_INHERIT, pthread_PRIO_NONE, pthread_PRIO_PROTECT :: CInt
pthread_PROCESS_SHARED, pthread_PROCESS_PRIVATE, pthread_RWLOCK_INITIALIZER, pthread_SCOPE_PROCESS, pthread_SCOPE_SYSTEM :: CInt
pthread_MUTEX_INITIALIZER :: PthreadMutex

pthread_CANCEL_ASYNCHRONOUS = extern "PTHREAD_CANCEL_ASYNCHRONOUS" pthread_header
pthread_CANCEL_ENABLE       = extern "PTHREAD_CANCEL_ENABLE" pthread_header
pthread_CANCEL_DEFERRED     = extern "PTHREAD_CANCEL_DEFERRED" pthread_header
pthread_CANCEL_DISABLE      = extern "PTHREAD_CANCEL_DISABLE" pthread_header
pthread_CANCELED            = extern "PTHREAD_CANCELED" pthread_header
pthread_COND_INITIALIZER    = extern "PTHREAD_COND_INITIALIZER" pthread_header
pthread_CREATE_DETACHED     = extern "PTHREAD_CREATE_DETACHED" pthread_header
pthread_CREATE_JOINABLE     = extern "PTHREAD_CREATE_JOINABLE" pthread_header
pthread_EXPLICIT_SCHED      = extern "PTHREAD_EXPLICIT_SCHED" pthread_header
pthread_INHERIT_SCHED       = extern "PTHREAD_INHERIT_SCHED" pthread_header
pthread_MUTEX_DEFAULT       = extern "PTHREAD_MUTEX_DEFAULT" pthread_header
pthread_MUTEX_ERRORCHECK    = extern "PTHREAD_MUTEX_ERRORCHECK" pthread_header
pthread_MUTEX_NORMAL        = extern "PTHREAD_MUTEX_NORMAL" pthread_header
pthread_MUTEX_INITIALIZER   = extern "PTHREAD_MUTEX_INITIALIZER" pthread_header
pthread_MUTEX_RECURSIVE     = extern "PTHREAD_MUTEX_RECURSIVE" pthread_header
pthread_ONCE_INIT           = extern "PTHREAD_ONCE_INIT" pthread_header
pthread_PRIO_INHERIT        = extern "PTHREAD_PRIO_INHERIT" pthread_header
pthread_PRIO_NONE           = extern "PTHREAD_PRIO_NONE" pthread_header
pthread_PRIO_PROTECT        = extern "PTHREAD_PRIO_PROTECT" pthread_header
pthread_PROCESS_SHARED      = extern "PTHREAD_PROCESS_SHARED" pthread_header
pthread_PROCESS_PRIVATE     = extern "PTHREAD_PROCESS_PRIVATE" pthread_header
pthread_RWLOCK_INITIALIZER  = extern "PTHREAD_RWLOCK_INITIALIZER" pthread_header
pthread_SCOPE_PROCESS       = extern "PTHREAD_SCOPE_PROCESS" pthread_header
pthread_SCOPE_SYSTEM        = extern "PTHREAD_SCOPE_SYSTEM" pthread_header

pthread_cast_voidfunc_fromint :: Def ('[ProcPtr ('[Ref s ('Stored Sint64)] ':-> ())] ':-> ThreadFuncPtr s)
pthread_cast_voidfunc_fromint = importProc "(void* (*) (void*))" pthread_header



pthread_attr_destroy :: Def ('[Ref s ('Stored PthreadAttr)] ':-> CInt)
pthread_attr_destroy = importProc "pthread_attr_destroy" pthread_header

pthread_attr_getdetachstate :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_attr_getdetachstate = importProc "pthread_attr_getdetachstate" pthread_header

pthread_attr_getguardsize :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored CSize_t)] ':-> CInt)
pthread_attr_getguardsize = importProc "pthread_attr_getguardsize" pthread_header

pthread_attr_getinheritsched :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_attr_getinheritsched = importProc "pthread_attr_getinheritsched" pthread_header

pthread_attr_getschedparam :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Struct "sched_param")] ':-> CInt)
pthread_attr_getschedparam = importProc "pthread_attr_getschedparam" pthread_header

pthread_attr_getschedpolicy :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_attr_getschedpolicy = importProc "pthread_attr_getschedpolicy" pthread_header

pthread_attr_getscope :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_attr_getscope = importProc "pthread_attr_getscope" pthread_header

pthread_attr_getstackaddr :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored (Ref s2 ('Stored VoidType)))] ':-> CInt)
pthread_attr_getstackaddr = importProc "pthread_attr_getstackaddr" pthread_header

pthread_attr_getstacksize :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored CSize_t)] ':-> CInt)
pthread_attr_getstacksize = importProc "pthread_attr_getstacksize" pthread_header



pthread_attr_init :: Def ('[Ref s ('Stored PthreadAttr)] ':-> CInt)
pthread_attr_init = importProc "pthread_attr_init" pthread_header

pthread_attr_setdetachstate :: Def ('[Ref s ('Stored PthreadAttr), CInt] ':-> CInt)
pthread_attr_setdetachstate = importProc "pthread_attr_setdetachstate" pthread_header

pthread_attr_setguardsize :: Def ('[Ref s ('Stored PthreadAttr), CSize_t] ':-> CInt)
pthread_attr_setguardsize = importProc "pthread_attr_setguardsize" pthread_header

pthread_attr_setinheritsched :: Def ('[Ref s ('Stored PthreadAttr), CInt] ':-> CInt)
pthread_attr_setinheritsched = importProc "pthread_attr_setinheritsched" pthread_header

pthread_attr_setschedparam :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Struct "sched_param")] ':-> CInt)
pthread_attr_setschedparam = importProc "pthread_attr_setschedparam" pthread_header

pthread_attr_setschedpolicy :: Def ('[Ref s ('Stored PthreadAttr), CInt] ':-> CInt)
pthread_attr_setschedpolicy = importProc "pthread_attr_setschedpolicy" pthread_header

pthread_attr_setscope :: Def ('[Ref s ('Stored PthreadAttr), CInt] ':-> CInt)
pthread_attr_setscope = importProc "pthread_attr_setscope" pthread_header

pthread_attr_setstackaddr :: Def ('[Ref s ('Stored PthreadAttr), Ref s2 ('Stored VoidType)] ':-> CInt)
pthread_attr_setstackaddr = importProc "pthread_attr_setstackaddr" pthread_header

pthread_attr_setstacksize :: Def ('[Ref s ('Stored PthreadAttr), CSize_t] ':-> CInt)
pthread_attr_setstacksize = importProc "pthread_attr_setstacksize" pthread_header



pthread_cancel :: Def ('[PthreadType] ':-> CInt)
pthread_cancel = importProc "pthread_cancel" pthread_header

pthread_cleanup_push :: Def ('[CleanupFuncPtr s2] ':-> ())
pthread_cleanup_push = importProc "pthread_cleanup_push" pthread_header

pthread_cleanup_pop :: Def ('[CInt] ':-> ())
pthread_cleanup_pop = importProc "pthread_cleanup_pop" pthread_header



pthread_cond_broadcast :: Def ('[Ref s ('Stored PthreadCond)] ':-> CInt)
pthread_cond_broadcast = importProc "pthread_cond_broadcast" pthread_header

pthread_cond_destroy :: Def ('[Ref s ('Stored PthreadCond)] ':-> CInt)
pthread_cond_destroy = importProc "pthread_cond_destroy" pthread_header

pthread_cond_init :: Def ('[Ref s ('Stored PthreadCond), Ref s2 ('Stored PthreadCondAttr)] ':-> CInt)
pthread_cond_init = importProc "pthread_cond_init" pthread_header

pthread_cond_signal :: Def ('[Ref s ('Stored PthreadCond)] ':-> CInt)
pthread_cond_signal = importProc "pthread_cond_signal" pthread_header

pthread_cond_timedwait :: Def ('[Ref s ('Stored PthreadCond), Ref s2 ('Stored PthreadMutex), Ref s3 ('Struct "timespec")] ':-> CInt)
pthread_cond_timedwait = importProc "pthread_cond_timedwait" pthread_header

pthread_cond_wait :: Def ('[Ref s ('Stored PthreadCond), Ref s ('Stored PthreadMutex)] ':-> CInt)
pthread_cond_wait = importProc "pthread_cond_wait" pthread_header



pthread_condattr_destroy :: Def ('[Ref s ('Stored PthreadCondAttr)] ':-> CInt)
pthread_condattr_destroy = importProc "pthread_condattr_destroy" pthread_header

pthread_condattr_getpshared :: Def ('[Ref s ('Stored PthreadCondAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_condattr_getpshared = importProc "pthread_condattr_getpshared" pthread_header

pthread_condattr_init :: Def ('[Ref s ('Stored PthreadCondAttr), CInt] ':-> CInt)
pthread_condattr_init = importProc "pthread_condattr_init" pthread_header

pthread_condattr_setpshared :: Def ('[Ref s ('Stored PthreadCondAttr)] ':-> CInt)
pthread_condattr_setpshared = importProc "pthread_condattr_setpshared" pthread_header



pthread_create :: Def ('[Ref s ('Stored PthreadType), Ref s1 ('Stored PthreadAttr), ThreadFuncPtr s2, Ref s3 ('Stored VoidType)] ':-> CInt)
pthread_create = importProc "pthread_create" pthread_header

pthread_detach :: Def ('[PthreadType] ':-> CInt)
pthread_detach = importProc "pthread_detach" pthread_header

pthread_equal :: Def ('[PthreadType, PthreadType] ':-> CInt)
pthread_equal = importProc "pthread_equal" pthread_header

pthread_exit :: Def ('[Ref s ('Stored VoidType)] ':-> ())
pthread_exit = importProc "pthread_exit" pthread_header

pthread_getconcurrency :: Def ('[] ':-> CInt)
pthread_getconcurrency = importProc "pthread_getconcurrency" pthread_header

pthread_getschedparam :: Def ('[PthreadType, Ref s ('Stored CInt), Ref s2 ('Struct "sched_param")] ':-> CInt)
pthread_getschedparam = importProc "pthread_getschedparam" pthread_header

pthread_getspecific :: Def ('[PthreadKey] ':-> Ref s ('Stored VoidType))
pthread_getspecific = importProc "pthread_getspecific" pthread_header



pthread_join ::  Def ('[PthreadType, Ref s ('Stored (Ref s ('Stored VoidType)))] ':-> CInt)
pthread_join = importProc "pthread_join" pthread_header

pthread_key_create :: Def ('[Ref s ('Stored PthreadKey), CleanupFuncPtr s2] ':-> CInt)
pthread_key_create = importProc "pthread_key_create" pthread_header

pthread_key_delete :: Def ('[PthreadKey] ':-> CInt)
pthread_key_delete = importProc "pthread_key_delete" pthread_header



pthread_mutex_destroy :: Def ('[Ref s ('Stored PthreadMutex)] ':-> CInt)
pthread_mutex_destroy = importProc "pthread_mutex_destroy" pthread_header

pthread_mutex_getprioceiling :: Def ('[Ref s ('Stored PthreadMutex), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_mutex_getprioceiling = importProc "pthread_mutex_getprioceiling" pthread_header

pthread_mutex_init :: Def ('[Ref s ('Stored PthreadMutex), Ref s2 ('Stored PthreadMutexAttr)] ':-> CInt)
pthread_mutex_init = importProc "pthread_mutex_init" pthread_header

pthread_mutex_lock :: Def ('[Ref s ('Stored PthreadMutex)] ':-> CInt)
pthread_mutex_lock = importProc "pthread_mutex_lock" pthread_header

pthread_mutex_setprioceiling :: Def ('[Ref s ('Stored PthreadMutex), CInt, Ref s2 ('Stored CInt)] ':-> CInt)
pthread_mutex_setprioceiling = importProc "pthread_mutex_setprioceiling" pthread_header

pthread_mutex_trylock :: Def ('[Ref s ('Stored PthreadMutex)] ':-> CInt)
pthread_mutex_trylock = importProc "pthread_mutex_trylock" pthread_header

pthread_mutex_unlock :: Def ('[Ref s ('Stored PthreadMutex)] ':-> CInt)
pthread_mutex_unlock = importProc "pthread_mutex_unlock" pthread_header



pthread_mutexattr_destroy :: Def ('[Ref s ('Stored PthreadMutexAttr)] ':-> CInt)
pthread_mutexattr_destroy = importProc "pthread_mutexattr_destroy" pthread_header

pthread_mutexattr_getprioceiling :: Def ('[Ref s ('Stored PthreadMutexAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_mutexattr_getprioceiling = importProc "pthread_mutexattr_getprioceiling" pthread_header

pthread_mutexattr_getprotocol :: Def ('[Ref s ('Stored PthreadMutexAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_mutexattr_getprotocol = importProc "pthread_mutexattr_getprotocol" pthread_header

pthread_mutexattr_getpshared :: Def ('[Ref s ('Stored PthreadMutexAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_mutexattr_getpshared = importProc "pthread_mutexattr_getpshared" pthread_header

pthread_mutexattr_gettype :: Def ('[Ref s ('Stored PthreadMutexAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_mutexattr_gettype = importProc "pthread_mutexattr_gettype" pthread_header

pthread_mutexattr_init :: Def ('[Ref s ('Stored PthreadMutexAttr)] ':-> CInt)
pthread_mutexattr_init = importProc "pthread_mutexattr_init" pthread_header

pthread_mutexattr_setprioceiling :: Def ('[Ref s ('Stored PthreadMutexAttr), CInt] ':-> CInt)
pthread_mutexattr_setprioceiling = importProc "pthread_mutexattr_setprioceiling" pthread_header

pthread_mutexattr_setprotocol :: Def ('[Ref s ('Stored PthreadMutexAttr), CInt] ':-> CInt)
pthread_mutexattr_setprotocol = importProc "pthread_mutexattr_setprotocol" pthread_header

pthread_mutexattr_setpshared :: Def ('[Ref s ('Stored PthreadMutexAttr), CInt] ':-> CInt)
pthread_mutexattr_setpshared = importProc "pthread_mutexattr_setpshared" pthread_header

pthread_mutexattr_settype :: Def ('[Ref s ('Stored PthreadMutexAttr), CInt] ':-> CInt)
pthread_mutexattr_settype = importProc "pthread_mutexattr_settype" pthread_header



pthread_once :: Def ('[Ref s ('Stored PthreadOnce), OnceFuncPtr s2] ':-> CInt)
pthread_once = importProc "pthread_once" pthread_header

pthread_rwlock_destroy :: Def ('[Ref s ('Stored PthreadRWLock)] ':-> CInt)
pthread_rwlock_destroy = importProc "pthread_rwlock_destroy" pthread_header

pthread_rwlock_init :: Def ('[Ref s ('Stored PthreadRWLock), Ref s2 ('Stored PthreadRWLockAttr)] ':-> CInt)
pthread_rwlock_init = importProc "pthread_rwlock_init" pthread_header

pthread_rwlock_rdlock :: Def ('[Ref s ('Stored PthreadRWLock)] ':-> CInt)
pthread_rwlock_rdlock = importProc "pthread_rwlock_rdlock" pthread_header

pthread_rwlock_tryrdlock :: Def ('[Ref s ('Stored PthreadRWLock)] ':-> CInt)
pthread_rwlock_tryrdlock = importProc "pthread_rwlock_tryrdlock" pthread_header

pthread_rwlock_trywrlock :: Def ('[Ref s ('Stored PthreadRWLock)] ':-> CInt)
pthread_rwlock_trywrlock = importProc "pthread_rwlock_trywrlock" pthread_header

pthread_rwlock_unlock :: Def ('[Ref s ('Stored PthreadRWLock)] ':-> CInt)
pthread_rwlock_unlock = importProc "pthread_rwlock_unlock" pthread_header

pthread_rwlock_wrlock :: Def ('[Ref s ('Stored PthreadRWLock)] ':-> CInt)
pthread_rwlock_wrlock = importProc "pthread_rwlock_trywrlock" pthread_header

pthread_rwlockattr_destroy :: Def ('[Ref s ('Stored PthreadRWLockAttr)] ':-> CInt)
pthread_rwlockattr_destroy = importProc "pthread_rwlockattr_destroy" pthread_header

pthread_rwlockattr_getpshared :: Def ('[Ref s ('Stored PthreadRWLockAttr), Ref s2 ('Stored CInt)] ':-> CInt)
pthread_rwlockattr_getpshared = importProc "pthread_rwlockattr_getpshared" pthread_header

pthread_rwlockattr_init :: Def ('[Ref s ('Stored PthreadRWLockAttr)] ':-> CInt)
pthread_rwlockattr_init = importProc "pthread_rwlockattr_init" pthread_header

pthread_rwlockattr_setpshared :: Def ('[Ref s ('Stored PthreadRWLockAttr), CInt] ':-> CInt)
pthread_rwlockattr_setpshared = importProc "pthread_rwlockattr_setpshared" pthread_header



pthread_self :: Def ('[] ':-> PthreadType)
pthread_self = importProc "pthread_self" pthread_header

pthread_setcancelstate :: Def ('[CInt, Ref s ('Stored CInt)] ':-> CInt)
pthread_setcancelstate = importProc "pthread_setcancelstate" pthread_header

pthread_setcanceltype :: Def ('[CInt, Ref s ('Stored CInt)] ':-> CInt)
pthread_setcanceltype = importProc "pthread_setcanceltype" pthread_header

pthread_setconcurrency :: Def ('[CInt] ':-> CInt)
pthread_setconcurrency = importProc "pthread_setconcurrency" pthread_header

pthread_setschedparam :: Def ('[PthreadType, CInt, Ref s ('Struct "sched_param")] ':-> CInt)
pthread_setschedparam = importProc "pthread_setschedparam" pthread_header

pthread_setspecific :: Def ('[PthreadKey, Ref s ('Stored VoidType)] ':-> CInt)
pthread_setspecific = importProc "pthread_setspecific" pthread_header

pthread_testcancel :: Def ('[] ':-> ())
pthread_testcancel = importProc "pthread_testcancel" pthread_header






sched_get_priority_max :: Def ('[CInt] ':-> (CInt))
sched_get_priority_max = importProc "sched_get_priority_max" sched_header

sched_get_priority_min :: Def ('[CInt] ':-> (CInt))
sched_get_priority_min = importProc "sched_get_priority_min" sched_header

uses_libpthread :: ModuleDef
uses_libpthread = do
  inclSym pthread_CANCEL_ASYNCHRONOUS
  inclSym pthread_CANCEL_ENABLE      
  inclSym pthread_CANCEL_DEFERRED    
  inclSym pthread_CANCEL_DISABLE     
  inclSym pthread_CANCELED           
  inclSym pthread_COND_INITIALIZER   
  inclSym pthread_CREATE_DETACHED    
  inclSym pthread_CREATE_JOINABLE    
  inclSym pthread_EXPLICIT_SCHED     
  inclSym pthread_INHERIT_SCHED      
  inclSym pthread_MUTEX_DEFAULT      
  inclSym pthread_MUTEX_ERRORCHECK   
  inclSym pthread_MUTEX_NORMAL       
  inclSym pthread_MUTEX_INITIALIZER  
  inclSym pthread_MUTEX_RECURSIVE    
  inclSym pthread_ONCE_INIT          
  inclSym pthread_PRIO_INHERIT       
  inclSym pthread_PRIO_NONE          
  inclSym pthread_PRIO_PROTECT       
  inclSym pthread_PROCESS_SHARED     
  inclSym pthread_PROCESS_PRIVATE    
  inclSym pthread_RWLOCK_INITIALIZER 
  inclSym pthread_SCOPE_PROCESS      
  inclSym pthread_SCOPE_SYSTEM   


  incl pthread_cast_voidfunc_fromint 

  incl pthread_attr_destroy
  incl pthread_attr_getdetachstate
  incl pthread_attr_getguardsize
  incl pthread_attr_getinheritsched
  incl pthread_attr_getschedparam
  incl pthread_attr_getschedpolicy
  incl pthread_attr_getscope
  incl pthread_attr_getstackaddr
  incl pthread_attr_getstacksize

  incl pthread_attr_init
  incl pthread_attr_setdetachstate
  incl pthread_attr_setguardsize
  incl pthread_attr_setinheritsched
  incl pthread_attr_setschedparam
  incl pthread_attr_setschedpolicy
  incl pthread_attr_setscope
  incl pthread_attr_setstackaddr
  incl pthread_attr_setstacksize

  incl pthread_cancel
  incl pthread_cleanup_push
  incl pthread_cleanup_pop

  incl pthread_cond_broadcast
  incl pthread_cond_destroy
  incl pthread_cond_init
  incl pthread_cond_signal
  incl pthread_cond_timedwait
  incl pthread_cond_wait

  incl pthread_condattr_destroy
  incl pthread_condattr_getpshared
  incl pthread_condattr_init
  incl pthread_condattr_setpshared

  incl pthread_create  
  incl pthread_detach
  incl pthread_equal
  incl pthread_exit
  incl pthread_getconcurrency
  incl pthread_getschedparam
  incl pthread_getspecific

  incl pthread_join
  incl pthread_key_create
  incl pthread_key_delete

  incl pthread_mutex_destroy
  incl pthread_mutex_getprioceiling
  incl pthread_mutex_init
  incl pthread_mutex_lock
  incl pthread_mutex_setprioceiling
  incl pthread_mutex_trylock
  incl pthread_mutex_unlock

  incl pthread_mutexattr_destroy
  incl pthread_mutexattr_getprioceiling
  incl pthread_mutexattr_getprotocol
  incl pthread_mutexattr_getpshared
  incl pthread_mutexattr_gettype
  incl pthread_mutexattr_init
  incl pthread_mutexattr_setprioceiling
  incl pthread_mutexattr_setprotocol
  incl pthread_mutexattr_setpshared
  incl pthread_mutexattr_settype

  incl pthread_once
  incl pthread_rwlock_destroy
  incl pthread_rwlock_init
  incl pthread_rwlock_rdlock
  incl pthread_rwlock_tryrdlock
  incl pthread_rwlock_trywrlock
  incl pthread_rwlock_unlock
  incl pthread_rwlock_wrlock
  incl pthread_rwlockattr_destroy
  incl pthread_rwlockattr_getpshared
  incl pthread_rwlockattr_init
  incl pthread_rwlockattr_setpshared

  incl pthread_self
  incl pthread_setcancelstate
  incl pthread_setcanceltype
  incl pthread_setconcurrency
  incl pthread_setschedparam
  incl pthread_setspecific
  incl pthread_testcancel


  inclSym sched_DEADLINE    
  inclSym sched_OTHER 
  inclSym sched_RR      
  inclSym sched_FIFO    

  incl sched_get_priority_min
  incl sched_get_priority_max