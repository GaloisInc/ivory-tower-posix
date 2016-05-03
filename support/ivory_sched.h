/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * ivory_sched.h --- Ivory Sched primitives for accessing sched_param struct
 *
 * Copyright (C) 2016, Galois, Inc.
 * All Rights Reserved.
 */

#ifndef __SMACCMPILOT_IVORY_SCHED_H__
#define __SMACCMPILOT_IVORY_SCHED_H__

#include <sched.h>
#include <pthread.h>

#ifdef __cplusplus
extern "C" {
#endif


/* DEFINING PTHREAD_PRIO_INHERIT in case of not defined */
#ifndef PTHREAD_PRIO_INHERIT
#ifndef PTHREAD_PRIO_NONE
#define PTHREAD_PRIO_NONE 0
#else
#define PTHREAD_PRIO_INHERIT PTHREAD_PRIO_NONE
#endif
#endif

static inline void ivory_sched_param_priority(struct sched_param* attr, int value)
{
    attr->sched_priority = value;
}

#ifdef __cplusplus
}
#endif

#endif  /* !defined __SMACCMPILOT_IVORY_HW_PRIM_H__ */