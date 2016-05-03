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

#ifdef __cplusplus
extern "C" {
#endif


static inline void ivory_sched_param_priority(struct sched_param* attr, int value)
{
    attr->sched_priority = value;
}

#ifdef __cplusplus
}
#endif

#endif  /* !defined __SMACCMPILOT_IVORY_HW_PRIM_H__ */