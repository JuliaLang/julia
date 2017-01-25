// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef THREADGROUP_H
#define THREADGROUP_H

#include <stdint.h>
#include "uv.h"

// for the barrier
typedef struct {
    int sense;
} ti_thread_sense_t;

// thread group
typedef struct {
    int16_t *tid_map, num_threads, added_threads;
    uint8_t num_sockets, num_cores, num_threads_per_core;

    // fork/join/barrier
    uint8_t group_sense; // Written only by master thread
    ti_thread_sense_t **thread_sense;
    void              *envelope;

    // to let threads sleep
    uv_mutex_t alarm_lock;
    uv_cond_t  alarm;
    uint64_t   sleep_threshold;
} ti_threadgroup_t;

int ti_threadgroup_create(uint8_t num_sockets, uint8_t num_cores,
                          uint8_t num_threads_per_core,
                          ti_threadgroup_t **newtg);
int ti_threadgroup_addthread(ti_threadgroup_t *tg, int16_t ext_tid,
                             int16_t *tgtid);
int ti_threadgroup_initthread(ti_threadgroup_t *tg, int16_t ext_tid);
int ti_threadgroup_member(ti_threadgroup_t *tg, int16_t ext_tid,
                          int16_t *tgtid);
int ti_threadgroup_size(ti_threadgroup_t *tg, int16_t *tgsize);
int ti_threadgroup_fork(ti_threadgroup_t *tg, int16_t ext_tid,
                        void **bcast_val);
int ti_threadgroup_join(ti_threadgroup_t *tg, int16_t ext_tid);
int ti_threadgroup_destroy(ti_threadgroup_t *tg);

#endif  /* THREADGROUP_H */
