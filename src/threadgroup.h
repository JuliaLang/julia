/*
Copyright (c) 2014, Intel Corporation

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef THREADGROUP_H
#define THREADGROUP_H

#include <stdint.h>
#include "uv.h"

// for the barrier
typedef struct {
    volatile int sense;

} ti_thread_sense_t;


// thread group
typedef struct {
    int16_t             *tid_map, num_threads, added_threads;
    uint8_t             num_sockets, num_cores, num_threads_per_core;

    // fork/join/barrier
    uint8_t             forked;
    volatile uint8_t    group_sense;
    ti_thread_sense_t  **thread_sense;
    void                *envelope;

    // to let threads sleep
    uv_mutex_t  alarm_lock;
    uv_cond_t   alarm;
    uint64_t            sleep_threshold;

} ti_threadgroup_t;


int  ti_threadgroup_create(uint8_t num_sockets, uint8_t num_cores,
                           uint8_t num_threads_per_core,
                           ti_threadgroup_t **newtg);
int  ti_threadgroup_addthread(ti_threadgroup_t *tg, int16_t ext_tid,
                              int16_t *tgtid);
int  ti_threadgroup_initthread(ti_threadgroup_t *tg, int16_t ext_tid);
int  ti_threadgroup_member(ti_threadgroup_t *tg, int16_t ext_tid,
                           int16_t *tgtid);
int  ti_threadgroup_size(ti_threadgroup_t *tg, int16_t *tgsize);
int  ti_threadgroup_fork(ti_threadgroup_t *tg, int16_t ext_tid,
                         void **bcast_val);
int  ti_threadgroup_join(ti_threadgroup_t *tg, int16_t ext_tid);
void ti_threadgroup_barrier(ti_threadgroup_t *tg, int16_t ext_tid);
int  ti_threadgroup_destroy(ti_threadgroup_t *tg);

extern ti_threadgroup_t *tgworld;

#endif  /* THREADGROUP_H */

