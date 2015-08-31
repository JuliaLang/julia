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

#ifndef THREADING_H
#define THREADING_H

#include <stdint.h>
#include "julia.h"
#include <pthread.h>
#include "threadgroup.h"

#define PROFILE_JL_THREADING            1


// thread ID
extern JL_THREAD int16_t ti_tid;

// GC
extern struct _jl_thread_heap_t **jl_all_heaps;
extern jl_gcframe_t ***jl_all_pgcstacks;
extern jl_thread_task_state_t *jl_all_task_states;

extern DLLEXPORT int jl_n_threads;  // # threads we're actually using

// thread state
enum {
    TI_THREAD_INIT,
    TI_THREAD_WORK
};


// passed to thread function
typedef struct {
    int16_t volatile    state;
    int16_t             tid;
    ti_threadgroup_t    *tg;

} ti_threadarg_t;


// commands to thread function
enum {
    TI_THREADWORK_DONE,
    TI_THREADWORK_RUN
};


// work command to thread function
typedef struct {
    uint8_t             command;
    jl_function_t       *fun;
    jl_svec_t           *args;
    jl_value_t          *ret;

} ti_threadwork_t;


// basic functions for thread creation
int  ti_threadcreate(pthread_t *pthread_id, int proc_num,
                     void *(*thread_fun)(void *), void *thread_arg);
void ti_threadsetaffinity(uint64_t pthread_id, int proc_num);

// thread function
void *ti_threadfun(void *arg);

// helpers for thread function
void ti_initthread(int16_t tid);
jl_value_t *ti_runthread(jl_function_t *f, jl_svec_t *args, size_t nargs);


#endif  /* THREADING_H */

