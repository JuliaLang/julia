// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef THREADING_H
#define THREADING_H

#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#include "threadgroup.h"
#include "julia.h"

#define PROFILE_JL_THREADING            1

// thread ID
extern jl_ptls_t *jl_all_tls_states;
extern JL_DLLEXPORT int jl_n_threads;  // # threads we're actually using

// thread state
enum {
    TI_THREAD_INIT,
    TI_THREAD_WORK
};

// passed to thread function
typedef struct {
    int16_t volatile state;
    int16_t          tid;
    ti_threadgroup_t *tg;
} ti_threadarg_t;

// commands to thread function
enum {
    TI_THREADWORK_DONE,
    TI_THREADWORK_RUN
};

// work command to thread function
typedef struct {
    uint8_t       command;
    jl_function_t *fun;
    jl_svec_t     *args;
    jl_value_t    *ret;
    jl_module_t   *current_module;
    size_t        world_age;
} ti_threadwork_t;

// thread function
void ti_threadfun(void *arg);

// helpers for thread function
jl_value_t *ti_runthread(jl_function_t *f, jl_svec_t *args, size_t nargs);

#ifdef __cplusplus
}
#endif

#endif  /* THREADING_H */
