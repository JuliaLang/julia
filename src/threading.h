// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef THREADING_H
#define THREADING_H

#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#include "julia.h"

#define PROFILE_JL_THREADING            0

extern jl_ptls_t *jl_all_tls_states;    /* thread local storage */
extern JL_DLLEXPORT int jl_n_threads;   /* # threads we're actually using */

typedef struct _jl_threadarg_t {
    int16_t tid;
    uv_barrier_t *barrier;
    void *arg;
} jl_threadarg_t;

// each thread must initialize its TLS
void jl_init_threadtls(int16_t tid);

// generic helper for a thread to run a function
jl_value_t *jl_thread_run_fun(jl_callptr_t fptr, jl_method_instance_t *mfunc,
                              jl_value_t **args, uint32_t nargs);

// provided by a threading infrastructure
void jl_init_threadinginfra(void);
void jl_init_threadarg(jl_threadarg_t *targ);
void jl_init_started_threads(jl_threadarg_t **targs);
void jl_threadfun(void *arg);

// interfaces defined by threading infrastructures
#ifdef JULIA_ENABLE_FORKJOIN_TI
#include "forkjoin-ti.h"
#else
#ifdef JULIA_ENABLE_PARTR
#include "partr.h"
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif  /* THREADING_H */
