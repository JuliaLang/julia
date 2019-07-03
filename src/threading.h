// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef THREADING_H
#define THREADING_H

#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#include "julia.h"

#define PROFILE_JL_THREADING            0

extern jl_ptls_t *jl_all_tls_states JL_GLOBALLY_ROOTED; /* thread local storage */
extern JL_DLLEXPORT int jl_n_threads;   /* # threads we're actually using */

#ifdef JULIA_ENABLE_THREADING
typedef struct _jl_threadarg_t {
    int16_t tid;
    uv_barrier_t *barrier;
    void *arg;
} jl_threadarg_t;

// each thread must initialize its TLS
void jl_init_threadtls(int16_t tid);

// provided by a threading infrastructure
void jl_init_threadinginfra(void);
void jl_threadfun(void *arg);
#endif

#ifdef __cplusplus
}
#endif

#endif  /* THREADING_H */
