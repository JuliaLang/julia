// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_FASTTLS_H
#define JL_FASTTLS_H

#ifdef __cplusplus
#include <atomic>
#define _Atomic(T) std::atomic<T>
#else
#include <stdatomic.h>
#endif

// Thread-local storage access

#ifdef __cplusplus
extern "C" {
#endif

/* Bring in definitions for `_OS_X_`, `JL_PATH_MAX` and `PATHSEPSTRING`, `jl_ptls_t`, etc... */
#include "platform.h"
#include "dirpath.h"

typedef struct _jl_gcframe_t jl_gcframe_t;

typedef jl_gcframe_t **(jl_get_pgcstack_func)(void);

#if !defined(_OS_WINDOWS_)
#define JULIA_DEFINE_FAST_TLS                                                                   \
static __attribute__((tls_model("local-exec"))) __thread jl_gcframe_t **jl_pgcstack_localexec;  \
JL_DLLEXPORT _Atomic(char) jl_pgcstack_static_semaphore;                                        \
JL_DLLEXPORT jl_gcframe_t **jl_get_pgcstack_static(void)                                        \
{                                                                                               \
    return jl_pgcstack_localexec;                                                               \
}                                                                                               \
JL_DLLEXPORT jl_gcframe_t ***jl_pgcstack_addr_static(void)                                      \
{                                                                                               \
    return &jl_pgcstack_localexec;                                                              \
}
#else
#define JULIA_DEFINE_FAST_TLS
#endif

#ifdef __cplusplus
}
#endif

#endif
