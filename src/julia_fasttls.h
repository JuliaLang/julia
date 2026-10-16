// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_FASTTLS_H
#define JL_FASTTLS_H

#ifdef __cplusplus
#include <atomic>
#define _Atomic(T) std::atomic<T>
#else
#include <stdatomic.h>
#endif
#include "analyzer_annotations.h"
#include <stdint.h>


// Thread-local storage access

#ifdef __cplusplus
extern "C" {
#endif

/* Bring in definitions for `_OS_X_`, `JL_PATH_MAX` and `PATHSEPSTRING`, `jl_ptls_t`, etc... */
#include "platform.h"
#include "dirpath.h"

typedef struct _jl_gcframe_t jl_gcframe_t;

typedef jl_gcframe_t **(*jl_get_pgcstack_func_t)(void) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;

#if !defined(_OS_WINDOWS_) && !defined(_OS_DARWIN_)
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

// `jl_tls_load(offset)` loads the pointer `offset` bytes from the thread
// register. Read at `jl_tls_pgcstack_offset`, it gives `jl_get_pgcstack()`;
// `jl_check_tls` publishes that offset only if such a read finds the slot.
//
// The asm is neither volatile nor clobbers memory, so the compiler may merge
// or hoist it like a call to a `const` function. The frame-stack slot allows
// that: it holds the current task's `&gcstack`, which does not change within
// a C function. Reading the thread register on its own would not: a task can
// resume on another thread.
#if (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_X86_64_)
#define JL_TLS_LOAD_ASM "movq %%fs:(%1), %0"
#elif (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_X86_)
#define JL_TLS_LOAD_ASM "movl %%gs:(%1), %0"
#elif (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_AARCH64_)
#define JL_TLS_LOAD_ASM "mrs %0, tpidr_el0\n\tldr %0, [%0, %1]"
#elif defined(_OS_DARWIN_) && defined(_CPU_X86_64_)
#define JL_TLS_LOAD_ASM "movq %%gs:(%1), %0"
#elif defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
// `pthread_getspecific` reads TPIDRRO_EL0 unmasked; the mask, as in Go's
// `load_g`, guards against kernels that keep the CPU number in its low bits
#define JL_TLS_LOAD_ASM "mrs %0, tpidrro_el0\n\tand %0, %0, #-8\n\tldr %0, [%0, %1]"
#endif

#ifdef JL_TLS_LOAD_ASM
static inline void *jl_tls_load(intptr_t offset) JL_NOTSAFEPOINT
{
    void *p;
    __asm__(JL_TLS_LOAD_ASM : "=&r"(p) : "r"(offset));
    return p;
}
#endif

#ifdef __cplusplus
}
#endif

#endif
