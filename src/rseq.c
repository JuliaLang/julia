// This file is a part of Julia. License is MIT: https://julialang.org/license

// Restartable-sequence (rseq(2)) registration and feature detection; see rseq.h for
// an overview and src/signals-unix.c for the paired RSEQ-flavored membarrier.

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_LINUX_)

#include <errno.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifndef __NR_rseq
#  if defined(_CPU_X86_64_)
#    define __NR_rseq 334
#  elif defined(_CPU_AARCH64_)
#    define __NR_rseq 293
#  endif
#endif

// glibc >= 2.35 registers an rseq area for every thread and exports its location
// relative to the thread pointer. Declared weak so binaries built against older
// glibc (as the official binaries are) still resolve them when *running* on a
// modern one, and see NULL otherwise.
extern ptrdiff_t __rseq_offset __attribute__((weak));
extern unsigned int __rseq_size __attribute__((weak));

static void *jl_thread_pointer(void) JL_NOTSAFEPOINT
{
#if defined(__clang__) || (defined(__GNUC__) && __GNUC__ >= 11)
    return __builtin_thread_pointer();
#elif defined(_CPU_X86_64_)
    void *tp;
    __asm__ ("movq %%fs:0, %0" : "=r" (tp));
    return tp;
#elif defined(_CPU_AARCH64_)
    void *tp;
    __asm__ ("mrs %0, tpidr_el0" : "=r" (tp));
    return tp;
#else
    return NULL;
#endif
}

#if defined(_CPU_AARCH64_)
#include <sys/auxv.h>
#ifndef HWCAP_ATOMICS
#define HWCAP_ATOMICS (1 << 8)
#endif
// LSE single-instruction atomics (swpal/casal), required by the RMW critical
// sections (see jl_rseq_have_rmw in rseq.h). Resolved with the process-wide gate
// below, before any thread registers.
int jl_rseq_lse = 0;
#endif

// 0 = disabled, 1 = enabled, -1 = not yet decided
static _Atomic(int) rseq_enabled = -1;

// Process-wide gate, decided once (on the first thread to initialize): the
// JULIA_RSEQ=0 escape hatch, an architecture with critical-section support, kernel
// rseq, and the RSEQ-flavored expedited membarrier (without which an in-flight
// critical section could not be fenced, see jl_retype_flag_partitions).
int jl_rseq_process_enabled(void) JL_NOTSAFEPOINT
{
    int enabled = jl_atomic_load_relaxed(&rseq_enabled);
    if (__likely(enabled >= 0))
        return enabled;
    enabled = 1;
    char *env = getenv("JULIA_RSEQ");
    if (env && env[0] == '0' && env[1] == '\0')
        enabled = 0;
#ifndef JL_HAVE_RSEQ_CS
    enabled = 0; // no critical-section support for this architecture yet
#endif
#ifndef __NR_rseq
    enabled = 0;
#endif
    if (enabled && jl_membarrier_rseq() != 0)
        enabled = 0;
#if defined(_CPU_AARCH64_)
    jl_rseq_lse = (getauxval(AT_HWCAP) & HWCAP_ATOMICS) != 0;
#endif
    jl_atomic_store_relaxed(&rseq_enabled, enabled);
    return enabled;
}

void jl_rseq_init_thread(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    ptls->rseq = NULL;
    if (!jl_rseq_process_enabled())
        return;
#ifdef __NR_rseq
    // Prefer the area glibc already registered for this thread (registering a
    // second one is an error). glibc uses the same canonical signature
    // (JL_RSEQ_SIG) as our abort blocks.
    if (&__rseq_size != NULL && __rseq_size > 0) {
        void *tp = jl_thread_pointer();
        if (tp != NULL) {
            ptls->rseq = (jl_rseq_t*)((char*)tp + __rseq_offset);
            return;
        }
    }
    // Self-register the ptls-embedded area, which stays valid for the OS thread's
    // whole lifetime (the kernel unregisters automatically at thread exit; ptls is
    // retired but never freed before then).
    if (syscall(__NR_rseq, &ptls->rseq_storage, (uint32_t)sizeof(jl_rseq_t), 0,
                JL_RSEQ_SIG) == 0)
        ptls->rseq = &ptls->rseq_storage;
    // any failure (old kernel, seccomp, unexpected prior registration) leaves
    // ptls->rseq NULL and this thread on the fallback protocol
#endif
}

JL_DLLEXPORT int jl_rseq_available(void) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_get_current_task();
    return ct != NULL && ct->ptls != NULL && ct->ptls->rseq != NULL;
}

#else // !_OS_LINUX_

JL_DLLEXPORT int jl_rseq_available(void) JL_NOTSAFEPOINT
{
    return 0;
}

#endif

#ifdef __cplusplus
}
#endif
