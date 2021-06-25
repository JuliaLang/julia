// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_ATOMICS_H
#define JL_ATOMICS_H

// Low-level atomic operations

#if defined(__i386__) && defined(__GNUC__) && !defined(__SSE2__)
#  error Julia can only be built for architectures above Pentium 4. Pass -march=pentium4, or set MARCH=pentium4 and ensure that -march is not passed separately with an older architecture.
#endif
#ifdef _COMPILER_MICROSOFT_
#  include <intrin.h>
#  include <type_traits>
#endif
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#  include <immintrin.h>
#endif
#ifndef _OS_WINDOWS_
#  include <pthread.h>
#endif
#include <signal.h>

enum jl_memory_order {
    jl_memory_order_unspecified = -2,
    jl_memory_order_invalid = -1,
    jl_memory_order_notatomic = 0,
    jl_memory_order_unordered,
    jl_memory_order_monotonic,
    jl_memory_order_consume,
    jl_memory_order_acquire,
    jl_memory_order_release,
    jl_memory_order_acq_rel,
    jl_memory_order_seq_cst
};

/**
 * Thread synchronization primitives:
 *
 * These roughly follows the c11/c++11 memory model and the act as memory
 * barriers at both the compiler level and the hardware level.
 * The only exception is the GC safepoint and GC state transitions for which
 * we use only a compiler (signal) barrier and use the signal handler to do the
 * synchronization in order to lower the mutator overhead as much as possible.
 *
 * We use the compiler intrinsics to implement a similar API to the c11/c++11
 * one instead of using it directly because, we need interoperability between
 * code written in different languages. The current c++ standard (c++14) does
 * not allow using c11 atomic functions or types and there's currently no
 * guarantee that the two types are compatible (although most of them probably
 * are). We also need to access these atomic variables from the LLVM JIT code
 * which is very hard unless the layout of the object is fully specified.
 */
#define jl_fence() __atomic_thread_fence(__ATOMIC_SEQ_CST)
#define jl_fence_release() __atomic_thread_fence(__ATOMIC_RELEASE)
#define jl_signal_fence() __atomic_signal_fence(__ATOMIC_SEQ_CST)


#  define jl_atomic_fetch_add_relaxed(obj, arg)         \
    __atomic_fetch_add(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_add(obj, arg)                 \
    __atomic_fetch_add(obj, arg, __ATOMIC_SEQ_CST)
#  define jl_atomic_add_fetch(obj, arg)                 \
    __atomic_add_fetch(obj, arg, __ATOMIC_SEQ_CST)
#  define jl_atomic_fetch_and_relaxed(obj, arg)         \
    __atomic_fetch_and(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_and(obj, arg)                 \
    __atomic_fetch_and(obj, arg, __ATOMIC_SEQ_CST)
#  define jl_atomic_fetch_or_relaxed(obj, arg)          \
    __atomic_fetch_or(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_or(obj, arg)                  \
    __atomic_fetch_or(obj, arg, __ATOMIC_SEQ_CST)
#  define jl_atomic_cmpswap(obj, expected, desired)    \
    __atomic_compare_exchange_n(obj, expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
#  define jl_atomic_cmpswap_relaxed(obj, expected, desired)    \
    __atomic_compare_exchange_n(obj, expected, desired, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED)
// TODO: Maybe add jl_atomic_cmpswap_weak for spin lock
#  define jl_atomic_exchange(obj, desired)              \
    __atomic_exchange_n(obj, desired, __ATOMIC_SEQ_CST)
#  define jl_atomic_exchange_relaxed(obj, desired)      \
    __atomic_exchange_n(obj, desired, __ATOMIC_RELAXED)
#  define jl_atomic_store(obj, val)                     \
    __atomic_store_n(obj, val, __ATOMIC_SEQ_CST)
#  define jl_atomic_store_relaxed(obj, val)             \
    __atomic_store_n(obj, val, __ATOMIC_RELAXED)

#  if defined(__clang__) || defined(__ICC) || defined(__INTEL_COMPILER) || \
    !(defined(_CPU_X86_) || defined(_CPU_X86_64_))
// ICC and Clang doesn't have this bug...
#    define jl_atomic_store_release(obj, val)           \
    __atomic_store_n(obj, val, __ATOMIC_RELEASE)
#  else
// Workaround a GCC bug when using store with release order by using the
// stronger version instead.
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=67458
// fixed in https://gcc.gnu.org/git/?p=gcc.git&a=commit;h=d8c40eff56f69877b33c697ded756d50fde90c27
#    define jl_atomic_store_release(obj, val) do {      \
        jl_signal_fence();                              \
        __atomic_store_n(obj, val, __ATOMIC_RELEASE);   \
    } while (0)
#  endif
#  define jl_atomic_load(obj)                   \
    __atomic_load_n(obj, __ATOMIC_SEQ_CST)
#  define jl_atomic_load_acquire(obj)           \
    __atomic_load_n(obj, __ATOMIC_ACQUIRE)
#ifdef JL_TSAN_ENABLED
// For the sake of tsan, call these loads consume ordering since they will act
// as such on the processors we support while normally, the compiler would
// upgrade this to acquire ordering, which is strong (and slower) than we want.
#  define jl_atomic_load_relaxed(obj)           \
    __atomic_load_n(obj, __ATOMIC_CONSUME)
#else
#  define jl_atomic_load_relaxed(obj)           \
    __atomic_load_n(obj, __ATOMIC_RELAXED)
#endif

#ifdef __clang_analyzer__
// for the purposes of the analyzer, we can turn these into non-atomic expressions with similar properties
// (for the sake of the analyzer, we don't care if it is an exact match for behavior)

#undef jl_atomic_exchange
#undef jl_atomic_exchange_relaxed
#define jl_atomic_exchange(obj, desired) \
    (__extension__({ \
            __typeof__((obj)) p__analyzer__ = (obj); \
            __typeof__(*p__analyzer__) temp__analyzer__ = *p__analyzer__; \
            *p__analyzer__ = (desired); \
            temp__analyzer__; \
        }))
#define jl_atomic_exchange_relaxed jl_atomic_exchange

#undef jl_atomic_cmpswap
#undef jl_atomic_cmpswap_relaxed
#define jl_atomic_cmpswap(obj, expected, desired) \
    (__extension__({ \
            __typeof__((obj)) p__analyzer__ = (obj); \
            __typeof__(*p__analyzer__) temp__analyzer__ = *p__analyzer__; \
            __typeof__((expected)) x__analyzer__ = (expected); \
            if (temp__analyzer__ == *x__analyzer__) \
                *p__analyzer__ = (desired); \
            else \
                *x__analyzer__ = temp__analyzer__; \
            temp__analyzer__ == *x__analyzer__; \
        }))
#define jl_atomic_cmpswap_relaxed jl_atomic_cmpswap

#undef jl_atomic_store
#undef jl_atomic_store_release
#undef jl_atomic_store_relaxed
#define jl_atomic_store(obj, val)         (*(obj) = (val))
#define jl_atomic_store_release jl_atomic_store
#define jl_atomic_store_relaxed jl_atomic_store

#undef jl_atomic_load
#undef jl_atomic_load_acquire
#undef jl_atomic_load_relaxed
#define jl_atomic_load(obj)         (*(obj))
#define jl_atomic_load_acquire jl_atomic_load
#define jl_atomic_load_relaxed jl_atomic_load

#endif


#endif // JL_ATOMICS_H
