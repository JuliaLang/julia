// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_ATOMICS_H
#define JL_ATOMICS_H

#if defined(__i386__) && defined(__GNUC__) && !defined(__SSE2__)
#  error Julia can only be built for architectures above Pentium 4. Pass -march=pentium4, or set MARCH=pentium4 and ensure that -march is not passed separately with an older architecture.
#endif

// Low-level atomic operations
#ifdef __cplusplus
#include <atomic>
using std::memory_order_relaxed;
using std::memory_order_consume;
using std::memory_order_acquire;
using std::memory_order_release;
using std::memory_order_acq_rel;
using std::memory_order_seq_cst;
using std::atomic_thread_fence;
using std::atomic_signal_fence;
using std::atomic_load;
using std::atomic_load_explicit;
using std::atomic_store;
using std::atomic_store_explicit;
using std::atomic_fetch_add;
using std::atomic_fetch_add_explicit;
using std::atomic_fetch_and;
using std::atomic_fetch_and_explicit;
using std::atomic_fetch_or;
using std::atomic_fetch_or_explicit;
using std::atomic_compare_exchange_strong;
using std::atomic_compare_exchange_strong_explicit;
using std::atomic_exchange;
using std::atomic_exchange_explicit;
extern "C" {
#define _Atomic(T) std::atomic<T>
#else
#include <stdatomic.h>
#endif
#include <signal.h> // for sig_atomic_t

#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#  include <immintrin.h>
#endif

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
#define jl_fence() atomic_thread_fence(memory_order_seq_cst)
#define jl_fence_release() atomic_thread_fence(memory_order_release)
#define jl_signal_fence() atomic_signal_fence(memory_order_seq_cst)

#ifdef __cplusplus
}
// implicit conversion wasn't correctly specified 2017, so many compilers get
// this wrong thus we include the correct definitions here (with implicit
// conversion), instead of using the macro version
template<class T>
T jl_atomic_load(std::atomic<T> *ptr)
{
     return std::atomic_load<T>(ptr);
}
template<class T>
T jl_atomic_load_explicit(std::atomic<T> *ptr, std::memory_order order)
{
     return std::atomic_load_explicit<T>(ptr, order);
}
#define jl_atomic_load_relaxed(ptr) jl_atomic_load_explicit(ptr, memory_order_relaxed)
#define jl_atomic_load_acquire(ptr) jl_atomic_load_explicit(ptr, memory_order_acquire)
template<class T, class S>
void jl_atomic_store(std::atomic<T> *ptr, S desired)
{
     std::atomic_store<T>(ptr, desired);
}
template<class T, class S>
void jl_atomic_store_explicit(std::atomic<T> *ptr, S desired, std::memory_order order)
{
     std::atomic_store_explicit<T>(ptr, desired, order);
}
#define jl_atomic_store_relaxed(ptr, val) jl_atomic_store_explicit(ptr, val, memory_order_relaxed)
#define jl_atomic_store_release(ptr, val) jl_atomic_store_explicit(ptr, val, memory_order_release)
template<class T, class S>
T jl_atomic_fetch_add(std::atomic<T> *ptr, S val)
{
     return std::atomic_fetch_add<T>(ptr, val);
}
template<class T, class S>
T jl_atomic_fetch_add_explicit(std::atomic<T> *ptr, S val, std::memory_order order)
{
     return std::atomic_fetch_add_explicit<T>(ptr, val, order);
}
#define jl_atomic_fetch_add_relaxed(ptr, val) jl_atomic_fetch_add_explicit(ptr, val, memory_order_relaxed)
template<class T, class S>
T jl_atomic_fetch_and(std::atomic<T> *ptr, S val)
{
     return std::atomic_fetch_and<T>(ptr, val);
}
template<class T, class S>
T jl_atomic_fetch_and_explicit(std::atomic<T> *ptr, S val, std::memory_order order)
{
     return std::atomic_fetch_and_explicit<T>(ptr, val, order);
}
#define jl_atomic_fetch_and_relaxed(ptr, val) jl_atomic_fetch_and_explicit(ptr, val, memory_order_relaxed)
template<class T, class S>
T jl_atomic_fetch_or(std::atomic<T> *ptr, S val)
{
     return std::atomic_fetch_or<T>(ptr, val);
}
template<class T, class S>
T jl_atomic_fetch_or_explicit(std::atomic<T> *ptr, S val, std::memory_order order)
{
     return std::atomic_fetch_or_explicit<T>(ptr, val, order);
}
#define jl_atomic_fetch_or_relaxed(ptr, val) jl_atomic_fetch_or_explicit(ptr, val, memory_order_relaxed)
template<class T, class S>
bool jl_atomic_cmpswap(std::atomic<T> *ptr, T *expected, S val)
{
     return std::atomic_compare_exchange_strong<T>(ptr, expected, val);
}
template<class T, class S>
bool jl_atomic_cmpswap_explicit(std::atomic<T> *ptr, T *expected, S val, std::memory_order order)
{
     return std::atomic_compare_exchange_strong_explicit<T>(ptr, expected, val, order, order);
}
#define jl_atomic_cmpswap_relaxed(ptr, val) jl_atomic_cmpswap_explicit(ptr, val, memory_order_relaxed)
template<class T, class S>
T jl_atomic_exchange(std::atomic<T> *ptr, S desired)
{
     return std::atomic_exchange<T>(ptr, desired);
}
template<class T, class S>
T jl_atomic_exchange_explicit(std::atomic<T> *ptr, S desired, std::memory_order order)
{
     return std::atomic_exchange_explicit<T>(ptr, desired, order);
}
#define jl_atomic_exchange_relaxed(ptr, val) jl_atomic_exchange_explicit(ptr, val, memory_order_relaxed)
extern "C" {
#else

#  define jl_atomic_fetch_add_relaxed(obj, arg)         \
    atomic_fetch_add_explicit(obj, arg, memory_order_relaxed)
#  define jl_atomic_fetch_add(obj, arg)                 \
    atomic_fetch_add(obj, arg)
#  define jl_atomic_fetch_and_relaxed(obj, arg)         \
    atomic_fetch_and_explicit(obj, arg, memory_order_relaxed)
#  define jl_atomic_fetch_and(obj, arg)                 \
    atomic_fetch_and(obj, arg)
#  define jl_atomic_fetch_or_relaxed(obj, arg)          \
    atomic_fetch_or_explicit(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_or(obj, arg)                  \
    atomic_fetch_or(obj, arg)
#  define jl_atomic_cmpswap(obj, expected, desired)     \
    atomic_compare_exchange_strong(obj, expected, desired)
#  define jl_atomic_cmpswap_relaxed(obj, expected, desired) \
    atomic_compare_exchange_strong_explicit(obj, expected, desired, memory_order_relaxed, memory_order_relaxed)
// TODO: Maybe add jl_atomic_cmpswap_weak for spin lock
#  define jl_atomic_exchange(obj, desired)       \
    atomic_exchange(obj, desired)
#  define jl_atomic_exchange_relaxed(obj, desired)      \
    atomic_exchange_explicit(obj, desired, memory_order_relaxed)
#  define jl_atomic_store(obj, val)                     \
    atomic_store(obj, val)
#  define jl_atomic_store_relaxed(obj, val)             \
    atomic_store_explicit(obj, val, memory_order_relaxed)

#  if defined(__clang__) || defined(__ICC) || defined(__INTEL_COMPILER) || \
    !(defined(_CPU_X86_) || defined(_CPU_X86_64_))
// ICC and Clang doesn't have this bug...
#    define jl_atomic_store_release(obj, val)           \
    atomic_store_explicit(obj, val, memory_order_release)
#  else
// Workaround a GCC bug when using store with release order by using the
// stronger version instead.
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=67458
// fixed in https://gcc.gnu.org/git/?p=gcc.git&a=commit;h=d8c40eff56f69877b33c697ded756d50fde90c27
#    define jl_atomic_store_release(obj, val) do {      \
        jl_signal_fence();                              \
        atomic_store_explicit(obj, val, memory_order_release);   \
    } while (0)
#  endif
#  define jl_atomic_load(obj)                   \
    atomic_load(obj)
#  define jl_atomic_load_acquire(obj)           \
    atomic_load_explicit(obj, memory_order_acquire)
#ifdef _COMPILER_TSAN_ENABLED_
// For the sake of tsan, call these loads consume ordering since they will act
// as such on the processors we support while normally, the compiler would
// upgrade this to acquire ordering, which is strong (and slower) than we want.
#  define jl_atomic_load_relaxed(obj)           \
    atomic_load_explicit(obj, memory_order_consume)
#else
#  define jl_atomic_load_relaxed(obj)           \
    atomic_load_explicit(obj, memory_order_relaxed)
#endif
#endif

#ifdef __clang_gcanalyzer__
// for the purposes of the GC analyzer, we can turn these into non-atomic
// expressions with similar properties (for the sake of the analyzer, we don't
// care if it is an exact match for behavior)

#undef _Atomic
#define _Atomic(T) T

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
            int eq__analyzer__ = memcmp(&temp__analyzer__, x__analyzer__, sizeof(temp__analyzer__)) == 0; \
            if (eq__analyzer__) \
                *p__analyzer__ = (desired); \
            else \
                *x__analyzer__ = temp__analyzer__; \
            eq__analyzer__; \
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

#undef jl_atomic_fetch_add
#undef jl_atomic_fetch_and
#undef jl_atomic_fetch_or
#undef jl_atomic_fetch_add_relaxed
#undef jl_atomic_fetch_and_relaxed
#undef jl_atomic_fetch_or_relaxed
#define jl_atomic_fetch_add(obj, val) \
    (__extension__({ \
            __typeof__((obj)) p__analyzer__ = (obj); \
            __typeof__(*p__analyzer__) temp__analyzer__ = *p__analyzer__; \
            *(p__analyzer__) = temp__analyzer__ + (val); \
            temp__analyzer__; \
        }))
#define jl_atomic_fetch_and(obj, val) \
    (__extension__({ \
            __typeof__((obj)) p__analyzer__ = (obj); \
            __typeof__(*p__analyzer__) temp__analyzer__ = *p__analyzer__; \
            *(p__analyzer__) = temp__analyzer__ & (val); \
            temp__analyzer__; \
        }))
#define jl_atomic_fetch_or(obj, val) \
    (__extension__({ \
            __typeof__((obj)) p__analyzer__ = (obj); \
            __typeof__(*p__analyzer__) temp__analyzer__ = *p__analyzer__; \
            *(p__analyzer__) = temp__analyzer__ | (val); \
            temp__analyzer__; \
        }))
#define jl_atomic_fetch_add_relaxed jl_atomic_fetch_add
#define jl_atomic_fetch_and_relaxed jl_atomic_fetch_and
#define jl_atomic_fetch_or_relaxed jl_atomic_fetch_or

#endif


#ifdef __cplusplus
}
#endif

#endif // JL_ATOMICS_H
