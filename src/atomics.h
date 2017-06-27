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
 * one instead of using it directly because,
 *
 *     1. We support GCC 4.7 and GCC add support for c11 atomics in 4.9.
 *        Luckily, the __atomic intrinsics were added in GCC 4.7.
 *     2. (most importantly) we need interoperability between code written
 *        in different languages.
 *        The current c++ standard (c++14) does not allow using c11 atomic
 *        functions or types and there's currently no guarantee that the two
 *        types are compatible (although most of them probably are).
 *        We also need to access these atomic variables from the LLVM JIT code
 *        which is very hard unless the layout of the object is fully
 *        specified.
 */
#if defined(__GNUC__)
#  define jl_signal_fence() __atomic_signal_fence(__ATOMIC_SEQ_CST)
#  define jl_atomic_fetch_add_relaxed(obj, arg)         \
    __atomic_fetch_add(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_add(obj, arg)                 \
    __atomic_fetch_add(obj, arg, __ATOMIC_SEQ_CST)
#  define jl_atomic_fetch_and_relaxed(obj, arg)         \
    __atomic_fetch_and(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_and(obj, arg)                 \
    __atomic_fetch_and(obj, arg, __ATOMIC_SEQ_CST)
#  define jl_atomic_fetch_or_relaxed(obj, arg)          \
    __atomic_fetch_or(obj, arg, __ATOMIC_RELAXED)
#  define jl_atomic_fetch_or(obj, arg)                  \
    __atomic_fetch_or(obj, arg, __ATOMIC_SEQ_CST)
// Returns the original value of `obj`
// Use the legacy __sync builtins for now, this can also be written using
// the __atomic builtins or c11 atomics with GNU extension or c11 _Generic
#  define jl_atomic_compare_exchange(obj, expected, desired)    \
    __sync_val_compare_and_swap(obj, expected, desired)
#  define jl_atomic_bool_compare_exchange(obj, expected, desired)          \
    __sync_bool_compare_and_swap(obj, expected, desired)
#  define jl_atomic_exchange(obj, desired)              \
    __atomic_exchange_n(obj, desired, __ATOMIC_SEQ_CST)
#  define jl_atomic_exchange_generic(obj, desired, orig)\
    __atomic_exchange(obj, desired, orig, __ATOMIC_SEQ_CST)
#  define jl_atomic_exchange_relaxed(obj, desired)      \
    __atomic_exchange_n(obj, desired, __ATOMIC_RELAXED)
// TODO: Maybe add jl_atomic_compare_exchange_weak for spin lock
#  define jl_atomic_store(obj, val)                     \
    __atomic_store_n(obj, val, __ATOMIC_SEQ_CST)
#  if defined(__clang__) || defined(__ICC) || defined(__INTEL_COMPILER) || \
    !(defined(_CPU_X86_) || defined(_CPU_X86_64_))
// ICC and Clang doesn't have this bug...
#    define jl_atomic_store_release(obj, val)           \
    __atomic_store_n(obj, val, __ATOMIC_RELEASE)
#  else
// Workaround a GCC bug when using store with release order by using the
// stronger version instead.
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=67458
#    define jl_atomic_store_release(obj, val) do {      \
        jl_signal_fence();                              \
        __atomic_store_n(obj, val, __ATOMIC_RELEASE);   \
    } while (0)
#  endif
#  define jl_atomic_load(obj)                   \
    __atomic_load_n(obj, __ATOMIC_SEQ_CST)
#  define jl_atomic_load_acquire(obj)           \
    __atomic_load_n(obj, __ATOMIC_ACQUIRE)
#elif defined(_COMPILER_MICROSOFT_)
#  define jl_signal_fence() _ReadWriteBarrier()

// add
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 1, T>::type
jl_atomic_fetch_add(T *obj, T2 arg)
{
    return (T)_InterlockedExchangeAdd8((volatile char*)obj, (char)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 2, T>::type
jl_atomic_fetch_add(T *obj, T2 arg)
{
    return (T)_InterlockedExchangeAdd16((volatile short*)obj, (short)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 4, T>::type
jl_atomic_fetch_add(T *obj, T2 arg)
{
    return (T)_InterlockedExchangeAdd((volatile LONG*)obj, (LONG)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 8, T>::type
jl_atomic_fetch_add(T *obj, T2 arg)
{
    return (T)_InterlockedExchangeAdd64((volatile __int64*)obj, (__int64)arg);
}
// TODO: jl_atomic_exchange_generic
#define jl_atomic_fetch_add_relaxed(obj, arg) jl_atomic_fetch_add(obj, arg)

// and
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 1, T>::type
jl_atomic_fetch_and(T *obj, T2 arg)
{
    return (T)_InterlockedAnd8((volatile char*)obj, (char)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 2, T>::type
jl_atomic_fetch_and(T *obj, T2 arg)
{
    return (T)_InterlockedAnd16((volatile short*)obj, (short)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 4, T>::type
jl_atomic_fetch_and(T *obj, T2 arg)
{
    return (T)_InterlockedAnd((volatile LONG*)obj, (LONG)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 8, T>::type
jl_atomic_fetch_and(T *obj, T2 arg)
{
    return (T)_InterlockedAnd64((volatile __int64*)obj, (__int64)arg);
}
#define jl_atomic_fetch_and_relaxed(obj, arg) jl_atomic_fetch_and(obj, arg)

// or
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 1, T>::type
jl_atomic_fetch_or(T *obj, T2 arg)
{
    return (T)_InterlockedOr8((volatile char*)obj, (char)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 2, T>::type
jl_atomic_fetch_or(T *obj, T2 arg)
{
    return (T)_InterlockedOr16((volatile short*)obj, (short)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 4, T>::type
jl_atomic_fetch_or(T *obj, T2 arg)
{
    return (T)_InterlockedOr((volatile LONG*)obj, (LONG)arg);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 8, T>::type
jl_atomic_fetch_or(T *obj, T2 arg)
{
    return (T)_InterlockedOr64((volatile __int64*)obj, (__int64)arg);
}
#define jl_atomic_fetch_or_relaxed(obj, arg) jl_atomic_fetch_or(obj, arg)

// Returns the original value of `obj`
template<typename T, typename T2, typename T3>
static inline typename std::enable_if<sizeof(T) == 1, T>::type
jl_atomic_compare_exchange(volatile T *obj, T2 expected, T3 desired)
{
    return (T)_InterlockedCompareExchange8((volatile char*)obj,
                                           (char)desired, (char)expected);
}
template<typename T, typename T2, typename T3>
static inline typename std::enable_if<sizeof(T) == 2, T>::type
jl_atomic_compare_exchange(volatile T *obj, T2 expected, T3 desired)
{
    return (T)_InterlockedCompareExchange16((volatile short*)obj,
                                            (short)desired, (short)expected);
}
template<typename T, typename T2, typename T3>
static inline typename std::enable_if<sizeof(T) == 4, T>::type
jl_atomic_compare_exchange(volatile T *obj, T2 expected, T3 desired)
{
    return (T)_InterlockedCompareExchange((volatile LONG*)obj,
                                          (LONG)desired, (LONG)expected);
}
template<typename T, typename T2, typename T3>
static inline typename std::enable_if<sizeof(T) == 8, T>::type
jl_atomic_compare_exchange(volatile T *obj, T2 expected, T3 desired)
{
    return (T)_InterlockedCompareExchange64((volatile __int64*)obj,
                                            (__int64)desired, (__int64)expected);
}
// TODO: jl_atomic_bool_compare_exchange
// atomic exchange
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 1, T>::type
jl_atomic_exchange(volatile T *obj, T2 val)
{
    return _InterlockedExchange8((volatile char*)obj, (char)val);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 2, T>::type
jl_atomic_exchange(volatile T *obj, T2 val)
{
    return _InterlockedExchange16((volatile short*)obj, (short)val);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 4, T>::type
jl_atomic_exchange(volatile T *obj, T2 val)
{
    return _InterlockedExchange((volatile LONG*)obj, (LONG)val);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 8, T>::type
jl_atomic_exchange(volatile T *obj, T2 val)
{
    return _InterlockedExchange64((volatile __int64*)obj, (__int64)val);
}
#define jl_atomic_exchange_relaxed(obj, val) jl_atomic_exchange(obj, val)
// atomic stores
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 1>::type
jl_atomic_store(volatile T *obj, T2 val)
{
    _InterlockedExchange8((volatile char*)obj, (char)val);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 2>::type
jl_atomic_store(volatile T *obj, T2 val)
{
    _InterlockedExchange16((volatile short*)obj, (short)val);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 4>::type
jl_atomic_store(volatile T *obj, T2 val)
{
    _InterlockedExchange((volatile LONG*)obj, (LONG)val);
}
template<typename T, typename T2>
static inline typename std::enable_if<sizeof(T) == 8>::type
jl_atomic_store(volatile T *obj, T2 val)
{
    _InterlockedExchange64((volatile __int64*)obj, (__int64)val);
}
template<typename T, typename T2>
static inline void jl_atomic_store_release(volatile T *obj, T2 val)
{
    jl_signal_fence();
    *obj = (T)val;
}
// atomic loads
template<typename T>
static inline T jl_atomic_load(volatile T *obj)
{
    // Trick to generate cheaper instructions compare to `_InterlockedOr`
    // Note that we don't care whether the exchange succeeded or not...
    return jl_atomic_compare_exchange(obj, T(0), T(0));
}
template<typename T>
static inline T jl_atomic_load_acquire(volatile T *obj)
{
    T val = *obj;
    jl_signal_fence();
    return val;
}
#else
#  error "No atomic operations supported."
#endif

#endif // JL_ATOMICS_H
