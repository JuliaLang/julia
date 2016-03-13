// This file is a part of Julia. License is MIT: http://julialang.org/license

// Meant to be included in <julia.h>
#ifndef JULIA_THREADS_H
#define JULIA_THREADS_H

// threading ------------------------------------------------------------------

// WARNING: Threading support is incomplete and experimental
// Nonetheless, we define JL_THREAD and use it to give advanced notice to
// maintainers of what eventual threading support will change.

// JULIA_ENABLE_THREADING is switched on in Make.inc if JULIA_THREADS is
// set (in Make.user)

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

// This includes all the thread local states we care about for a thread.
#define JL_MAX_BT_SIZE 80000
typedef struct _jl_tls_states_t {
    struct _jl_gcframe_t *pgcstack;
    struct _jl_value_t *exception_in_transit;
    // Whether it is safe to execute GC at the same time.
#define JL_GC_STATE_WAITING 1
    // gc_state = 1 means the thread is doing GC or is waiting for the GC to
    //              finish.
#define JL_GC_STATE_SAFE 2
    // gc_state = 2 means the thread is running unmanaged code that can be
    //              execute at the same time with the GC.
    volatile int8_t gc_state;
    volatile int8_t in_finalizer;
    int8_t disable_gc;
    struct _jl_thread_heap_t *heap;
    struct _jl_module_t *current_module;
    struct _jl_task_t *volatile current_task;
    struct _jl_task_t *root_task;
    struct _jl_value_t *volatile task_arg_in_transit;
    void *stackbase;
    char *stack_lo;
    char *stack_hi;
    jl_jmp_buf *volatile jmp_target;
    jl_jmp_buf base_ctx; // base context of stack
    jl_jmp_buf *safe_restore;
    int16_t tid;
    size_t bt_size;
    uintptr_t bt_data[JL_MAX_BT_SIZE + 1];
} jl_tls_states_t;

#ifdef __MIC__
#  define jl_cpu_pause() _mm_delay_64(100)
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */
#  define jl_cpu_pause() _mm_pause()
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
#  define jl_cpu_pause() __asm__ volatile ("wfe" ::: "memory")
#  define jl_cpu_wake() __asm__ volatile ("sev" ::: "memory")
#  define JL_CPU_WAKE_NOOP 0
#else
#  define jl_cpu_pause() ((void)0)
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#endif

/**
 * Thread synchronization primitives:
 *
 * These roughly follows the c11/c++11 memory model and the act as memory
 * barriers at both the compiler level and the hardware level.
 * The only exception is the GC safepoint and GC state transitions for which
 * we use only a compiler (signal) barrier and use the signal handler to do the
 * synchronization in order to lower the mutator overhead as much as possible.
 *
 * We use the compiler instrinsics to implement a similar API to the c11/c++11
 * one instead of using it directly because,
 *
 *     1. We support GCC 4.7 and GCC add support for c11 atomics in 4.9.
 *        Luckily, the __atomic intrinsics were added in GCC 4.7.
 *     2. (most importantly) we need interoperability between code written
 *        in different languages.
 *        The current c++ standard (c++14) does not allow using c11 atomic
 *        functions or types and there's currently no grantee that the two
 *        types are compatible (although most of them probably are).
 *        We also need to access these atomic variables from the LLVM JIT code
 *        which is very hard unless the layout of the object is fully
 *        specified.
 */
#if defined(__GNUC__)
#  define jl_signal_fence() __atomic_signal_fence(__ATOMIC_SEQ_CST)
#  define jl_atomic_fetch_add(obj, arg)                 \
    __atomic_fetch_add(obj, arg, __ATOMIC_SEQ_CST)
// Returns the original value of `obj`
// Use the legacy __sync builtins for now, this can also be written using
// the __atomic builtins or c11 atomics with GNU extension or c11 _Generic
#  define jl_atomic_compare_exchange(obj, expected, desired)    \
    __sync_val_compare_and_swap(obj, expected, desired)
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
#    define jl_atomic_store_release(obj, val) jl_atomic_store(obj, val)
#  endif
#  define jl_atomic_load(obj)                   \
    __atomic_load_n(obj, __ATOMIC_SEQ_CST)
#  define jl_atomic_load_acquire(obj)           \
    __atomic_load_n(obj, __ATOMIC_ACQUIRE)
#elif defined(_COMPILER_MICROSOFT_)
#  define jl_signal_fence() _ReadWriteBarrier()
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

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int16_t jl_threadid(void);
JL_DLLEXPORT void *jl_threadgroup(void);
JL_DLLEXPORT void jl_threading_profile(void);
JL_DLLEXPORT void (jl_cpu_pause)(void);
JL_DLLEXPORT void (jl_cpu_wake)(void);

// Accessing the tls variables, gc safepoint and gc states
JL_DLLEXPORT JL_CONST_FUNC jl_tls_states_t *(jl_get_ptls_states)(void);
#ifndef JULIA_ENABLE_THREADING
extern JL_DLLEXPORT jl_tls_states_t jl_tls_states;
#define jl_get_ptls_states() (&jl_tls_states)
#define jl_gc_state() ((int8_t)0)
#define jl_gc_safepoint()
STATIC_INLINE int8_t jl_gc_state_set(int8_t state, int8_t old_state)
{
    (void)state;
    return old_state;
}
#else // ifndef JULIA_ENABLE_THREADING
typedef jl_tls_states_t *(*jl_get_ptls_states_func)(void);
JL_DLLEXPORT void jl_set_ptls_states_getter(jl_get_ptls_states_func f);
// Make sure jl_gc_state() is always a rvalue
#define jl_gc_state() ((int8_t)(jl_get_ptls_states()->gc_state))
JL_DLLEXPORT extern volatile size_t *jl_gc_signal_page;
STATIC_INLINE void jl_gc_safepoint(void)
{
    // This triggers a SegFault when we are in GC
    // Assign it to a variable to make sure the compiler emit the load
    // and to avoid Clang warning for -Wunused-volatile-lvalue
    jl_signal_fence();
    size_t v = *jl_gc_signal_page;
    jl_signal_fence();
    (void)v;
}
STATIC_INLINE int8_t jl_gc_state_set(int8_t state, int8_t old_state)
{
    jl_get_ptls_states()->gc_state = state;
    // A safe point is required if we transition from GC-safe region to
    // non GC-safe region.
    if (old_state && !state)
        jl_gc_safepoint();
    return old_state;
}
#endif // ifndef JULIA_ENABLE_THREADING
STATIC_INLINE int8_t jl_gc_state_save_and_set(int8_t state)
{
    return jl_gc_state_set(state, jl_gc_state());
}
#define jl_gc_unsafe_enter() jl_gc_state_save_and_set(0)
#define jl_gc_unsafe_leave(state) ((void)jl_gc_state_set((state), 0))
#define jl_gc_safe_enter() jl_gc_state_save_and_set(JL_GC_STATE_SAFE)
#define jl_gc_safe_leave(state) ((void)jl_gc_state_set((state), JL_GC_STATE_SAFE))

// Locks
#ifdef JULIA_ENABLE_THREADING
#define JL_DEFINE_MUTEX(m)                                                \
    uint64_t volatile m ## _mutex = 0;                                    \
    int32_t m ## _lock_count = 0;                                         \
    void jl_unlock_## m ## _func(void)                                    \
    {                                                                     \
        JL_UNLOCK_RAW(m);                                                 \
    }

#define JL_DEFINE_MUTEX_EXT(m)                                            \
    extern uint64_t volatile m ## _mutex;                                 \
    extern int32_t m ## _lock_count;                                      \
    void jl_unlock_## m ## _func(void);

#define JL_LOCK_WAIT(m, wait_ex) do {                                   \
        if (m ## _mutex == uv_thread_self()) {                          \
            ++m ## _lock_count;                                         \
        }                                                               \
        else {                                                          \
            for (;;) {                                                  \
                if (m ## _mutex == 0 &&                                 \
                    jl_atomic_compare_exchange(&m ## _mutex, 0,         \
                                               uv_thread_self()) == 0) { \
                    m ## _lock_count = 1;                               \
                    break;                                              \
                }                                                       \
                wait_ex;                                                \
                jl_cpu_pause();                                         \
            }                                                           \
        }                                                               \
    } while (0)

#define JL_UNLOCK_RAW(m) do {                                           \
        if (m ## _mutex == uv_thread_self()) {                          \
            --m ## _lock_count;                                         \
            if (m ## _lock_count == 0) {                                \
                jl_atomic_compare_exchange(&m ## _mutex, uv_thread_self(), 0); \
                jl_cpu_wake();                                          \
            }                                                           \
        }                                                               \
        else {                                                          \
            assert(0 && "Unlocking a lock in a different thread.");     \
        }                                                               \
    } while (0)

// JL_LOCK is a GC safe point while JL_LOCK_NOGC is not
// Always use JL_LOCK unless no one holding the lock can trigger a GC or GC
// safepoint. JL_LOCK_NOGC should only be needed for GC internal locks.
#define JL_LOCK(m) do {                                 \
        JL_LOCK_WAIT(m, jl_gc_safepoint());             \
        jl_lock_frame_push(jl_unlock_## m ## _func);    \
    } while (0)
#define JL_UNLOCK(m) do {                       \
        JL_UNLOCK_RAW(m);                       \
        if (__likely(jl_current_task))          \
            jl_current_task->locks.len--;       \
    } while (0)
#define JL_LOCK_NOGC(m) JL_LOCK_WAIT(m, )
#define JL_UNLOCK_NOGC(m) JL_UNLOCK_RAW(m)
#else // JULIA_ENABLE_THREADING
#define JL_DEFINE_MUTEX(m)
#define JL_DEFINE_MUTEX_EXT(m)
#define JL_LOCK(m) do {} while (0)
#define JL_UNLOCK(m) do {} while (0)
#define JL_LOCK_NOGC(m) do {} while (0)
#define JL_UNLOCK_NOGC(m) do {} while (0)
#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif

#endif
