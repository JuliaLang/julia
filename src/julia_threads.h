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

#define JL_MAX_BT_SIZE 80000
// Define this struct early so that we are free to use it in the inline
// functions below.
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
    int8_t in_jl_;
    int16_t tid;
    size_t bt_size;
    intptr_t bt_data[JL_MAX_BT_SIZE + 1];
} jl_tls_states_t;

#ifdef _COMPILER_MICROSOFT_
#  include <intrin.h>
#  define jl_signal_fence() _ReadWriteBarrier()
#else
#  define jl_signal_fence() __atomic_signal_fence(__ATOMIC_SEQ_CST)
#endif

#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#  include <immintrin.h>
#endif

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

#if defined(__GNUC__)
#  define JL_ATOMIC_FETCH_AND_ADD(a, b) __sync_fetch_and_add(a, b)
// Returns the original value of `a`
#  define JL_ATOMIC_COMPARE_AND_SWAP(a, b, c)   \
    __sync_val_compare_and_swap(a, b, c)
#elif defined(_COMPILER_MICROSOFT_)
#  define JL_ATOMIC_FETCH_AND_ADD(a, b)                 \
    _InterlockedExchangeAdd((volatile LONG*)(a), b)
// Returns the original value of `a`
#  define JL_ATOMIC_COMPARE_AND_SWAP(a, b, c)                   \
    _InterlockedCompareExchange64((volatile LONG64*)(a), c, b)
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

#ifdef JULIA_ENABLE_THREADING
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
                    JL_ATOMIC_COMPARE_AND_SWAP(&m ## _mutex, 0,         \
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
                JL_ATOMIC_COMPARE_AND_SWAP(&m ## _mutex, uv_thread_self(), 0); \
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
#define jl_gc_safepoint()
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
