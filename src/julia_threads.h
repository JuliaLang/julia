// This file is a part of Julia. License is MIT: https://julialang.org/license

// Meant to be included in <julia.h>
#ifndef JL_THREADS_H
#define JL_THREADS_H

#ifndef WITH_THIRD_PARTY_HEAP
#include "gc-tls-stock.h"
#else
// Pick the appropriate third-party implementation
#ifdef WITH_THIRD_PARTY_HEAP
#if WITH_THIRD_PARTY_HEAP == 1 // MMTk
#include "gc-tls-mmtk.h"
#endif
#endif
#endif
#include "gc-tls-common.h"
#include "julia_atomics.h"
#ifndef _OS_WINDOWS_
#include "pthread.h"
#endif
// threading ------------------------------------------------------------------

#ifdef __cplusplus
extern "C" {
#endif


JL_DLLEXPORT int16_t jl_threadid(void);
JL_DLLEXPORT int8_t jl_threadpoolid(int16_t tid) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint64_t jl_get_ptls_rng(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_set_ptls_rng(uint64_t new_seed) JL_NOTSAFEPOINT;

// JULIA_ENABLE_THREADING may be controlled by altering JULIA_THREADS in Make.user

// When running into scheduler issues, this may help provide information on the
// sequence of events that led to the issue. Normally, it is empty.
//#define JULIA_DEBUG_SLEEPWAKE(x) x
#define JULIA_DEBUG_SLEEPWAKE(x)

//  Options for task switching algorithm (in order of preference):
// JL_HAVE_ASM -- mostly setjmp
// JL_HAVE_ASM && JL_HAVE_UNW_CONTEXT -- libunwind-based
// JL_HAVE_UNW_CONTEXT -- libunwind-based
// JL_HAVE_UCONTEXT -- posix standard API, requires syscall for resume

#ifdef _OS_WINDOWS_
#define JL_HAVE_UCONTEXT
typedef win32_ucontext_t jl_stack_context_t;
typedef jl_stack_context_t _jl_ucontext_t;

#elif defined(_OS_OPENBSD_)
#define JL_HAVE_UNW_CONTEXT
#define UNW_LOCAL_ONLY
#include <libunwind.h>
typedef unw_context_t _jl_ucontext_t;
typedef struct {
    jl_jmp_buf uc_mcontext;
} jl_stack_context_t;

#else
typedef struct {
    jl_jmp_buf uc_mcontext;
} jl_stack_context_t;
#if !defined(JL_HAVE_UCONTEXT) && \
    !defined(JL_HAVE_ASM) && \
    !defined(JL_HAVE_UNW_CONTEXT)
#if (defined(_CPU_X86_64_) || defined(_CPU_X86_) || defined(_CPU_AARCH64_) ||  \
     defined(_CPU_ARM_) || defined(_CPU_PPC64_) || defined(_CPU_RISCV64_))
#define JL_HAVE_ASM
#endif
#if 0
// very slow, but more debugging
//#elif defined(_OS_DARWIN_)
//#define JL_HAVE_UNW_CONTEXT
//#elif defined(_OS_LINUX_)
//#define JL_HAVE_UNW_CONTEXT
#elif !defined(JL_HAVE_ASM)
#define JL_HAVE_UNW_CONTEXT // optimistically?
#endif
#endif

#if !defined(JL_HAVE_UNW_CONTEXT) && defined(JL_HAVE_ASM)
typedef jl_stack_context_t _jl_ucontext_t;
#endif
#pragma GCC visibility push(default)
#if defined(JL_HAVE_UNW_CONTEXT)
#define UNW_LOCAL_ONLY
#include <libunwind.h>
typedef unw_context_t _jl_ucontext_t;
#endif
#if defined(JL_HAVE_UCONTEXT)
#include <ucontext.h>
typedef ucontext_t _jl_ucontext_t;
#endif
#pragma GCC visibility pop
#endif

typedef struct {
    union {
        _jl_ucontext_t *ctx;
        jl_stack_context_t *copy_ctx;
    };
    void *stkbuf; // malloc'd memory (either copybuf or stack)
    size_t bufsz; // actual sizeof stkbuf
    unsigned int copy_stack:31; // sizeof stack for copybuf
    unsigned int started:1;
#if defined(_COMPILER_TSAN_ENABLED_)
    void *tsan_state;
#endif
#if defined(_COMPILER_ASAN_ENABLED_)
    void *asan_fake_stack;
#endif
} jl_ucontext_t;


// handle to reference an OS thread
#ifdef _OS_WINDOWS_
typedef HANDLE jl_thread_t;
#else
typedef pthread_t jl_thread_t;
#endif

struct _jl_task_t;

// Recursive spin lock
typedef struct {
    _Atomic(struct _jl_task_t*) owner;
    uint32_t count;
} jl_mutex_t;

struct _jl_bt_element_t;

// This includes all the thread local states we care about for a thread.
// Changes to TLS field types must be reflected in codegen.
#define JL_MAX_BT_SIZE 80000
typedef struct _jl_tls_states_t {
    int16_t tid;
    int8_t threadpoolid;
    uint64_t rngseed;
    _Atomic(volatile size_t *) safepoint; // may be changed to the suspend page by any thread
    _Atomic(int8_t) sleep_check_state; // read/write from foreign threads
    // Whether it is safe to execute GC at the same time.
#define JL_GC_STATE_UNSAFE 0
    // gc_state = 0 means the thread is running Julia code and is not
    //              safe to run concurrently to the GC
#define JL_GC_STATE_WAITING 1
    // gc_state = 1 means the thread is doing GC or is waiting for the GC to
    //              finish.
#define JL_GC_STATE_SAFE 2
    // gc_state = 2 means the thread is running unmanaged code that can be
    //              execute at the same time with the GC.
#define JL_GC_PARALLEL_COLLECTOR_THREAD 3
    // gc_state = 3 means the thread is a parallel collector thread (i.e. never runs Julia code)
#define JL_GC_CONCURRENT_COLLECTOR_THREAD 4
    // gc_state = 4 means the thread is a concurrent collector thread (background sweeper thread that never runs Julia code)
    _Atomic(int8_t) gc_state; // read from foreign threads
    // execution of certain certain impure
    // statements is prohibited from certain
    // callbacks (such as generated functions)
    // as it may make compilation undecidable
    int16_t in_pure_callback;
    int16_t in_finalizer;
    int16_t disable_gc;
    // Counter to disable finalizer **on the current thread**
    int finalizers_inhibited;
    jl_gc_tls_states_t gc_tls; // this is very large, and the offset of the first member is baked into codegen
    jl_gc_tls_states_common_t gc_tls_common; // common tls for both GCs
    small_arraylist_t lazily_freed_mtarraylist_buffers;
    volatile sig_atomic_t defer_signal;
    _Atomic(struct _jl_task_t*) current_task;
    struct _jl_task_t *next_task;
    struct _jl_task_t *previous_task;
    struct _jl_task_t *root_task;
    struct _jl_timing_block_t *timing_stack;
    // This is the location of our copy_stack
    void *stackbase;
    size_t stacksize;
    // Temp storage for exception thrown in signal handler. Not rooted.
    struct _jl_value_t *sig_exception;
    // Temporary backtrace buffer. Scanned for gc roots when bt_size > 0.
    struct _jl_bt_element_t *bt_data; // JL_MAX_BT_SIZE + 1 elements long
    size_t bt_size;    // Size for backtrace in transit in bt_data
    // Temporary backtrace buffer used only for allocations profiler.
    struct _jl_bt_element_t *profiling_bt_buffer;
    // Atomically set by the sender, reset by the handler.
    volatile _Atomic(sig_atomic_t) signal_request; // TODO: no actual reason for this to be _Atomic
    // Allow the sigint to be raised asynchronously
    // this is limited to the few places we do synchronous IO
    // we can make this more general (similar to defer_signal) if necessary
    volatile sig_atomic_t io_wait;
#ifdef _OS_WINDOWS_
    int needs_resetstkoflw;
#else
    void *signal_stack;
    size_t signal_stack_size;
#endif
    jl_thread_t system_id;
    _Atomic(int16_t) suspend_count;
    arraylist_t finalizers;
    // Saved exception for previous *external* API call or NULL if cleared.
    // Access via jl_exception_occurred().
    struct _jl_value_t *previous_exception;
#ifdef _OS_DARWIN_
    jl_jmp_buf *volatile safe_restore;
#endif

    // currently-held locks, to be released when an exception is thrown
    small_arraylist_t locks;
    size_t engine_nqueued;

    JULIA_DEBUG_SLEEPWAKE(
        uint64_t uv_run_enter;
        uint64_t uv_run_leave;
        uint64_t sleep_enter;
        uint64_t sleep_leave;
    )

    // some hidden state (usually just because we don't have the type's size declaration)
#ifdef JL_LIBRARY_EXPORTS
    uv_mutex_t sleep_lock;
    uv_cond_t wake_signal;
#endif
} jl_tls_states_t;

#define JL_RNG_SIZE 5 // xoshiro 4 + splitmix 1

// all values are callable as Functions
typedef jl_value_t jl_function_t;

typedef struct _jl_timing_block_t jl_timing_block_t;
typedef struct _jl_timing_event_t jl_timing_event_t;
typedef struct _jl_excstack_t jl_excstack_t;

typedef struct _jl_handler_t jl_handler_t;

typedef struct _jl_task_t {
    JL_DATA_TYPE
    jl_value_t *next; // invasive linked list for scheduler
    jl_value_t *queue; // invasive linked list for scheduler
    jl_value_t *tls;
    jl_value_t *donenotify;
    jl_value_t *result;
    jl_value_t *scope;
    jl_function_t *start;
    _Atomic(uint8_t) _state;
    uint8_t sticky; // record whether this Task can be migrated to a new thread
    uint16_t priority;
    _Atomic(uint8_t) _isexception; // set if `result` is an exception to throw or that we exited with
    uint8_t pad0[3];
    // === 64 bytes (cache line)
    uint64_t rngState[JL_RNG_SIZE];
    // flag indicating whether or not to record timing metrics for this task
    uint8_t metrics_enabled;
    uint8_t pad1[3];
    // timestamp this task first entered the run queue
    _Atomic(uint64_t) first_enqueued_at;
    // timestamp this task was most recently scheduled to run
    _Atomic(uint64_t) last_started_running_at;
    // time this task has spent running; updated when it yields or finishes.
    _Atomic(uint64_t) running_time_ns;
    // === 64 bytes (cache line)
    // timestamp this task finished (i.e. entered state DONE or FAILED).
    _Atomic(uint64_t) finished_at;

// hidden state:

    // id of owning thread - does not need to be defined until the task runs
    _Atomic(int16_t) tid;
    // threadpool id
    int8_t threadpoolid;
    // Reentrancy bits
    // Bit 0: 1 if we are currently running inference/codegen
    // Bit 1-2: 0-3 counter of how many times we've reentered inference
    // Bit 3: 1 if we are writing the image and inference is illegal
    uint8_t reentrant_timing;
    // 2 bytes of padding on 32-bit, 6 bytes on 64-bit
    // uint16_t padding2_32;
    // uint48_t padding2_64;
    // saved gc stack top for context switches
    jl_gcframe_t *gcstack;
    size_t world_age;
    // quick lookup for current ptls
    jl_ptls_t ptls; // == jl_all_tls_states[tid]
#ifdef USE_TRACY
    const char *name;
#endif
    // saved exception stack
    jl_excstack_t *excstack;
    // current exception handler
    jl_handler_t *eh;
    // saved thread state
    jl_ucontext_t ctx; // pointer into stkbuf, if suspended
} jl_task_t;

JL_DLLEXPORT void *jl_get_ptls_states(void);

// Update codegen version in `ccall.cpp` after changing either `pause` or `wake`
#ifdef __MIC__
#  define jl_cpu_pause() _mm_delay_64(100)
#  define jl_cpu_suspend() _mm_delay_64(100)
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */
#  define jl_cpu_pause() _mm_pause()
#  define jl_cpu_suspend() _mm_pause()
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
#  define jl_cpu_pause() __asm__ volatile ("isb" ::: "memory")
#  define jl_cpu_suspend() __asm__ volatile ("wfe" ::: "memory")
#  define jl_cpu_wake() __asm__ volatile ("sev" ::: "memory")
#  define JL_CPU_WAKE_NOOP 0
#else
#  define jl_cpu_pause() ((void)0)
#  define jl_cpu_suspend() ((void)0)
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#endif

JL_DLLEXPORT void (jl_cpu_pause)(void);
JL_DLLEXPORT void (jl_cpu_suspend)(void);
JL_DLLEXPORT void (jl_cpu_wake)(void);

#ifdef __clang_gcanalyzer__
// Note that the sigint safepoint can also trigger GC, albeit less likely
void jl_gc_safepoint_(jl_ptls_t tls);
void jl_sigint_safepoint(jl_ptls_t tls);
#else
// gc safepoint and gc states
// This triggers a SegFault when we are in GC
// Assign it to a variable to make sure the compiler emit the load
// and to avoid Clang warning for -Wunused-volatile-lvalue
#define jl_gc_safepoint_(ptls) do {                                            \
        jl_signal_fence();                                                     \
        size_t safepoint_load = jl_atomic_load_relaxed(&ptls->safepoint)[0];   \
        jl_signal_fence();                                                     \
        (void)safepoint_load;                                                  \
    } while (0)
#define jl_sigint_safepoint(ptls) do {                                         \
        jl_signal_fence();                                                     \
        size_t safepoint_load = jl_atomic_load_relaxed(&ptls->safepoint)[-1];  \
        jl_signal_fence();                                                     \
        (void)safepoint_load;                                                  \
    } while (0)
#endif
STATIC_INLINE int8_t jl_gc_state_set(jl_ptls_t ptls, int8_t state,
                                     int8_t old_state)
{
    assert(old_state != JL_GC_PARALLEL_COLLECTOR_THREAD);
    assert(old_state != JL_GC_CONCURRENT_COLLECTOR_THREAD);
    jl_atomic_store_release(&ptls->gc_state, state);
    if (state == JL_GC_STATE_UNSAFE || old_state == JL_GC_STATE_UNSAFE)
        jl_gc_safepoint_(ptls);
    return old_state;
}
STATIC_INLINE int8_t jl_gc_state_save_and_set(jl_ptls_t ptls,
                                              int8_t state)
{
    return jl_gc_state_set(ptls, state, jl_atomic_load_relaxed(&ptls->gc_state));
}
#ifdef __clang_gcanalyzer__
// these might not be a safepoint (if they are no-op safe=>safe transitions), but we have to assume it could be (statically)
// however mark a delineated region in which safepoints would be not permissible
int8_t jl_gc_unsafe_enter(jl_ptls_t ptls) JL_NOTSAFEPOINT_LEAVE;
void jl_gc_unsafe_leave(jl_ptls_t ptls, int8_t state) JL_NOTSAFEPOINT_ENTER;
int8_t jl_gc_safe_enter(jl_ptls_t ptls) JL_NOTSAFEPOINT_ENTER;
void jl_gc_safe_leave(jl_ptls_t ptls, int8_t state) JL_NOTSAFEPOINT_LEAVE;
#else
#define jl_gc_unsafe_enter(ptls) jl_gc_state_save_and_set(ptls, JL_GC_STATE_UNSAFE)
#define jl_gc_unsafe_leave(ptls, state) ((void)jl_gc_state_set(ptls, (state), JL_GC_STATE_UNSAFE))
#define jl_gc_safe_enter(ptls) jl_gc_state_save_and_set(ptls, JL_GC_STATE_SAFE)
#define jl_gc_safe_leave(ptls, state) ((void)jl_gc_state_set(ptls, (state), JL_GC_STATE_SAFE))
#endif

JL_DLLEXPORT void jl_gc_enable_finalizers(struct _jl_task_t *ct, int on);
JL_DLLEXPORT void jl_gc_disable_finalizers_internal(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_enable_finalizers_internal(void);
JL_DLLEXPORT void jl_gc_run_pending_finalizers(struct _jl_task_t *ct);
extern JL_DLLEXPORT _Atomic(int) jl_gc_have_pending_finalizers;
JL_DLLEXPORT int8_t jl_gc_is_in_finalizer(void) JL_NOTSAFEPOINT;

JL_DLLEXPORT void jl_wakeup_thread(int16_t tid);

JL_DLLEXPORT int jl_getaffinity(int16_t tid, char *mask, int cpumasksize);
JL_DLLEXPORT int jl_setaffinity(int16_t tid, char *mask, int cpumasksize);

#ifdef __cplusplus
}
#endif

#endif
