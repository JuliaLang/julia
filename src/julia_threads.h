// This file is a part of Julia. License is MIT: https://julialang.org/license

// Meant to be included in <julia.h>
#ifndef JL_THREADS_H
#define JL_THREADS_H

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

// JULIA_ENABLE_THREADING may be controlled by altering JULIA_THREADS in Make.user

// When running into scheduler issues, this may help provide information on the
// sequence of events that led to the issue. Normally, it is empty.
//#define JULIA_DEBUG_SLEEPWAKE(x) x
#define JULIA_DEBUG_SLEEPWAKE(x)

//  Options for task switching algorithm (in order of preference):
// JL_HAVE_ASM -- mostly setjmp
// JL_HAVE_ASM && JL_HAVE_UNW_CONTEXT -- libunwind-based
// JL_HAVE_UNW_CONTEXT -- libunwind-based
// JL_HAVE_ASYNCIFY -- task switching based on the binary asyncify transform
// JL_HAVE_UCONTEXT -- posix standard API, requires syscall for resume
// JL_HAVE_SIGALTSTACK -- requires several syscall for start, setjmp for resume

#ifdef _OS_WINDOWS_
#define JL_HAVE_UCONTEXT
typedef win32_ucontext_t jl_stack_context_t;
typedef jl_stack_context_t _jl_ucontext_t;
#else
typedef struct {
    jl_jmp_buf uc_mcontext;
} jl_stack_context_t;
#if !defined(JL_HAVE_UCONTEXT) && \
    !defined(JL_HAVE_ASM) && \
    !defined(JL_HAVE_UNW_CONTEXT) && \
    !defined(JL_HAVE_SIGALTSTACK) && \
    !defined(JL_HAVE_ASYNCIFY)
#if (defined(_CPU_X86_64_) || defined(_CPU_X86_) || defined(_CPU_AARCH64_) ||  \
     defined(_CPU_ARM_) || defined(_CPU_PPC64_))
#define JL_HAVE_ASM
#endif
#if 0
// very slow, but more debugging
//#elif defined(_OS_DARWIN_)
//#define JL_HAVE_UNW_CONTEXT
//#elif defined(_OS_LINUX_)
//#define JL_HAVE_UNW_CONTEXT
#elif defined(_OS_EMSCRIPTEN_)
#define JL_HAVE_ASYNCIFY
#elif !defined(JL_HAVE_ASM)
#define JL_HAVE_UNW_CONTEXT // optimistically?
#endif
#endif

#if (!defined(JL_HAVE_UNW_CONTEXT) && defined(JL_HAVE_ASM)) || defined(JL_HAVE_SIGALTSTACK)
typedef jl_stack_context_t _jl_ucontext_t;
#endif
#if defined(JL_HAVE_ASYNCIFY)
#if defined(_COMPILER_TSAN_ENABLED_)
#error TSAN not currently supported with asyncify
#endif
typedef struct {
    // This is the extent of the asyncify stack, but because the top of the
    // asyncify stack (stacktop) is also the bottom of the C stack, we can
    // reuse stacktop for both. N.B.: This matches the layout of the
    // __asyncify_data struct.
    void *stackbottom;
    void *stacktop;
} _jl_ucontext_t;
#endif
#if defined(JL_HAVE_UNW_CONTEXT)
#define UNW_LOCAL_ONLY
#include <libunwind.h>
typedef unw_context_t _jl_ucontext_t;
#endif
#if defined(JL_HAVE_UCONTEXT)
#include <ucontext.h>
typedef ucontext_t _jl_ucontext_t;
#endif
#endif

typedef struct {
    union {
        _jl_ucontext_t ctx;
        jl_stack_context_t copy_ctx;
    };
#if defined(_COMPILER_TSAN_ENABLED_)
    void *tsan_state;
#endif
} jl_ucontext_t;


// handle to reference an OS thread
#ifdef _OS_WINDOWS_
typedef DWORD jl_thread_t;
#else
typedef pthread_t jl_thread_t;
#endif

struct _jl_task_t;

// Recursive spin lock
typedef struct {
    _Atomic(struct _jl_task_t*) owner;
    uint32_t count;
} jl_mutex_t;

typedef struct {
    jl_taggedvalue_t *freelist;   // root of list of free objects
    jl_taggedvalue_t *newpages;   // root of list of chunks of free objects
    uint16_t osize;      // size of objects in this pool
} jl_gc_pool_t;

typedef struct {
    _Atomic(int64_t) allocd;
    _Atomic(int64_t) freed;
    _Atomic(uint64_t) malloc;
    _Atomic(uint64_t) realloc;
    _Atomic(uint64_t) poolalloc;
    _Atomic(uint64_t) bigalloc;
    _Atomic(uint64_t) freecall;
} jl_thread_gc_num_t;

typedef struct {
    // variable for tracking weak references
    arraylist_t weak_refs;
    // live tasks started on this thread
    // that are holding onto a stack from the pool
    arraylist_t live_tasks;

    // variables for tracking malloc'd arrays
    struct _mallocarray_t *mallocarrays;
    struct _mallocarray_t *mafreelist;

    // variables for tracking big objects
    struct _bigval_t *big_objects;

    // variables for tracking "remembered set"
    arraylist_t rem_bindings;
    arraylist_t _remset[2]; // contains jl_value_t*
    // lower bound of the number of pointers inside remembered values
    int remset_nptr;
    arraylist_t *remset;
    arraylist_t *last_remset;

    // variables for allocating objects from pools
#ifdef _P64
#  define JL_GC_N_POOLS 49
#elif MAX_ALIGN == 8
#  define JL_GC_N_POOLS 50
#else
#  define JL_GC_N_POOLS 51
#endif
    jl_gc_pool_t norm_pools[JL_GC_N_POOLS];

#define JL_N_STACK_POOLS 16
    arraylist_t free_stacks[JL_N_STACK_POOLS];
} jl_thread_heap_t;

// Cache of thread local change to global metadata during GC
// This is sync'd after marking.
typedef union _jl_gc_mark_data jl_gc_mark_data_t;

typedef struct {
    void **pc; // Current stack address for the pc (up growing)
    jl_gc_mark_data_t *data; // Current stack address for the data (up growing)
    void **pc_start; // Cached value of `gc_cache->pc_stack`
    void **pc_end; // Cached value of `gc_cache->pc_stack_end`
} jl_gc_mark_sp_t;

typedef struct {
    // thread local increment of `perm_scanned_bytes`
    size_t perm_scanned_bytes;
    // thread local increment of `scanned_bytes`
    size_t scanned_bytes;
    // Number of queued big objects (<= 1024)
    size_t nbig_obj;
    // Array of queued big objects to be moved between the young list
    // and the old list.
    // A set low bit means that the object should be moved from the old list
    // to the young list (`mark_reset_age`).
    // Objects can only be put into this list when the mark bit is flipped to
    // `1` (atomically). Combining with the sync after marking,
    // this makes sure that a single objects can only appear once in
    // the lists (the mark bit cannot be flipped to `0` without sweeping)
    void *big_obj[1024];
    void **pc_stack;
    void **pc_stack_end;
    jl_gc_mark_data_t *data_stack;
} jl_gc_mark_cache_t;

struct _jl_bt_element_t;

// This includes all the thread local states we care about for a thread.
// Changes to TLS field types must be reflected in codegen.
#define JL_MAX_BT_SIZE 80000
typedef struct _jl_tls_states_t {
    int16_t tid;
    int8_t threadpoolid;
    uint64_t rngseed;
    volatile size_t *safepoint;
    _Atomic(int8_t) sleep_check_state; // read/write from foreign threads
    // Whether it is safe to execute GC at the same time.
#define JL_GC_STATE_WAITING 1
    // gc_state = 1 means the thread is doing GC or is waiting for the GC to
    //              finish.
#define JL_GC_STATE_SAFE 2
    // gc_state = 2 means the thread is running unmanaged code that can be
    //              execute at the same time with the GC.
    _Atomic(int8_t) gc_state; // read from foreign threads
    // execution of certain certain impure
    // statements is prohibited from certain
    // callbacks (such as generated functions)
    // as it may make compilation undecidable
    int8_t in_pure_callback;
    int8_t in_finalizer;
    int8_t disable_gc;
    // Counter to disable finalizer **on the current thread**
    int finalizers_inhibited;
    jl_thread_heap_t heap; // this is very large, and the offset is baked into codegen
    jl_thread_gc_num_t gc_num;
    volatile sig_atomic_t defer_signal;
    _Atomic(struct _jl_task_t*) current_task;
    struct _jl_task_t *next_task;
    struct _jl_task_t *previous_task;
    struct _jl_task_t *root_task;
    struct _jl_timing_block_t *timing_stack;
    void *stackbase;
    size_t stacksize;
    union {
        _jl_ucontext_t base_ctx; // base context of stack
        // This hack is needed to support always_copy_stacks:
        jl_stack_context_t copy_stack_ctx;
    };
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
#endif
    jl_thread_t system_id;
    arraylist_t finalizers;
    jl_gc_mark_cache_t gc_cache;
    arraylist_t sweep_objs;
    jl_gc_mark_sp_t gc_mark_sp;
    // Saved exception for previous *external* API call or NULL if cleared.
    // Access via jl_exception_occurred().
    struct _jl_value_t *previous_exception;

    // currently-held locks, to be released when an exception is thrown
    small_arraylist_t locks;

    JULIA_DEBUG_SLEEPWAKE(
        uint64_t uv_run_enter;
        uint64_t uv_run_leave;
        uint64_t sleep_enter;
        uint64_t sleep_leave;
    )

    // some hidden state (usually just because we don't have the type's size declaration)
#ifdef LIBRARY_EXPORTS
    uv_mutex_t sleep_lock;
    uv_cond_t wake_signal;
#endif
} jl_tls_states_t;

#ifndef LIBRARY_EXPORTS
// deprecated (only for external consumers)
JL_DLLEXPORT void *jl_get_ptls_states(void);
#endif

// Update codegen version in `ccall.cpp` after changing either `pause` or `wake`
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

JL_DLLEXPORT void (jl_cpu_pause)(void);
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
#define jl_gc_safepoint_(ptls) do {                     \
        jl_signal_fence();                              \
        size_t safepoint_load = *ptls->safepoint;       \
        jl_signal_fence();                              \
        (void)safepoint_load;                           \
    } while (0)
#define jl_sigint_safepoint(ptls) do {                  \
        jl_signal_fence();                              \
        size_t safepoint_load = ptls->safepoint[-1];    \
        jl_signal_fence();                              \
        (void)safepoint_load;                           \
    } while (0)
#endif
STATIC_INLINE int8_t jl_gc_state_set(jl_ptls_t ptls, int8_t state,
                                     int8_t old_state)
{
    jl_atomic_store_release(&ptls->gc_state, state);
    // A safe point is required if we transition from GC-safe region to
    // non GC-safe region.
    if (old_state && !state)
        jl_gc_safepoint_(ptls);
    return old_state;
}
STATIC_INLINE int8_t jl_gc_state_save_and_set(jl_ptls_t ptls,
                                              int8_t state)
{
    return jl_gc_state_set(ptls, state, jl_atomic_load_relaxed(&ptls->gc_state));
}
#ifdef __clang_gcanalyzer__
int8_t jl_gc_unsafe_enter(jl_ptls_t ptls); // Can be a safepoint
int8_t jl_gc_unsafe_leave(jl_ptls_t ptls, int8_t state) JL_NOTSAFEPOINT;
int8_t jl_gc_safe_enter(jl_ptls_t ptls) JL_NOTSAFEPOINT;
int8_t jl_gc_safe_leave(jl_ptls_t ptls, int8_t state); // Can be a safepoint
#else
#define jl_gc_unsafe_enter(ptls) jl_gc_state_save_and_set(ptls, 0)
#define jl_gc_unsafe_leave(ptls, state) ((void)jl_gc_state_set(ptls, (state), 0))
#define jl_gc_safe_enter(ptls) jl_gc_state_save_and_set(ptls, JL_GC_STATE_SAFE)
#define jl_gc_safe_leave(ptls, state) ((void)jl_gc_state_set(ptls, (state), JL_GC_STATE_SAFE))
#endif

JL_DLLEXPORT void jl_gc_enable_finalizers(struct _jl_task_t *ct, int on);
JL_DLLEXPORT void jl_gc_disable_finalizers_internal(void);
JL_DLLEXPORT void jl_gc_enable_finalizers_internal(void);
JL_DLLEXPORT void jl_gc_run_pending_finalizers(struct _jl_task_t *ct);
extern JL_DLLEXPORT _Atomic(int) jl_gc_have_pending_finalizers;

JL_DLLEXPORT void jl_wakeup_thread(int16_t tid);

#ifdef __cplusplus
}
#endif

#endif
