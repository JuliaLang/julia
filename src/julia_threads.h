// This file is a part of Julia. License is MIT: https://julialang.org/license

// Meant to be included in <julia.h>
#ifndef JL_THREADS_H
#define JL_THREADS_H

// threading ------------------------------------------------------------------

// WARNING: Threading support is incomplete and experimental
// Nonetheless, we define JL_THREAD and use it to give advanced notice to
// maintainers of what eventual threading support will change.

// JULIA_ENABLE_THREADING is switched on in Make.inc if JULIA_THREADS is
// set (in Make.user)

// Recursive spin lock
typedef struct {
    volatile unsigned long owner;
    uint32_t count;
} jl_mutex_t;

typedef struct {
    jl_taggedvalue_t *freelist;   // root of list of free objects
    jl_taggedvalue_t *newpages;   // root of list of chunks of free objects
    uint16_t osize;      // size of objects in this pool
} jl_gc_pool_t;

typedef struct {
    // variable for tracking weak references
    arraylist_t weak_refs;

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
#  define JL_GC_N_POOLS 41
#elif defined(_CPU_ARM_) || defined(_CPU_PPC_)
#  define JL_GC_N_POOLS 42
#else
#  define JL_GC_N_POOLS 43
#endif
    jl_gc_pool_t norm_pools[JL_GC_N_POOLS];
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
    jl_mutex_t stack_lock;
    void **pc_stack;
    void **pc_stack_end;
    jl_gc_mark_data_t *data_stack;
} jl_gc_mark_cache_t;

// This includes all the thread local states we care about for a thread.
#define JL_MAX_BT_SIZE 80000
struct _jl_tls_states_t {
    struct _jl_gcframe_t *pgcstack;
    size_t world_age;
    struct _jl_value_t *exception_in_transit;
    volatile size_t *safepoint;
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
    volatile sig_atomic_t defer_signal;
    struct _jl_module_t *current_module;
    struct _jl_task_t *volatile current_task;
    struct _jl_task_t *root_task;
    void *stackbase;
    char *stack_lo;
    char *stack_hi;
    jl_jmp_buf base_ctx; // base context of stack
    jl_jmp_buf *safe_restore;
    int16_t tid;
    size_t bt_size;
    // JL_MAX_BT_SIZE + 1 elements long
    uintptr_t *bt_data;
    // Atomically set by the sender, reset by the handler.
    volatile sig_atomic_t signal_request;
    // Allow the sigint to be raised asynchronously
    // this is limited to the few places we do synchronous IO
    // we can make this more general (similar to defer_signal) if necessary
    volatile sig_atomic_t io_wait;
    jl_thread_heap_t heap;
#ifndef _OS_WINDOWS_
    // These are only used on unix now
    pthread_t system_id;
    void *signal_stack;
#endif
    // execution of certain certain impure
    // statements is prohibited from certain
    // callbacks (such as generated functions)
    // as it may make compilation undecidable
    int in_pure_callback;
    // Counter to disable finalizer **on the current thread**
    int finalizers_inhibited;
    arraylist_t finalizers;
    jl_gc_mark_cache_t gc_cache;
    arraylist_t sweep_objs;
    jl_gc_mark_sp_t gc_mark_sp;
};

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

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT void (jl_cpu_pause)(void);
JL_DLLEXPORT void (jl_cpu_wake)(void);

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
#ifndef JULIA_ENABLE_THREADING
#define jl_gc_state(ptls) ((int8_t)0)
STATIC_INLINE int8_t jl_gc_state_set(jl_ptls_t ptls, int8_t state,
                                     int8_t old_state)
{
    (void)ptls;
    (void)state;
    return old_state;
}
#else // ifndef JULIA_ENABLE_THREADING
// Make sure jl_gc_state() is always a rvalue
#define jl_gc_state(ptls) ((int8_t)ptls->gc_state)
STATIC_INLINE int8_t jl_gc_state_set(jl_ptls_t ptls, int8_t state,
                                     int8_t old_state)
{
    ptls->gc_state = state;
    // A safe point is required if we transition from GC-safe region to
    // non GC-safe region.
    if (old_state && !state)
        jl_gc_safepoint_(ptls);
    return old_state;
}
#endif // ifndef JULIA_ENABLE_THREADING
STATIC_INLINE int8_t jl_gc_state_save_and_set(jl_ptls_t ptls,
                                              int8_t state)
{
    return jl_gc_state_set(ptls, state, jl_gc_state(ptls));
}
#define jl_gc_unsafe_enter(ptls) jl_gc_state_save_and_set(ptls, 0)
#define jl_gc_unsafe_leave(ptls, state) ((void)jl_gc_state_set(ptls, (state), 0))
#define jl_gc_safe_enter(ptls) jl_gc_state_save_and_set(ptls, JL_GC_STATE_SAFE)
#define jl_gc_safe_leave(ptls, state) ((void)jl_gc_state_set(ptls, (state), JL_GC_STATE_SAFE))
JL_DLLEXPORT void (jl_gc_safepoint)(void);

JL_DLLEXPORT void jl_gc_enable_finalizers(jl_ptls_t ptls, int on);

#ifdef __cplusplus
}
#endif

#endif
