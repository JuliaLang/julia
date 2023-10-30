// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_TIMING_H
#define JL_TIMING_H

#include "julia.h"

static inline const char *gnu_basename(const char *path)
{
    const char *base = strrchr(path, '/');
#ifdef _WIN32
    const char *backslash = strrchr(path, '\\');
    if (backslash > base)
        base = backslash;
#endif
    return base ? base+1 : path;
}

#ifdef USE_TRACY
typedef struct {
    _Atomic(int64_t) val;
    char* name;
} jl_tracy_counter_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

void jl_init_timing(void);
void jl_destroy_timing(void) JL_NOTSAFEPOINT;

// Update the enable bit-mask to enable/disable tracing events for
// the subsystem in `jl_timing_subsystems` matching the provided string.
//
// Returns -1 if no matching sub-system was found.
JL_DLLEXPORT int jl_timing_set_enable(const char *subsystem, uint8_t enabled);

// Check for environment vars "JULIA_TIMING_METADATA_PRINT_LIMIT" and
// "JULIA_TIMING_SUBSYSTEMS" and if present apply these to the metadata
// print limit and the timings enable mask, respectively.
//
// For example, to enable INFERENCE and METHOD_MATCH and disable GC:
//     JULIA_TIMING_SUBSYSTEMS="+INFERENCE,-GC,+METHOD_MATCH"
//
// For example, to increase the metadata item print limit from 10 to 20:
//     JULIA_TIMING_METADATA_PRINT_LIMIT=20
void jl_timing_apply_env(void);

// Configurable item limit, runtime code should use this to limit printing
// when adding potentially many items of metadata to a single timing zone.
extern JL_DLLEXPORT uint32_t jl_timing_print_limit;

JL_DLLEXPORT jl_timing_event_t *_jl_timing_event_create(const char *subsystem, const char *name, const char *function, const char *file, int line, int color);
JL_DLLEXPORT void _jl_timing_block_init(char *buf, size_t size, jl_timing_event_t *event);
JL_DLLEXPORT void _jl_timing_block_start(jl_timing_block_t *cur_block);
JL_DLLEXPORT void _jl_timing_block_end(jl_timing_block_t *cur_block);

#ifdef __cplusplus
}
#endif

#if defined(_COMPILER_CLANG_)
#define HAVE_TIMING_SUPPORT
#elif defined(_COMPILER_GCC_)
#define HAVE_TIMING_SUPPORT
#endif

#if defined( USE_TRACY ) || defined( USE_ITTAPI ) || defined( USE_TIMING_COUNTS )
#define ENABLE_TIMINGS
#endif

#if !defined( ENABLE_TIMINGS ) || !defined( HAVE_TIMING_SUPPORT )

#define JL_TIMING(subsystem, event)
#define JL_TIMING_CREATE_BLOCK(new_block_name, subsystem, event)

#define JL_TIMING_SUSPEND_TASK(subsystem, ct)

#define jl_timing_show(v, b)
#define jl_timing_show_module(m, b)
#define jl_timing_show_filename(f, b)
#define jl_timing_show_method_instance(mi, b)
#define jl_timing_show_method(mi, b)
#define jl_timing_show_func_sig(tt, b)
#define jl_timing_show_location(file, line, mod, b)
#define jl_timing_show_macro(macro, lno, mod, b)
#define jl_timing_printf(b, f, ...)
#define jl_timing_puts(b, s)
#define jl_timing_task_init(t)
#define jl_timing_event_create(blk)
#define jl_timing_block_start(blk)
#define jl_timing_block_task_enter(ct, ptls, blk)
#define jl_timing_block_task_exit(ct, ptls) ((jl_timing_block_t *)NULL)
#define jl_timing_block_pop(blk)

#define jl_timing_counter_inc(counter, value)
#define jl_timing_counter_dec(counter, value)

#define jl_profile_lock_init(lock, name)
#define jl_profile_lock_start_wait(lock)
#define jl_profile_lock_acquired(lock)
#define jl_profile_lock_release_start(lock)
#define jl_profile_lock_release_end(lock)

#else

#include "julia_assert.h"
#ifdef USE_TRACY
#include "tracy/TracyC.h"
typedef struct ___tracy_source_location_data TracySrcLocData;
#endif

#ifdef USE_ITTAPI
#include <ittapi/ittnotify.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
void jl_print_timings(void);

void jl_timing_task_init(jl_task_t *t);
void jl_timing_block_task_enter(jl_task_t *ct, jl_ptls_t ptls, jl_timing_block_t *prev_blk);
jl_timing_block_t *jl_timing_block_task_exit(jl_task_t *ct, jl_ptls_t ptls);
jl_timing_block_t *jl_timing_block_pop(jl_timing_block_t *cur_block);

// Add the output of `jl_static_show(x)` as a text annotation to the
// profiling region corresponding to `cur_block`.
//
// If larger than IOS_INLSIZE (~80 characters), text is truncated.
JL_DLLEXPORT void jl_timing_show(jl_value_t *v, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_module(jl_module_t *m, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_filename(const char *path, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_method_instance(jl_method_instance_t *mi, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_method(jl_method_t *method, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_func_sig(jl_value_t *v, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_location(const char *file, int line, jl_module_t* mod, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_show_macro(jl_method_instance_t *macro, jl_value_t* lno, jl_module_t* mod, jl_timing_block_t *cur_block);
JL_DLLEXPORT void jl_timing_printf(jl_timing_block_t *cur_block, const char *format, ...);
JL_DLLEXPORT void jl_timing_puts(jl_timing_block_t *cur_block, const char *str);

#define jl_timing_event_create(subsystem, name, function, file, line, color) _jl_timing_event_create(subsystem, name, function, file, line, color)
#define jl_timing_block_start(blk) _jl_timing_block_start(blk)
#define jl_timing_block_end(blk) _jl_timing_block_end(blk)

#ifdef __cplusplus
}
#endif

#define JL_TIMING_DEFAULT_BLOCK (&__timing_block)

#define JL_TIMING_SUBSYSTEMS     \
        X(ROOT)                  \
        X(GC)                    \
        X(LOWERING)              \
        X(PARSING)               \
        X(INFERENCE)             \
        X(CODEGEN)               \
        X(METHOD_LOOKUP_SLOW)    \
        X(METHOD_LOOKUP_FAST)    \
        X(CODEINST_COMPILE)      \
        X(LLVM_JIT)              \
        X(METHOD_MATCH)          \
        X(TYPE_CACHE_LOOKUP)     \
        X(TYPE_CACHE_INSERT)     \
        X(STAGED_FUNCTION)       \
        X(MACRO_INVOCATION)      \
        X(AST_COMPRESS)          \
        X(AST_UNCOMPRESS)        \
        X(SYSIMG_DUMP)           \
        X(NATIVE_AOT)            \
        X(ADD_METHOD)            \
        X(LOAD_MODULE)           \
        X(LOAD_IMAGE)            \
        X(VERIFY_IMAGE)          \
        X(VERIFY_IR)             \
        X(SAVE_MODULE)           \
        X(INIT_MODULE)           \
        X(LOCK_SPIN)             \
        X(STACKWALK)             \
        X(DL_OPEN)               \
        X(JULIA_INIT)            \


#define JL_TIMING_COUNTERS \
        X(Invalidations) \
        X(HeapSize) \
        X(JITSize) \
        X(JITCodeSize) \
        X(JITDataSize) \
        X(ImageSize) \


enum jl_timing_subsystem {
#define X(name) JL_TIMING_ ## name,
    JL_TIMING_SUBSYSTEMS
#undef X
    JL_TIMING_SUBSYSTEM_LAST
};

enum jl_timing_counter_types {
#define X(name) JL_TIMING_COUNTER_ ## name,
    JL_TIMING_COUNTERS
#undef X
    JL_TIMING_COUNTER_LAST
};

#define TIMING_XCONCAT(x1, x2) x1##x2
#define TIMING_CONCAT(x1, x2) TIMING_XCONCAT(x1, x2)

/**
 * Timing Backend: Aggregated timing counts (implemented in timing.c)
 **/

typedef struct jl_timing_counts_event_t {
    const char *name;
    _Atomic(uint64_t) self;
    _Atomic(uint64_t) total;
} jl_timing_counts_event_t;

typedef struct _jl_timing_counts_t {
    uint64_t total;
    uint64_t start;
    uint64_t t0;
#ifdef JL_DEBUG_BUILD
    uint8_t running;
#endif
} jl_timing_counts_t;

#ifdef USE_TIMING_COUNTS
#define _COUNTS_EVENT_MEMBER             jl_timing_counts_event_t *counts_event;
#define _COUNTS_BLOCK_MEMBER             jl_timing_counts_t counts_ctx;
#define _COUNTS_START(block, t)          _jl_timing_counts_start(block, t)
#define _COUNTS_STOP(block, t)           _jl_timing_counts_stop(block, t)
#define _COUNTS_PAUSE(block, t)          _jl_timing_counts_pause(block, t)
#define _COUNTS_RESUME(block, t)         _jl_timing_counts_resume(block, t)
#else
#define _COUNTS_EVENT_MEMBER
#define _COUNTS_BLOCK_MEMBER
#define _COUNTS_START(block, t)
#define _COUNTS_STOP(block, t)
#define _COUNTS_PAUSE(block, t)
#define _COUNTS_RESUME(block, t)
#endif

/**
 * Timing Backend: Tracy
 **/

#ifdef USE_TRACY
#define _TRACY_EVENT_MEMBER              TracySrcLocData tracy_srcloc;
#define _TRACY_BLOCK_MEMBER              TracyCZoneCtx tracy_ctx;
#define _TRACY_START(block)              (block)->tracy_ctx = ___tracy_emit_zone_begin( &(block)->event->tracy_srcloc, 1 );
#define _TRACY_STOP(ctx)                 TracyCZoneEnd(ctx)
#else
#define _TRACY_EVENT_MEMBER
#define _TRACY_BLOCK_MEMBER
#define _TRACY_START(block)
#define _TRACY_STOP(ctx)
#endif

/**
 * Timing Backend: Intel VTune (ITTAPI)
 **/

#ifdef USE_ITTAPI
#define _ITTAPI_EVENT_MEMBER              __itt_event ittapi_event;
#define _ITTAPI_BLOCK_MEMBER
#define _ITTAPI_START(block)              __itt_event_start((block)->event->ittapi_event)
#define _ITTAPI_STOP(block)               __itt_event_end((block)->event->ittapi_event)
#else
#define _ITTAPI_EVENT_MEMBER
#define _ITTAPI_BLOCK_MEMBER
#define _ITTAPI_START(block)
#define _ITTAPI_STOP(block)
#endif

/**
 * Top-level jl_timing implementation
 **/

extern JL_DLLEXPORT _Atomic(uint64_t) jl_timing_disable_mask[(JL_TIMING_SUBSYSTEM_LAST + sizeof(uint64_t) * CHAR_BIT - 1) / (sizeof(uint64_t) * CHAR_BIT)];
extern const char *jl_timing_subsystems[(int)JL_TIMING_SUBSYSTEM_LAST];

/**
 * Stores all static attributes associated with a profiling event.
 *
 * A single event can be used to create many timing blocks with
 * the same name/source information.
 **/
struct _jl_timing_event_t { // typedef in julia.h
    _TRACY_EVENT_MEMBER
    _ITTAPI_EVENT_MEMBER
    _COUNTS_EVENT_MEMBER

    int subsystem;
};

/**
 * Stores all dynamic attributes associated with a timing block.
 *
 * Every time the application enters an instrumented block of code,
 * a new timing block is created. A timing block corresponds to one
 * "span" of time in the profiler.
 **/
struct _jl_timing_block_t { // typedef in julia.h
    struct _jl_timing_block_t *prev;
    jl_timing_event_t *event;

    _TRACY_BLOCK_MEMBER
    _ITTAPI_BLOCK_MEMBER
    _COUNTS_BLOCK_MEMBER

    uint8_t is_running;
};

STATIC_INLINE int _jl_timing_enabled(int subsystem) JL_NOTSAFEPOINT {
    return (jl_atomic_load_relaxed(jl_timing_disable_mask + subsystem / (sizeof(uint64_t) * CHAR_BIT)) & (1 << (subsystem % (sizeof(uint64_t) * CHAR_BIT)))) == 0;
}

typedef struct _jl_timing_suspend_t {
    jl_task_t *ct;
} jl_timing_suspend_t;

STATIC_INLINE void _jl_timing_suspend_ctor(jl_timing_suspend_t *suspend, const char *subsystem, jl_task_t *ct) JL_NOTSAFEPOINT {
    suspend->ct = ct;
#ifdef USE_TRACY
    TracyCFiberEnter(subsystem);
#endif
}

STATIC_INLINE void _jl_timing_suspend_destroy(jl_timing_suspend_t *suspend) JL_NOTSAFEPOINT {
#ifdef USE_TRACY
    TracyCFiberEnter(suspend->ct->name);
#endif
}

#define JL_TIMING(subsystem, event)                                           \
    JL_TIMING_CREATE_BLOCK(__timing_block, subsystem, event);                 \
    jl_timing_block_start(&__timing_block)

#define JL_TIMING_CREATE_BLOCK(block, subsystem_name, event_name)             \
    static jl_timing_event_t *TIMING_CONCAT(__timing_event, __LINE__) = 0;    \
    if (!TIMING_CONCAT(__timing_event, __LINE__))                             \
        TIMING_CONCAT(__timing_event, __LINE__) = jl_timing_event_create(     \
            #subsystem_name, #event_name, __func__, __FILE__, __LINE__, 0     \
        );                                                                    \
    __attribute__((cleanup(_jl_timing_block_end)))                            \
    jl_timing_block_t block = { 0 };                                          \
    block.event = TIMING_CONCAT(__timing_event, __LINE__)

#define JL_TIMING_SUSPEND_TASK(subsystem, ct) \
    __attribute__((cleanup(_jl_timing_suspend_destroy))) \
    jl_timing_suspend_t __timing_suspend; \
    _jl_timing_suspend_ctor(&__timing_suspend, #subsystem, ct)

// Counting
#ifdef USE_ITTAPI
#define _ITTAPI_COUNTER_MEMBER __itt_counter ittapi_counter;
#else
#define _ITTAPI_COUNTER_MEMBER
#endif

#ifdef USE_TRACY
# define _TRACY_COUNTER_MEMBER jl_tracy_counter_t tracy_counter;
# else
# define _TRACY_COUNTER_MEMBER
#endif

#ifdef USE_TIMING_COUNTS
#define _COUNTS_MEMBER _Atomic(uint64_t) basic_counter;
#else
#define _COUNTS_MEMBER
#endif

typedef struct {
    _ITTAPI_COUNTER_MEMBER
    _TRACY_COUNTER_MEMBER
    _COUNTS_MEMBER
} jl_timing_counter_t;

JL_DLLEXPORT extern jl_timing_counter_t jl_timing_counters[JL_TIMING_COUNTER_LAST];

static inline void jl_timing_counter_inc(int counter, uint64_t val) JL_NOTSAFEPOINT {
#ifdef USE_ITTAPI
    __itt_counter_inc_delta(jl_timing_counters[counter].ittapi_counter, val);
#endif
#ifdef USE_TRACY
    jl_tracy_counter_t *tracy_counter = &jl_timing_counters[counter].tracy_counter;
    uint64_t oldval = jl_atomic_fetch_add_relaxed(&tracy_counter->val, val);
    TracyCPlotI(tracy_counter->name, oldval + val);
#endif
#ifdef USE_TIMING_COUNTS
    jl_atomic_fetch_add_relaxed(&jl_timing_counters[counter].basic_counter, val);
#endif
}

static inline void jl_timing_counter_dec(int counter, uint64_t val) JL_NOTSAFEPOINT {
#ifdef USE_ITTAPI
    __itt_counter_dec_delta(jl_timing_counters[counter].ittapi_counter, val);
#endif
#ifdef USE_TRACY
    jl_tracy_counter_t *tracy_counter = &jl_timing_counters[counter].tracy_counter;
    uint64_t oldval = jl_atomic_fetch_add_relaxed(&tracy_counter->val, -val);
    TracyCPlotI(tracy_counter->name, oldval - val);
#endif
#ifdef USE_TIMING_COUNTS
    jl_atomic_fetch_add_relaxed(&jl_timing_counters[counter].basic_counter, -(int64_t)val);
#endif
}

// Locking profiling
static inline void jl_profile_lock_init(jl_mutex_t *lock, const char *name) {
#ifdef USE_ITTAPI
    __itt_sync_create(lock, "jl_mutex_t", name, __itt_attr_mutex);
#endif
}
static inline void jl_profile_lock_start_wait(jl_mutex_t *lock) {
#ifdef USE_ITTAPI
    __itt_sync_prepare(lock);
#endif
}
static inline void jl_profile_lock_acquired(jl_mutex_t *lock) {
#ifdef USE_ITTAPI
    __itt_sync_acquired(lock);
#endif
}
static inline void jl_profile_lock_release_start(jl_mutex_t *lock) {
#ifdef USE_ITTAPI
    __itt_sync_releasing(lock);
#endif
}
static inline void jl_profile_lock_release_end(jl_mutex_t *lock) {}

#endif

#endif
