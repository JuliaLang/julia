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
// the subsystem in `jl_timing_names` matching the provided string.
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
#define jl_timing_init_task(t)
#define jl_timing_block_start(blk)
#define jl_timing_block_enter_task(ct, ptls, blk)
#define jl_timing_block_exit_task(ct, ptls) ((jl_timing_block_t *)NULL)
#define jl_pop_timing_block(blk)

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
#endif

#ifdef USE_ITTAPI
#include <ittapi/ittnotify.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
void jl_print_timings(void);
jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block);

void jl_timing_init_task(jl_task_t *t);
void jl_timing_block_enter_task(jl_task_t *ct, jl_ptls_t ptls, jl_timing_block_t *prev_blk);
jl_timing_block_t *jl_timing_block_exit_task(jl_task_t *ct, jl_ptls_t ptls);

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
        X(LLVM_OPT)              \
        X(LLVM_ORC)              \
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
        X(SAVE_MODULE)           \
        X(INIT_MODULE)           \
        X(LOCK_SPIN)             \
        X(STACKWALK)             \
        X(DL_OPEN)               \
        X(JULIA_INIT)            \


#define JL_TIMING_EVENTS \
        JL_TIMING_SUBSYSTEMS \
        X(GC_Stop) \
        X(GC_Mark) \
        X(GC_FullSweep) \
        X(GC_IncrementalSweep) \
        X(GC_Finalizers) \
        X(CODEGEN_LLVM) \
        X(CODEGEN_Codeinst) \
        X(CODEGEN_Workqueue) \
        X(LOAD_Sysimg) \
        X(LOAD_Pkgimg) \
        X(LOAD_Processor) \
        X(VERIFY_Edges) \
        X(VERIFY_Methods) \
        X(VERIFY_Graph) \
        X(STACKWALK_Backtrace) \
        X(STACKWALK_Excstack) \
        X(NATIVE_Dump) \
        X(NATIVE_Create) \


#define JL_TIMING_COUNTERS \
        X(Invalidations) \
        X(HeapSize) \
        X(JITSize) \
        X(JITCodeSize) \
        X(JITDataSize) \
        X(ImageSize) \


enum jl_timing_subsystems {
#define X(name) JL_TIMING_ ## name,
    JL_TIMING_SUBSYSTEMS
#undef X
    JL_TIMING_LAST
};

enum jl_timing_events {
#define X(name) JL_TIMING_EVENT_ ## name,
    JL_TIMING_EVENTS
#undef X
    JL_TIMING_EVENT_LAST
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

#ifdef USE_TIMING_COUNTS
#define _COUNTS_CTX_MEMBER jl_timing_counts_t counts_ctx;
#define _COUNTS_CTOR(block) _jl_timing_counts_ctor(block)
#define _COUNTS_DESTROY(block, subsystem) _jl_timing_counts_destroy(block, subsystem)
#define _COUNTS_START(block, t) _jl_timing_counts_start(block, t)
#define _COUNTS_STOP(block, t) _jl_timing_counts_stop(block, t)
#else
#define _COUNTS_CTX_MEMBER
#define _COUNTS_CTOR(block)
#define _COUNTS_DESTROY(block, subsystem)
#define _COUNTS_START(block, t)
#define _COUNTS_STOP(block, t)
#endif

/**
 * Timing Backend: Tracy
 **/

#ifdef USE_TRACY
#define _TRACY_CTX_MEMBER TracyCZoneCtx tracy_ctx; const struct ___tracy_source_location_data *tracy_srcloc;
#define _TRACY_CTOR(block, name) static const struct ___tracy_source_location_data TIMING_CONCAT(__tracy_source_location,__LINE__) = { name, __func__,  TracyFile, (uint32_t)__LINE__, 0 }; \
                                         (block)->tracy_srcloc = &TIMING_CONCAT(__tracy_source_location,__LINE__); \
                                         (block)->tracy_ctx.active = 0
#define _TRACY_START(block) (block)->tracy_ctx = ___tracy_emit_zone_begin( (block)->tracy_srcloc, 1 );
#define _TRACY_STOP(ctx) TracyCZoneEnd(ctx)
#else
#define _TRACY_CTX_MEMBER
#define _TRACY_CTOR(block, name)
#define _TRACY_START(block)
#define _TRACY_STOP(ctx)
#endif

/**
 * Timing Backend: Intel VTune (ITTAPI)
 **/

#ifdef USE_ITTAPI
#define _ITTAPI_CTX_MEMBER __itt_event ittapi_event;
#define _ITTAPI_CTOR(block, name) static __itt_event TIMING_CONCAT(__itt_event,__LINE__) = INT_MAX; \
                                  if (TIMING_CONCAT(__itt_event,__LINE__) == INT_MAX) \
                                      TIMING_CONCAT(__itt_event,__LINE__) = __itt_event_create(name, strlen(name)); \
                                  (block)->ittapi_event = TIMING_CONCAT(__itt_event,__LINE__)
#define _ITTAPI_START(block) __itt_event_start((block)->ittapi_event)
#define _ITTAPI_STOP(block) __itt_event_end((block)->ittapi_event)
#else
#define _ITTAPI_CTX_MEMBER
#define _ITTAPI_CTOR(block, name)
#define _ITTAPI_START(block)
#define _ITTAPI_STOP(block)
#endif

/**
 * Implementation: Aggregated counts back-end
 **/

extern JL_DLLEXPORT uint64_t jl_timing_counts[(int)JL_TIMING_LAST];
typedef struct _jl_timing_counts_t {
    uint64_t total;
    uint64_t t0;
#ifdef JL_DEBUG_BUILD
    uint8_t running;
#endif
} jl_timing_counts_t;

STATIC_INLINE void _jl_timing_counts_stop(jl_timing_counts_t *block, uint64_t t) JL_NOTSAFEPOINT {
#ifdef JL_DEBUG_BUILD
    assert(block->running);
    block->running = 0;
#endif
    block->total += t - block->t0;
}

STATIC_INLINE void _jl_timing_counts_start(jl_timing_counts_t *block, uint64_t t) JL_NOTSAFEPOINT {
#ifdef JL_DEBUG_BUILD
    assert(!block->running);
    block->running = 1;
#endif
    block->t0 = t;
}

STATIC_INLINE void _jl_timing_counts_ctor(jl_timing_counts_t *block) JL_NOTSAFEPOINT {
    block->total = 0;
#ifdef JL_DEBUG_BUILD
    block->running = 0;
#endif
}

STATIC_INLINE void _jl_timing_counts_destroy(jl_timing_counts_t *block, int subsystem) JL_NOTSAFEPOINT {
    jl_timing_counts[subsystem] += block->total;
}

/**
 * Top-level jl_timing implementation
 **/

extern JL_DLLEXPORT uint64_t jl_timing_enable_mask;
extern const char *jl_timing_names[(int)JL_TIMING_LAST];

struct _jl_timing_block_t { // typedef in julia.h
    struct _jl_timing_block_t *prev;

    _TRACY_CTX_MEMBER
    _ITTAPI_CTX_MEMBER
    _COUNTS_CTX_MEMBER

    int subsystem;
    int event;
    int8_t is_running;
};

STATIC_INLINE int _jl_timing_enabled(int subsystem) JL_NOTSAFEPOINT {
    return (jl_timing_enable_mask & (1 << subsystem)) != 0;
}

STATIC_INLINE void jl_timing_block_start(jl_timing_block_t *block) {
    assert(!block->is_running);
    if (!_jl_timing_enabled(block->subsystem)) return;

    uint64_t t = cycleclock(); (void)t;
    _COUNTS_START(&block->counts_ctx, t);
    _ITTAPI_START(block);
    _TRACY_START(block);

    jl_timing_block_t **prevp = &jl_current_task->ptls->timing_stack;
    block->prev = *prevp;
    block->is_running = 1;
    if (block->prev) {
        _COUNTS_STOP(&block->prev->counts_ctx, t);
    }
    *prevp = block;
}

STATIC_INLINE void _jl_timing_block_ctor(jl_timing_block_t *block, int subsystem, int event) JL_NOTSAFEPOINT {
    block->subsystem = subsystem;
    block->event = event;
    block->is_running = 0;
    _COUNTS_CTOR(&block->counts_ctx);
}

STATIC_INLINE void _jl_timing_block_destroy(jl_timing_block_t *block) JL_NOTSAFEPOINT {
    if (block->is_running) {
        uint64_t t = cycleclock(); (void)t;
        _ITTAPI_STOP(block);
        _COUNTS_STOP(&block->counts_ctx, t);
        _TRACY_STOP(block->tracy_ctx);

        jl_task_t *ct = jl_current_task;
        jl_timing_block_t **pcur = &ct->ptls->timing_stack;
        assert(*pcur == block);
        *pcur = block->prev;
        if (block->prev) {
            _COUNTS_START(&block->prev->counts_ctx, t);
        }
    }

    _COUNTS_DESTROY(&block->counts_ctx, block->subsystem);
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

#define JL_TIMING(subsystem, event) \
    JL_TIMING_CREATE_BLOCK(__timing_block, subsystem, event); \
    jl_timing_block_start(&__timing_block)

#define JL_TIMING_CREATE_BLOCK(block, subsystem, event) \
    __attribute__((cleanup(_jl_timing_block_destroy))) \
    jl_timing_block_t block; \
    _jl_timing_block_ctor(&block, JL_TIMING_ ## subsystem, JL_TIMING_EVENT_ ## event); \
    _TRACY_CTOR(&block, #event); \
    _ITTAPI_CTOR(&block, #event)

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
