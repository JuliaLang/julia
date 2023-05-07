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

#ifdef __cplusplus
extern "C" {
#endif

void jl_init_timing(void);
void jl_destroy_timing(void) JL_NOTSAFEPOINT;

// Update the enable bit-mask to enable/disable tracing events for
// the subsystem in `jl_timing_names` matching the provided string.
//
// Returns -1 if no matching sub-system was found.
int jl_timing_set_enable(const char *subsystem, uint8_t enabled);

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
extern uint32_t jl_timing_print_limit;

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#define HAVE_TIMING_SUPPORT
#elif defined(_COMPILER_CLANG_)
#define HAVE_TIMING_SUPPORT
#elif defined(_COMPILER_GCC_)
#define HAVE_TIMING_SUPPORT
#endif

#if defined( USE_TRACY ) || defined( USE_ITTAPI ) || defined( USE_TIMING_COUNTS )
#define ENABLE_TIMINGS
#endif

#if !defined( ENABLE_TIMINGS ) || !defined( HAVE_TIMING_SUPPORT )

#define JL_TIMING(subsystem, event)
#define JL_TIMING_SUSPEND(subsystem, ct)

#define jl_timing_show(v, b)
#define jl_timing_show_module(m, b)
#define jl_timing_show_filename(f, b)
#define jl_timing_show_method_instance(mi, b)
#define jl_timing_show_method(mi, b)
#define jl_timing_show_func_sig(tt, b)
#define jl_timing_printf(b, f, ...)
#define jl_timing_puts(b, s)
#define jl_timing_init_task(t)
#define jl_timing_block_enter_task(ct, ptls, blk)
#define jl_timing_block_exit_task(ct, ptls) ((jl_timing_block_t *)NULL)
#define jl_pop_timing_block(blk)

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
void jl_timing_show(jl_value_t *v, jl_timing_block_t *cur_block);
void jl_timing_show_module(jl_module_t *m, jl_timing_block_t *cur_block);
void jl_timing_show_filename(const char *path, jl_timing_block_t *cur_block);
void jl_timing_show_method_instance(jl_method_instance_t *mi, jl_timing_block_t *cur_block);
void jl_timing_show_method(jl_method_t *method, jl_timing_block_t *cur_block);
void jl_timing_show_func_sig(jl_value_t *v, jl_timing_block_t *cur_block);
void jl_timing_printf(jl_timing_block_t *cur_block, const char *format, ...);
void jl_timing_puts(jl_timing_block_t *cur_block, const char *str);
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#define JL_TIMING_CURRENT_BLOCK (&__timing_block.block)
#else
#define JL_TIMING_CURRENT_BLOCK (&__timing_block)
#endif

#define JL_TIMING_OWNERS         \
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
        JL_TIMING_OWNERS \
        X(GC_Stop) \
        X(GC_Mark) \
        X(GC_Sweep) \
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


enum jl_timing_owners {
#define X(name) JL_TIMING_ ## name,
    JL_TIMING_OWNERS
#undef X
    JL_TIMING_LAST
};

enum jl_timing_events {
#define X(name) JL_TIMING_EVENT_ ## name,
    JL_TIMING_EVENTS
#undef X
    JL_TIMING_EVENT_LAST
};

/**
 * Timing back-ends differ in terms of whether they support nested
 * and asynchronous events.
 **/

/**
 * Timing Backend: Aggregated timing counts (implemented in timing.c)
 **/

#ifdef USE_TIMING_COUNTS
#define _COUNTS_CTX_MEMBER jl_timing_counts_t counts_ctx;
#define _COUNTS_CTOR(block, owner) _jl_timing_counts_ctor(block, owner)
#define _COUNTS_DESTROY(block) _jl_timing_counts_destroy(block)
#define _COUNTS_START(block, t) _jl_timing_counts_start(block, t)
#define _COUNTS_STOP(block, t) _jl_timing_counts_stop(block, t)
#else
#define _COUNTS_CTX_MEMBER
#define _COUNTS_CTOR(block, owner)
#define _COUNTS_DESTROY(block)
#define _COUNTS_START(block, t)
#define _COUNTS_STOP(block, t)
#endif

/**
 * Timing Backend: Tracy
 **/

#ifdef USE_TRACY
#define _TRACY_CTX_MEMBER TracyCZoneCtx *tracy_ctx;
#define _TRACY_CTOR(context, name, enable) TracyCZoneN(__tracy_ctx, name, (enable)); \
                                           (context) = &__tracy_ctx
#define _TRACY_DESTROY(ctx) TracyCZoneEnd(*ctx)
#else
#define _TRACY_CTX_MEMBER
#define _TRACY_CTOR(context, name, enable)
#define _TRACY_DESTROY(block)
#endif

#ifdef USE_ITTAPI
#define _ITTAPI_CTX_MEMBER int owner; int event;
#define _ITTAPI_CTOR(block, owner, event) block->owner = owner; block->event = event
#define _ITTAPI_START(block) if (_jl_timing_enabled(block->owner)) __itt_event_start(jl_timing_ittapi_events[block->event])
#define _ITTAPI_STOP(block) if (_jl_timing_enabled(block->owner)) __itt_event_end(jl_timing_ittapi_events[block->event])
#else
#define _ITTAPI_CTX_MEMBER
#define _ITTAPI_CTOR(block, owner, event)
#define _ITTAPI_START(block)
#define _ITTAPI_STOP(block)
#endif

/**
 * Implementation: Aggregated counts back-end
 **/

extern uint64_t jl_timing_counts[(int)JL_TIMING_LAST];
typedef struct _jl_timing_counts_t {
    uint64_t total;
    uint64_t t0;
    int owner;
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

STATIC_INLINE void _jl_timing_counts_ctor(jl_timing_counts_t *block, int owner) JL_NOTSAFEPOINT {
    block->owner = owner;
    block->total = 0;
#ifdef JL_DEBUG_BUILD
    block->running = 0;
#endif
}

STATIC_INLINE void _jl_timing_counts_destroy(jl_timing_counts_t *block) JL_NOTSAFEPOINT {
    jl_timing_counts[block->owner] += block->total;
}

/**
 * Top-level jl_timing implementation
 **/

extern uint64_t jl_timing_enable_mask;
extern const char *jl_timing_names[(int)JL_TIMING_LAST];
#ifdef USE_ITTAPI
extern __itt_event jl_timing_ittapi_events[(int)JL_TIMING_EVENT_LAST];
#endif

struct _jl_timing_block_t { // typedef in julia.h
    struct _jl_timing_block_t *prev;
    _TRACY_CTX_MEMBER
    _ITTAPI_CTX_MEMBER
    _COUNTS_CTX_MEMBER
};

STATIC_INLINE int _jl_timing_enabled(int event) JL_NOTSAFEPOINT {
    return !!(jl_timing_enable_mask & (1 << event));
}

STATIC_INLINE void _jl_timing_block_ctor(jl_timing_block_t *block, int owner, int event) JL_NOTSAFEPOINT {
    uint64_t t = cycleclock(); (void)t;
    _COUNTS_CTOR(&block->counts_ctx, owner);
    _COUNTS_START(&block->counts_ctx, t);
    _ITTAPI_CTOR(block, owner, event);
    _ITTAPI_START(block);

    jl_task_t *ct = jl_current_task;
    jl_timing_block_t **prevp = &ct->ptls->timing_stack;
    block->prev = *prevp;
    if (block->prev) {
        _COUNTS_STOP(&block->prev->counts_ctx, t);
    }
    *prevp = block;
}

STATIC_INLINE void _jl_timing_block_destroy(jl_timing_block_t *block) JL_NOTSAFEPOINT {
    uint64_t t = cycleclock(); (void)t;

    _ITTAPI_STOP(block);
    _COUNTS_STOP(&block->counts_ctx, t);
    _COUNTS_DESTROY(&block->counts_ctx);
    _TRACY_DESTROY(block->tracy_ctx);

    jl_task_t *ct = jl_current_task;
    jl_timing_block_t **pcur = &ct->ptls->timing_stack;
    assert(*pcur == block);
    *pcur = block->prev;
    if (block->prev) {
        _COUNTS_START(&block->prev->counts_ctx, t);
    }
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

#ifdef __cplusplus
struct jl_timing_block_cpp_t {
    jl_timing_block_t block;
    jl_timing_block_cpp_t(int owner, int event) JL_NOTSAFEPOINT {
        _jl_timing_block_ctor(&block, owner, event);
    }
    ~jl_timing_block_cpp_t() JL_NOTSAFEPOINT {
        _jl_timing_block_destroy(&block);
    }
    jl_timing_block_cpp_t(const jl_timing_block_cpp_t&) = delete;
    jl_timing_block_cpp_t(const jl_timing_block_cpp_t&&) = delete;
    jl_timing_block_cpp_t& operator=(const jl_timing_block_cpp_t &) = delete;
    jl_timing_block_cpp_t& operator=(const jl_timing_block_cpp_t &&) = delete;
};
#define JL_TIMING(subsystem, event) jl_timing_block_cpp_t __timing_block(JL_TIMING_ ## subsystem, JL_TIMING_EVENT_ ## event); \
    _TRACY_CTOR(__timing_block.block.tracy_ctx, #event, (jl_timing_enable_mask >> (JL_TIMING_ ## subsystem)) & 1)
#else
#define JL_TIMING(subsystem, event) \
    __attribute__((cleanup(_jl_timing_block_destroy))) \
    jl_timing_block_t __timing_block; \
    _jl_timing_block_ctor(&__timing_block, JL_TIMING_ ## subsystem, JL_TIMING_EVENT_ ## event); \
    _TRACY_CTOR(__timing_block.tracy_ctx, #event, (jl_timing_enable_mask >> (JL_TIMING_ ## subsystem)) & 1)
#endif

#ifdef __cplusplus
struct jl_timing_suspend_cpp_t {
    jl_timing_suspend_t suspend;
    jl_timing_suspend_cpp_t(const char *subsystem, jl_task_t *ct) JL_NOTSAFEPOINT {
        _jl_timing_suspend_ctor(&suspend, subsystem, ct);
    }
    ~jl_timing_suspend_cpp_t() JL_NOTSAFEPOINT {
        _jl_timing_suspend_destroy(&suspend);
    }
    jl_timing_suspend_cpp_t(const jl_timing_suspend_cpp_t &) = delete;
    jl_timing_suspend_cpp_t(jl_timing_suspend_cpp_t &&) = delete;
    jl_timing_suspend_cpp_t& operator=(const jl_timing_suspend_cpp_t &) = delete;
    jl_timing_suspend_cpp_t& operator=(jl_timing_suspend_cpp_t &&) = delete;
};
#define JL_TIMING_SUSPEND(subsystem, ct) jl_timing_suspend_cpp_t __suspend_block(#subsystem, ct)
#else
#define JL_TIMING_SUSPEND(subsystem, ct) \
    __attribute__((cleanup(_jl_timing_suspend_destroy))) \
    jl_timing_suspend_t __timing_suspend; \
    _jl_timing_suspend_ctor(&__timing_suspend, #subsystem, ct)
#endif

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
