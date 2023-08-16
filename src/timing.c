// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "options.h"
#include "stdio.h"

#if defined(USE_TRACY) || defined(USE_ITTAPI)
#define DISABLE_FREQUENT_EVENTS
#endif

jl_module_t *jl_module_root(jl_module_t *m);

#ifdef __cplusplus
extern "C" {
#endif

#ifdef ENABLE_TIMINGS

#ifndef HAVE_TIMING_SUPPORT
#error Timings are not supported on your compiler
#endif

static uint64_t t0;

JL_DLLEXPORT _Atomic(uint64_t) jl_timing_disable_mask[(JL_TIMING_SUBSYSTEM_LAST + sizeof(uint64_t) * CHAR_BIT - 1) / (sizeof(uint64_t) * CHAR_BIT)];

// Used to as an item limit when several strings of metadata can
// potentially be associated with a single timing zone.
JL_DLLEXPORT uint32_t jl_timing_print_limit = 10;

const char *jl_timing_subsystems[(int)JL_TIMING_SUBSYSTEM_LAST] =
    {
#define X(name) #name,
        JL_TIMING_SUBSYSTEMS
#undef X
    };

JL_DLLEXPORT jl_timing_counter_t jl_timing_counters[JL_TIMING_COUNTER_LAST];

#ifdef USE_TIMING_COUNTS
static arraylist_t jl_timing_counts_events;
static jl_mutex_t jl_timing_counts_events_lock;
#endif //USE_TIMING_COUNTS

#ifdef USE_ITTAPI
static arraylist_t jl_timing_ittapi_events;
static jl_mutex_t jl_timing_ittapi_events_lock;
#endif //USE_ITTAPI

#ifdef USE_TIMING_COUNTS
static int cmp_counts_events(const void *a, const void *b) {
    jl_timing_counts_event_t *event_a = *(jl_timing_counts_event_t **)a;
    jl_timing_counts_event_t *event_b = *(jl_timing_counts_event_t **)b;
    return strcmp(event_a->name, event_b->name);
}
#endif

void jl_print_timings(void)
{
#ifdef USE_TIMING_COUNTS
    qsort(jl_timing_counts_events.items, jl_timing_counts_events.len,
          sizeof(jl_timing_counts_event_t *), cmp_counts_events);

    JL_LOCK_NOGC(&jl_timing_counts_events_lock);
    uint64_t total_time = cycleclock() - t0;
    uint64_t root_time = total_time;
    jl_timing_counts_event_t *root_event;
    for (int i = 0; i < jl_timing_counts_events.len; i++) {
        jl_timing_counts_event_t *other_event = (jl_timing_counts_event_t *)jl_timing_counts_events.items[i];
        if (strcmp(other_event->name, "ROOT") == 0) {
            root_event = other_event;
        } else {
            root_time -= jl_atomic_load_relaxed(&other_event->self);
        }
    }
    jl_atomic_store_relaxed(&root_event->self, root_time);
    jl_atomic_store_relaxed(&root_event->total, total_time);

    fprintf(stderr, "\nJULIA TIMINGS\n");
    fprintf(stderr, "%-25s, %-30s, %-30s\n", "Event", "Self Cycles (% of Total)", "Total Cycles (% of Total)");
    for (int i = 0; i < jl_timing_counts_events.len; i++) {
        jl_timing_counts_event_t *event = (jl_timing_counts_event_t *)jl_timing_counts_events.items[i];
        uint64_t self = jl_atomic_load_relaxed(&event->self);
        uint64_t total = jl_atomic_load_relaxed(&event->total);
        if (total != 0)
            fprintf(stderr, "%-25s, %20" PRIu64 " (%5.2f %%), %20" PRIu64 " (%5.2f %%)\n",
                    event->name,
                    self, 100 * (((double)self) / total_time),
                    total, 100 * (((double)total) / total_time));
    }
    JL_UNLOCK_NOGC(&jl_timing_counts_events_lock);

    fprintf(stderr, "\nJULIA COUNTERS\n");
    fprintf(stderr, "%-25s, %-20s\n", "Counter", "Value");
#define X(name) do { \
        int64_t val = (int64_t) jl_atomic_load_relaxed(&jl_timing_counters[(int)JL_TIMING_COUNTER_##name].basic_counter); \
        if (val != 0) \
            fprintf(stderr, "%-25s, %20" PRIi64 "\n", #name, val); \
    } while (0);

    JL_TIMING_COUNTERS
#undef X
#endif
}

static int indirect_strcmp(const void *a, const void *b) {
    return strcmp(*(const char **)a, *(const char **)b);
}

void jl_init_timing(void)
{
    t0 = cycleclock();

    _Static_assert(JL_TIMING_SUBSYSTEM_LAST < sizeof(uint64_t) * CHAR_BIT, "Too many timing subsystems!");

#ifdef USE_TIMING_COUNTS
    JL_MUTEX_INIT(&jl_timing_counts_events_lock, "jl_timing_counts_events_lock");

    // Create events list for counts backend
    arraylist_new(&jl_timing_counts_events, 1);

    jl_timing_counts_event_t *root_event = (jl_timing_counts_event_t *)malloc(sizeof(jl_timing_counts_event_t));
    arraylist_push(&jl_timing_counts_events, (void *)root_event);

    root_event->name = "ROOT";
    jl_atomic_store_relaxed(&root_event->self, 0);
    jl_atomic_store_relaxed(&root_event->total, 0);
#endif

#ifdef USE_ITTAPI
    // Create events list for ITTAPI backend
    JL_MUTEX_INIT(&jl_timing_ittapi_events_lock, "jl_timing_ittapi_events_lock");
    arraylist_new(&jl_timing_ittapi_events, 0);
#endif

    // Sort the subsystem names for quick enable/disable lookups
    qsort(jl_timing_subsystems, JL_TIMING_SUBSYSTEM_LAST,
          sizeof(const char *), indirect_strcmp);

    int i __attribute__((unused)) = 0;
#ifdef USE_ITTAPI
    i = 0;
#define X(name) jl_timing_counters[i++].ittapi_counter = __itt_counter_create(#name, "julia.runtime");
    JL_TIMING_COUNTERS
#undef X
#endif
#ifdef USE_TRACY
    i = 0;
#define X(counter_name) jl_timing_counters[i].tracy_counter = (jl_tracy_counter_t){0, #counter_name}; \
        TracyCPlotConfig(jl_timing_counters[i++].tracy_counter.name, TracyPlotFormatNumber, /* rectilinear */ 1, /* fill */ 1, /* color */ 0);
    JL_TIMING_COUNTERS
#undef X
    // We reference these by enum indexing and then asking for the name, since that allows the compiler
    // to catch name mismatches.
    TracyCPlotConfig(jl_timing_counters[JL_TIMING_COUNTER_HeapSize].tracy_counter.name, TracyPlotFormatMemory, /* rectilinear */ 0, /* fill */ 1, /* color */ 0);
    TracyCPlotConfig(jl_timing_counters[JL_TIMING_COUNTER_JITSize].tracy_counter.name, TracyPlotFormatMemory, /* rectilinear */ 0, /* fill */ 1, /* color */ 0);
    TracyCPlotConfig(jl_timing_counters[JL_TIMING_COUNTER_JITCodeSize].tracy_counter.name, TracyPlotFormatMemory, /* rectilinear */ 0, /* fill */ 1, /* color */ 0);
    TracyCPlotConfig(jl_timing_counters[JL_TIMING_COUNTER_JITDataSize].tracy_counter.name, TracyPlotFormatMemory, /* rectilinear */ 0, /* fill */ 1, /* color */ 0);
    TracyCPlotConfig(jl_timing_counters[JL_TIMING_COUNTER_ImageSize].tracy_counter.name, TracyPlotFormatMemory, /* rectilinear */ 0, /* fill */ 1, /* color */ 0);
#endif

/**
 * These sources often generate millions of events / minute. Although Tracy
 * can generally keep up with that, those events also bloat the saved ".tracy"
 * files, so we disable them by default.
 **/
#ifdef DISABLE_FREQUENT_EVENTS
    uint8_t error = 0;
    error |= jl_timing_set_enable("ROOT", 0);
    error |= jl_timing_set_enable("TYPE_CACHE_LOOKUP", 0);
    error |= jl_timing_set_enable("METHOD_MATCH", 0);
    error |= jl_timing_set_enable("METHOD_LOOKUP_FAST", 0);
    error |= jl_timing_set_enable("AST_COMPRESS", 0);
    error |= jl_timing_set_enable("AST_UNCOMPRESS", 0);
    if (error)
        jl_error("invalid timing subsystem encountered in jl_init_timing");
#endif

    // Apply e.g. JULIA_TIMING_SUBSYSTEMS="+GC,-INFERENCE" and
    //            JULIA_TIMING_METADATA_PRINT_LIMIT=20
    jl_timing_apply_env();
}

void jl_destroy_timing(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_timing_block_t *stack = ptls->timing_stack;
    while (stack) {
        jl_timing_block_end(stack);
        stack = stack->prev;
    }
}

static const int get_timing_subsystem(const char *subsystem) {
    const char **match = (const char **)bsearch(
        &subsystem, jl_timing_subsystems, JL_TIMING_SUBSYSTEM_LAST,
        sizeof(const char *), indirect_strcmp
    );
    if (!match)
        return JL_TIMING_SUBSYSTEM_LAST;

    return (int)(match - &jl_timing_subsystems[0]);
}

#ifdef USE_ITTAPI

typedef struct {
    __itt_event event;
    const char *name;
} cached_ittapi_event_t;

static __itt_event _jl_timing_ittapi_event_create(const char *event) {
    JL_LOCK_NOGC(&jl_timing_ittapi_events_lock);
    const size_t n = jl_timing_ittapi_events.len;
    for (size_t i = 0; i < n; i++) {
        cached_ittapi_event_t *other_event = (cached_ittapi_event_t *)jl_timing_ittapi_events.items[i];
        if (strcmp(event, other_event->name) == 0) {
            JL_UNLOCK_NOGC(&jl_timing_ittapi_events_lock);
            return other_event->event;
        }
    }

    // No matching event found - create a new one
    cached_ittapi_event_t *new_event = (cached_ittapi_event_t *)malloc(sizeof(cached_ittapi_event_t));
    arraylist_push(&jl_timing_ittapi_events, (void *)new_event);
    new_event->name = event;
    new_event->event = __itt_event_create(event, strlen(event));
    JL_UNLOCK_NOGC(&jl_timing_ittapi_events_lock);

    return new_event->event;
}

#endif // USE_ITTAPI

#ifdef USE_TIMING_COUNTS

// This function is analogous to __itt_event_create but for the counts backend
//
// `event` is required to live forever
static jl_timing_counts_event_t *_jl_timing_counts_event_create(const char *event) {
    JL_LOCK_NOGC(&jl_timing_counts_events_lock);
    const size_t n = jl_timing_counts_events.len;
    for (size_t i = 0; i < n; i++) {
        jl_timing_counts_event_t *other_event = (jl_timing_counts_event_t *)jl_timing_counts_events.items[i];
        if (strcmp(event, other_event->name) == 0) {
            JL_UNLOCK_NOGC(&jl_timing_counts_events_lock);
            return other_event;
        }
    }

    // No matching event found - create a new one
    jl_timing_counts_event_t *new_event = (jl_timing_counts_event_t *)malloc(sizeof(jl_timing_counts_event_t));
    arraylist_push(&jl_timing_counts_events, (void *)new_event);
    new_event->name = event;
    jl_atomic_store_relaxed(&new_event->self, 0);
    jl_atomic_store_relaxed(&new_event->total, 0);
    JL_UNLOCK_NOGC(&jl_timing_counts_events_lock);

    return new_event;
}

STATIC_INLINE void _jl_timing_counts_pause(jl_timing_counts_t *block, uint64_t t) JL_NOTSAFEPOINT {
#ifdef JL_DEBUG_BUILD
    assert(block->running);
    block->running = 0;
#endif
    block->total += t - block->start;
}

STATIC_INLINE void _jl_timing_counts_resume(jl_timing_counts_t *block, uint64_t t) JL_NOTSAFEPOINT {
#ifdef JL_DEBUG_BUILD
    assert(!block->running);
    block->running = 1;
#endif
    block->start = t;
}

STATIC_INLINE void _jl_timing_counts_start(jl_timing_counts_t *block, uint64_t t) JL_NOTSAFEPOINT {
    block->total = 0;
    block->start = t;
    block->t0 = t;
#ifdef JL_DEBUG_BUILD
    block->running = 1;
#endif
}

STATIC_INLINE void _jl_timing_counts_stop(jl_timing_block_t *block, uint64_t t) JL_NOTSAFEPOINT {
#ifdef JL_DEBUG_BUILD
    assert(block->counts_ctx.running);
    block->counts_ctx.running = 0;
#endif
    jl_timing_counts_event_t *event = block->event->counts_event;
    block->counts_ctx.total += t - block->counts_ctx.start;
    jl_atomic_fetch_add_relaxed(&event->self, block->counts_ctx.total);
    jl_atomic_fetch_add_relaxed(&event->total, t - block->counts_ctx.t0);
}

#endif // USE_TIMING_COUNTS

JL_DLLEXPORT jl_timing_event_t *_jl_timing_event_create(const char *subsystem, const char *name, const char *function, const char *file, int line, int color) {
    int maybe_subsystem = get_timing_subsystem(subsystem);
    if (maybe_subsystem >= JL_TIMING_SUBSYSTEM_LAST) {
        jl_errorf("invalid timing subsystem name: %s", subsystem);
        return NULL;
    }

    jl_timing_event_t *event = (jl_timing_event_t *) malloc(sizeof(jl_timing_event_t));
    event->subsystem = maybe_subsystem;

#ifdef USE_TIMING_COUNTS
    event->counts_event = _jl_timing_counts_event_create(name);
#endif // USE_TIMING_COUNTS

#ifdef USE_ITTAPI
    event->ittapi_event = _jl_timing_ittapi_event_create(name);
#endif // USE_ITTAPI

#ifdef USE_TRACY
    event->tracy_srcloc.name = name;
    event->tracy_srcloc.function = function;
    event->tracy_srcloc.file = file;
    event->tracy_srcloc.line = line;
    event->tracy_srcloc.color = color;
#endif // USE_TRACY

    return event;
}

JL_DLLEXPORT void _jl_timing_block_init(char *buf, size_t size, jl_timing_event_t *event) {
    if (size < sizeof(jl_timing_block_t)) {
        jl_errorf("jl_timing_block_t buffer must be at least %d bytes", sizeof(jl_timing_block_t));
        return;
    }

    jl_timing_block_t *block = (jl_timing_block_t *)buf;
    memset(block, 0, sizeof(jl_timing_block_t));
    block->event = event;
}

JL_DLLEXPORT void _jl_timing_block_start(jl_timing_block_t *block) {
    assert(!block->is_running);
    if (!_jl_timing_enabled(block->event->subsystem)) return;
    if (jl_get_pgcstack() == NULL) return; // not setup on this thread

    uint64_t t = cycleclock(); (void)t;
    _COUNTS_START(&block->counts_ctx, t);
    _ITTAPI_START(block);
    _TRACY_START(block);

    jl_timing_block_t **prevp = &jl_current_task->ptls->timing_stack;
    block->prev = *prevp;
    block->is_running = 1;
    if (block->prev) {
        _COUNTS_PAUSE(&block->prev->counts_ctx, t);
    }
    *prevp = block;
}

JL_DLLEXPORT void _jl_timing_block_end(jl_timing_block_t *block) {
    if (block->is_running) {
        uint64_t t = cycleclock(); (void)t;
        _ITTAPI_STOP(block);
        _TRACY_STOP(block->tracy_ctx);
        _COUNTS_STOP(block, t);

        jl_task_t *ct = jl_current_task;
        jl_timing_block_t **pcur = &ct->ptls->timing_stack;
        assert(*pcur == block);
        *pcur = block->prev;
        if (block->prev) {
            _COUNTS_RESUME(&block->prev->counts_ctx, t);
        }
    }
}

jl_timing_block_t *jl_timing_block_pop(jl_timing_block_t *cur_block)
{
    jl_timing_block_end(cur_block);
    return cur_block->prev;
}

void jl_timing_block_task_enter(jl_task_t *ct, jl_ptls_t ptls, jl_timing_block_t *prev_blk)
{
    if (prev_blk != NULL) {
        assert(ptls->timing_stack == NULL);

        ptls->timing_stack = prev_blk;
        if (prev_blk != NULL) {
            _COUNTS_RESUME(&prev_blk->counts_ctx, cycleclock());
        }
    }

#ifdef USE_TRACY
    TracyCFiberEnter(ct->name);
#else
    (void)ct;
#endif
}

jl_timing_block_t *jl_timing_block_task_exit(jl_task_t *ct, jl_ptls_t ptls)
{
#ifdef USE_TRACY
    // Tracy is fairly strict about not leaving a fiber that hasn't
    // been entered, which happens often when connecting to a running
    // Julia session.
    //
    // Eventually, Tracy will support telling the server which fibers
    // are active upon connection, but until then we work around the
    // problem by not explicitly leaving the fiber at all.
    //
    // Later when we enter the new fiber directly, that will cause the
    // the active fiber to be left implicitly.

    //TracyCFiberLeave;
#endif
    (void)ct;

    jl_timing_block_t *blk = ptls->timing_stack;
    ptls->timing_stack = NULL;

    if (blk != NULL) {
        _COUNTS_PAUSE(&blk->counts_ctx, cycleclock());
    }
    return blk;
}

JL_DLLEXPORT void jl_timing_show(jl_value_t *v, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    ios_t buf;
    ios_mem(&buf, IOS_INLSIZE);
    buf.growable = 0; // Restrict to inline buffer to avoid allocation

    jl_static_show((JL_STREAM*)&buf, v);
    if (buf.size == buf.maxsize)
        memset(&buf.buf[IOS_INLSIZE - 3], '.', 3);

    TracyCZoneText(cur_block->tracy_ctx, buf.buf, buf.size);
#endif
}

JL_DLLEXPORT void jl_timing_show_module(jl_module_t *m, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    jl_module_t *root = jl_module_root(m);
    if (root == m || root == jl_main_module) {
        const char *module_name = jl_symbol_name(m->name);
        TracyCZoneText(cur_block->tracy_ctx, module_name, strlen(module_name));
    } else {
        jl_timing_printf(cur_block, "%s.%s", jl_symbol_name(root->name), jl_symbol_name(m->name));
    }
#endif
}

JL_DLLEXPORT void jl_timing_show_filename(const char *path, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    const char *filename = gnu_basename(path);
    TracyCZoneText(cur_block->tracy_ctx, filename, strlen(filename));
#endif
}

JL_DLLEXPORT void jl_timing_show_location(const char *file, int line, jl_module_t* mod, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    jl_module_t *root = jl_module_root(mod);
    if (root == mod || root == jl_main_module) {
        jl_timing_printf(cur_block, "%s:%d in %s",
                         gnu_basename(file),
                         line,
                         jl_symbol_name(mod->name));
    } else {
        // TODO: generalize to print the entire module hierarchy
        jl_timing_printf(cur_block, "%s:%d in %s.%s",
                         gnu_basename(file),
                         line,
                         jl_symbol_name(root->name),
                         jl_symbol_name(mod->name));
    }
#endif
}

JL_DLLEXPORT void jl_timing_show_method_instance(jl_method_instance_t *mi, jl_timing_block_t *cur_block)
{
    jl_timing_show_func_sig(mi->specTypes, cur_block);
    if (jl_is_method(mi->def.value)) {
        jl_method_t *def = mi->def.method;
        jl_timing_show_location(jl_symbol_name(def->file), def->line, def->module, cur_block);
    } else {
        jl_timing_printf(cur_block, "<top-level thunk> in %s",
                         jl_symbol_name(mi->def.module->name));
    }
}

JL_DLLEXPORT void jl_timing_show_method(jl_method_t *method, jl_timing_block_t *cur_block)
{
    jl_timing_show((jl_value_t *)method, cur_block);
    jl_timing_show_location(jl_symbol_name(method->file), method->line, method->module, cur_block);
}

JL_DLLEXPORT void jl_timing_show_func_sig(jl_value_t *v, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    ios_t buf;
    ios_mem(&buf, IOS_INLSIZE);
    buf.growable = 0; // Restrict to inline buffer to avoid allocation

    jl_static_show_config_t config = { /* quiet */ 1 };
    jl_static_show_func_sig_((JL_STREAM*)&buf, v, config);
    if (buf.size == buf.maxsize)
        memset(&buf.buf[IOS_INLSIZE - 3], '.', 3);

    TracyCZoneText(cur_block->tracy_ctx, buf.buf, buf.size);
#endif
}

JL_DLLEXPORT void jl_timing_show_macro(jl_method_instance_t *macro, jl_value_t* lno, jl_module_t* mod, jl_timing_block_t *cur_block)
{
    jl_timing_printf(cur_block, "%s", jl_symbol_name(macro->def.method->name));
    assert(jl_typetagis(lno, jl_linenumbernode_type));
    jl_timing_show_location(jl_symbol_name((jl_sym_t*)jl_fieldref(lno, 1)),
                            jl_unbox_int64(jl_fieldref(lno, 0)),
                            mod, cur_block);
}

JL_DLLEXPORT void jl_timing_printf(jl_timing_block_t *cur_block, const char *format, ...)
{
    va_list args;
    va_start(args, format);

#ifdef USE_TRACY
    ios_t buf;
    ios_mem(&buf, IOS_INLSIZE);
    buf.growable = 0; // Restrict to inline buffer to avoid allocation

    jl_vprintf((JL_STREAM*)&buf, format, args);
    if (buf.size == buf.maxsize)
        memset(&buf.buf[IOS_INLSIZE - 3], '.', 3);

    TracyCZoneText(cur_block->tracy_ctx, buf.buf, buf.size);
#endif
    va_end(args);
}

JL_DLLEXPORT void jl_timing_puts(jl_timing_block_t *cur_block, const char *str)
{
#ifdef USE_TRACY
    TracyCZoneText(cur_block->tracy_ctx, str, strlen(str));
#endif
}

void jl_timing_task_init(jl_task_t *t)
{
#ifdef USE_TRACY
    jl_value_t *start_type = jl_typeof(t->start);
    const char *start_name = "";
    if (jl_is_datatype(start_type))
        start_name = jl_symbol_name(((jl_datatype_t *) start_type)->name->name);

    static uint16_t task_id = 1;

    // XXX: Tracy uses this as a handle internally and requires that this
    // string live forever, so this allocation is intentionally leaked.
    char *fiber_name;
    if (start_name[0] == '#') {
        jl_method_instance_t *mi = jl_method_lookup(&t->start, 1, jl_get_world_counter());
        const char *filename = gnu_basename(jl_symbol_name(mi->def.method->file));
        const char *module_name = jl_symbol_name(mi->def.method->module->name);

        // 26 characters in "Task 65535 (:0000000 in )\0"
        size_t fiber_name_len = strlen(filename) + strlen(module_name) + 26;
        fiber_name = (char *)malloc(fiber_name_len);
        snprintf(fiber_name, fiber_name_len,  "Task %d (%s:%d in %s)",
                 task_id++, filename, mi->def.method->line, module_name);
    } else {

        // 16 characters in "Task 65535 (\"\")\0"
        size_t fiber_name_len = strlen(start_name) + 16;
        fiber_name = (char *)malloc(fiber_name_len);
        snprintf(fiber_name, fiber_name_len,  "Task %d (\"%s\")",
                 task_id++, start_name);
    }

    t->name = fiber_name;
#endif
}

JL_DLLEXPORT int jl_timing_set_enable(const char *subsystem, uint8_t enabled)
{
    int i = get_timing_subsystem(subsystem);
    if (i >= JL_TIMING_SUBSYSTEM_LAST)
        return -1;

    uint64_t subsystem_bit = 1ul << (i % (sizeof(uint64_t) * CHAR_BIT));
    if (enabled) {
        jl_atomic_fetch_and_relaxed(jl_timing_disable_mask + (i / (sizeof(uint64_t) * CHAR_BIT)), ~subsystem_bit);
    } else {
        jl_atomic_fetch_or_relaxed(jl_timing_disable_mask + (i / (sizeof(uint64_t) * CHAR_BIT)), subsystem_bit);
    }
    return 0;
}

static void jl_timing_set_enable_from_env(void)
{
    const char *env = getenv("JULIA_TIMING_SUBSYSTEMS");
    if (!env)
        return;

    // Copy `env`, so that we can modify it
    size_t sz = strlen(env) + 1;
    char *env_copy = (char *)malloc(sz);
    memcpy(env_copy, env, sz);

    char *subsystem = env_copy;
    char *ch = subsystem;
    uint8_t enable = 1;
    while (1) {
        // +SUBSYSTEM means enable, -SUBSYSTEM means disable
        if (*subsystem == '+' || *subsystem == '-')
            enable = (*subsystem++ == '+');

        if (*ch == ',') {
            *ch++ = '\0';
            if ((*subsystem != '\0') && jl_timing_set_enable(subsystem, enable))
                fprintf(stderr, "warning: unable to configure timing for non-existent subsystem \"%s\"\n", subsystem);

            subsystem = ch;
            enable = 1;
        }
        else if (*ch == '\0') {
            if ((*subsystem != '\0') && jl_timing_set_enable(subsystem, enable))
                fprintf(stderr, "warning: unable to configure timing for non-existent subsystem \"%s\"\n", subsystem);

            break;
        }
        else ch++;
    }
    free(env_copy);
}

static void jl_timing_set_print_limit_from_env(void)
{
    const char *const env = getenv("JULIA_TIMING_METADATA_PRINT_LIMIT");
    if (!env)
        return;

    char *endp;
    long value = strtol(env, &endp, 10);
    if (*endp == '\0' && value >= 0 && value <= UINT32_MAX)
        jl_timing_print_limit = (uint32_t)value;
}

void jl_timing_apply_env(void)
{
    // JULIA_TIMING_SUBSYSTEMS
    jl_timing_set_enable_from_env();

    // JULIA_TIMING_METADATA_PRINT_LIMIT
    jl_timing_set_print_limit_from_env();
}

#else

void jl_init_timing(void) { }
void jl_destroy_timing(void) { }

JL_DLLEXPORT jl_timing_event_t *_jl_timing_event_create(const char *subsystem, const char *name, const char *function, const char *file, int line, int color) { return NULL; }

JL_DLLEXPORT void _jl_timing_block_init(char *buf, size_t size, jl_timing_event_t *event) { }
JL_DLLEXPORT void _jl_timing_block_start(jl_timing_block_t *block) { }
JL_DLLEXPORT void _jl_timing_block_end(jl_timing_block_t *block) { }

JL_DLLEXPORT int jl_timing_set_enable(const char *subsystem, uint8_t enabled) { return -1; }
JL_DLLEXPORT uint32_t jl_timing_print_limit = 0;

#endif

#ifdef __cplusplus
}
#endif
