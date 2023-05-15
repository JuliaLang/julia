// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "options.h"
#include "stdio.h"

jl_module_t *jl_module_root(jl_module_t *m);

#ifdef __cplusplus
extern "C" {
#endif

#ifdef ENABLE_TIMINGS

#ifndef HAVE_TIMING_SUPPORT
#error Timings are not supported on your compiler
#endif

static uint64_t t0;
#if defined(USE_TRACY) || defined(USE_ITTAPI)
/**
 * These sources often generate millions of events / minute. Although Tracy
 * can generally keep up with that, those events also bloat the saved ".tracy"
 * files, so we disable them by default.
 **/
JL_DLLEXPORT uint64_t jl_timing_enable_mask = ~((1ull << JL_TIMING_ROOT) |
                                              (1ull << JL_TIMING_TYPE_CACHE_LOOKUP) |
                                              (1ull << JL_TIMING_METHOD_MATCH) |
                                              (1ull << JL_TIMING_METHOD_LOOKUP_FAST) |
                                              (1ull << JL_TIMING_AST_COMPRESS) |
                                              (1ull << JL_TIMING_AST_UNCOMPRESS));
#else
JL_DLLEXPORT uint64_t jl_timing_enable_mask = ~0ull;
#endif

JL_DLLEXPORT uint64_t jl_timing_counts[(int)JL_TIMING_LAST] = {0};

// Used to as an item limit when several strings of metadata can
// potentially be associated with a single timing zone.
JL_DLLEXPORT uint32_t jl_timing_print_limit = 10;

const char *jl_timing_names[(int)JL_TIMING_LAST] =
    {
#define X(name) #name,
        JL_TIMING_OWNERS
#undef X
    };

#ifdef USE_ITTAPI
JL_DLLEXPORT __itt_event jl_timing_ittapi_events[(int)JL_TIMING_EVENT_LAST];
#endif

void jl_print_timings(void)
{
    uint64_t total_time = cycleclock() - t0;
    uint64_t root_time = total_time;
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        root_time -= jl_timing_counts[i];
    }
    jl_timing_counts[0] = root_time;
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        if (jl_timing_counts[i] != 0)
            fprintf(stderr, "%-25s : %5.2f %%   %" PRIu64 "\n", jl_timing_names[i],
                    100 * (((double)jl_timing_counts[i]) / total_time), jl_timing_counts[i]);
    }
}

void jl_init_timing(void)
{
    t0 = cycleclock();

    _Static_assert(JL_TIMING_EVENT_LAST < sizeof(uint64_t) * CHAR_BIT, "Too many timing events!");
    _Static_assert((int)JL_TIMING_LAST <= (int)JL_TIMING_EVENT_LAST, "More owners than events!");

    int i __attribute__((unused)) = 0;
#ifdef USE_ITTAPI
#define X(name) jl_timing_ittapi_events[i++] = __itt_event_create(#name, strlen(#name));
    JL_TIMING_EVENTS
#undef X
#endif
}

void jl_destroy_timing(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_timing_block_t *stack = ptls->timing_stack;
    while (stack) {
        _jl_timing_block_destroy(stack);
        stack = stack->prev;
    }
}

jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block)
{
    _jl_timing_block_destroy(cur_block);
    return cur_block->prev;
}

void jl_timing_block_enter_task(jl_task_t *ct, jl_ptls_t ptls, jl_timing_block_t *prev_blk)
{
    if (prev_blk != NULL) {
        assert(ptls->timing_stack == NULL);

        ptls->timing_stack = prev_blk;
        if (prev_blk != NULL) {
            _COUNTS_START(&prev_blk->counts_ctx, cycleclock());
        }
    }

#ifdef USE_TRACY
    TracyCFiberEnter(ct->name);
#else
    (void)ct;
#endif
}

jl_timing_block_t *jl_timing_block_exit_task(jl_task_t *ct, jl_ptls_t ptls)
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
        _COUNTS_STOP(&blk->counts_ctx, cycleclock());
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

    TracyCZoneText(*(cur_block->tracy_ctx), buf.buf, buf.size);
#endif
}

JL_DLLEXPORT void jl_timing_show_module(jl_module_t *m, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    jl_module_t *root = jl_module_root(m);
    if (root == m || root == jl_main_module) {
        const char *module_name = jl_symbol_name(m->name);
        TracyCZoneText(*(cur_block->tracy_ctx), module_name, strlen(module_name));
    } else {
        jl_timing_printf(cur_block, "%s.%s", jl_symbol_name(root->name), jl_symbol_name(m->name));
    }
#endif
}

JL_DLLEXPORT void jl_timing_show_filename(const char *path, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    const char *filename = gnu_basename(path);
    TracyCZoneText(*(cur_block->tracy_ctx), filename, strlen(filename));
#endif
}

JL_DLLEXPORT void jl_timing_show_method_instance(jl_method_instance_t *mi, jl_timing_block_t *cur_block)
{
    jl_timing_show_func_sig(mi->specTypes, cur_block);
    jl_method_t *def = mi->def.method;
    jl_timing_printf(cur_block, "%s:%d in %s",
                     gnu_basename(jl_symbol_name(def->file)),
                     def->line,
                     jl_symbol_name(def->module->name));
}

JL_DLLEXPORT void jl_timing_show_method(jl_method_t *method, jl_timing_block_t *cur_block)
{
    jl_timing_show((jl_value_t *)method, cur_block);
    jl_timing_printf(cur_block, "%s:%d in %s",
                    gnu_basename(jl_symbol_name(method->file)),
                    method->line,
                    jl_symbol_name(method->module->name));
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

    TracyCZoneText(*(cur_block->tracy_ctx), buf.buf, buf.size);
#endif
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

    TracyCZoneText(*(cur_block->tracy_ctx), buf.buf, buf.size);
#endif
    va_end(args);
}

JL_DLLEXPORT void jl_timing_puts(jl_timing_block_t *cur_block, const char *str)
{
#ifdef USE_TRACY
    TracyCZoneText(*(cur_block->tracy_ctx), str, strlen(str));
#endif
}

void jl_timing_init_task(jl_task_t *t)
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
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        if (strcmp(subsystem, jl_timing_names[i]) == 0) {
            uint64_t subsystem_bit = (1ul << i);
            if (enabled) {
                jl_timing_enable_mask |= subsystem_bit;
            } else {
                jl_timing_enable_mask &= ~subsystem_bit;
            }
            return 0;
        }
    }
    return -1;
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
JL_DLLEXPORT int jl_timing_set_enable(const char *subsystem, uint8_t enabled) { return -1; }
JL_DLLEXPORT uint32_t jl_timing_print_limit = 0;

#endif

#ifdef __cplusplus
}
#endif
