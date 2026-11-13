// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Tiered execution: interpreter + compiler.

  With tiering enabled (JULIA_TIERED=1), a method that is reached through
  dynamic dispatch and has no compiled code yet starts out in the interpreter
  instead of being inferred and compiled. Two independent counters decide when
  code moves to the compiled tier:

  - a budget per MethodInstance of statements interpreted in its frames
    (jl_tier_work_threshold, 20000). Once it is spent, the specialization is compiled
    through the normal pipeline the next time it is dispatched to. Each specialization
    has its own budget: a rarely reached, cheap specialization of a much-called
    method stays interpreted, and is compiled as part of its callers instead.

  - a budget per interpreted frame of statements executed, checked at
    back-edges (jl_tier_loop_threshold, 20000), which compiles a continuation for the
    outermost loop around the hot back-edge and transfers the frame state into
    it (see interpreter.c and base/tiered.jl).

  A call can only be hot inside a hot loop or a hot recursion. The second
  budget covers loops in interpreted code; the first covers recursion, whose
  recursive calls are dispatches that find the compiled code once the budget is
  spent, and calls that compiled code makes through dynamic dispatch.

  The first budget is also at most 300 statements per statement of the method's lowered
  code: small methods, which are cheap to compile and are interpreted a call at a time,
  compile sooner, and nothing compiles later than it otherwise would. Interpreting a statement costs about what compiling a lowered statement costs divided
  by 300 to 500, so this is about where compiling a small method pays for itself.

  Counters only ever decide *when* something gets compiled, never *what*: the
  code generated for a MethodInstance is exactly what the normal pipeline
  produces, so steady-state performance is unchanged by construction.
*/

#include <stdlib.h>
#include <string.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int jl_tier_enabled = 0;
JL_DLLEXPORT uint32_t jl_tier_work_threshold = 20000;
JL_DLLEXPORT uint32_t jl_tier_loop_threshold = 20000;
static uint32_t tier_small_cost = 300; // the cap on a small method's budget (see above)

void jl_init_tiered(void)
{
    const char *e = getenv("JULIA_TIERED");
    if (jl_options.tiered != 0)
        jl_tier_enabled = jl_options.tiered > 0;
    else
        jl_tier_enabled = e != NULL && (strcmp(e, "1") == 0 || strcmp(e, "yes") == 0 || strcmp(e, "true") == 0);
    // Tiering changes how code executes, not what it computes; instrumentation
    // that observes how code executes stays on the compiled path.
    if (jl_options.code_coverage != JL_LOG_NONE || jl_options.malloc_log != JL_LOG_NONE)
        jl_tier_enabled = 0;
}

// The statement budget of a specialization of `m` (see the top of this file).
static uint32_t tier_budget(jl_method_t *m) JL_NOTSAFEPOINT
{
    uint32_t budget = jl_tier_work_threshold;
    // at most this many statements per lowered statement: once the method's size is known
    // (after its first call), small methods compile sooner, and nothing compiles later
    uint32_t n = tier_small_cost != 0 ? jl_atomic_load_relaxed(&m->tier_nstmts) : 0;
    if (n != 0 && (uint64_t)n * tier_small_cost < budget)
        budget = n * tier_small_cost;
    return budget;
}

JL_DLLEXPORT void jl_tier_set_small_cost(uint32_t v) JL_NOTSAFEPOINT
{
    tier_small_cost = v;
}

JL_DLLEXPORT uint32_t jl_tier_get_small_cost(void) JL_NOTSAFEPOINT
{
    return tier_small_cost;
}

JL_DLLEXPORT int jl_tier_is_enabled(void) JL_NOTSAFEPOINT
{
    return jl_tier_enabled;
}

JL_DLLEXPORT void jl_tier_set_enabled(int on) JL_NOTSAFEPOINT
{
    jl_tier_enabled = on != 0;
}

JL_DLLEXPORT uint32_t jl_tier_get_threshold(int which) JL_NOTSAFEPOINT
{
    return which == 0 ? jl_tier_work_threshold : jl_tier_loop_threshold;
}

JL_DLLEXPORT void jl_tier_set_threshold(int which, uint32_t v) JL_NOTSAFEPOINT
{
    if (which == 0)
        jl_tier_work_threshold = v;
    else
        jl_tier_loop_threshold = v;
}

// Does `a`, a statement or an argument of a call, name `name`: as a symbol (as in
// `getproperty(Core.Intrinsics, :llvmcall)`, which `jl_code_requires_compiler` does not
// see), as a global, as the function itself (which macros like `@muladd` splice in), or
// as the module a function belongs to (as for the `Base.FastMath` functions)?
static int tier_names(jl_value_t *a, const char *name) JL_NOTSAFEPOINT
{
    if (jl_is_quotenode(a))
        a = jl_quotenode_value(a);
    else if (jl_is_globalref(a))
        a = (jl_value_t*)jl_globalref_name(a);
    if (jl_is_symbol(a))
        return strcmp(jl_symbol_name((jl_sym_t*)a), name) == 0;
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(a);
    if (jl_is_datatype(t) && t->instance == a) {
        const char *fname = jl_symbol_name(t->name->name);
        return (fname[0] == '#' && strcmp(fname + 1, name) == 0) ||
               strcmp(jl_symbol_name(t->name->module->name), name) == 0;
    }
    return 0;
}

// Does statement `st` name `name`, itself or in a call's arguments?
static int tier_stmt_names(jl_value_t *st, const char *name) JL_NOTSAFEPOINT
{
    if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_call_sym) {
        jl_expr_t *e = (jl_expr_t*)st;
        for (size_t i = 0; i < jl_expr_nargs(e); i++) {
            if (tier_names(jl_exprarg(e, i), name))
                return 1;
        }
        return 0;
    }
    return tier_names(st, name);
}

// Does `src` need the compiler beyond what `jl_code_requires_compiler` checks? Code
// that uses `@fastmath` or `muladd` does: compiled, its operations may be fused or reassociated with
// one another (e.g. a multiply and an add into one rounding), and whether they are
// depends on what is inlined into what, so the code must be compiled as it would be
// without tiering, together with its callees, not interpreted with them compiled
// separately. Code that names `llvmcall`, which the interpreter cannot run, is caught
// here too.
//
// Code using `@inbounds`, `@boundscheck` or `@propagate_inbounds` is interpreted like any
// other: the interpreter performs every bounds check, which `@inbounds` permits, so the only
// difference is that an out-of-bounds access throws.
static int tier_needs_compiler(jl_code_info_t *src) JL_NOTSAFEPOINT
{
    jl_array_t *code = src->code;
    size_t n = jl_array_nrows(code);
    for (size_t i = 0; i < n; i++) {
        jl_value_t *st = jl_array_ptr_ref(code, i);
        if (tier_stmt_names(st, "llvmcall") || tier_stmt_names(st, "FastMath") ||
            tier_stmt_names(st, "muladd"))
            return 1;
    }
    return 0;
}

// Can `m` run in the interpreter at all? Computed once per Method, from the
// lowered source, so the answer never depends on run history.
static int tier_method_interpretable(jl_method_t *m) JL_CANSAFEPOINT
{
    uint8_t flags = jl_atomic_load_relaxed(&m->tier_flags);
    if (!(flags & JL_TIER_CHECKED)) {
        jl_value_t *src = jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)&m->source);
        int needs = 1;
        if (src != NULL && src != jl_nothing) {
            JL_GC_PUSH1(&src);
            if (!jl_is_code_info(src))
                src = (jl_value_t*)jl_uncompress_ir(m, NULL, src);
            needs = jl_code_requires_compiler((jl_code_info_t*)src, 1) ||
                    tier_needs_compiler((jl_code_info_t*)src);
            size_t n = jl_array_nrows(((jl_code_info_t*)src)->code);
            jl_atomic_store_relaxed(&m->tier_nstmts, n == 0 ? 1 : n < UINT32_MAX ? (uint32_t)n : UINT32_MAX);
            JL_GC_POP();
        }
        flags = JL_TIER_CHECKED | (needs ? JL_TIER_NEEDS_COMPILER : 0);
        jl_atomic_fetch_or_relaxed(&m->tier_flags, flags);
    }
    return !(flags & JL_TIER_NEEDS_COMPILER);
}

// Whatever the budget, may a call to `mi` run in the interpreter? Returns JL_TIER_INTERPRET
// if so, and otherwise why it must be compiled.
static int tier_may_interpret(jl_method_instance_t *mi) JL_CANSAFEPOINT
{
    if (!jl_is_method(mi->def.value))
        return JL_TIER_WHY_OTHER;
    jl_method_t *m = mi->def.method;
    // Precompilation exists to compile ahead of time: bypass the gate.
    if (jl_generating_output())
        return JL_TIER_WHY_PRECOMPILE;
    // So do measurements (`@allocated`, `@allocations`), which ask for the compiled tier.
    if (jl_current_task->tier_compiled)
        return JL_TIER_WHY_MEASUREMENT;
    // And so does tracing compilation (`--trace-compile`, `@trace_compile`), whose output
    // is used to precompile what a session runs: it must list everything that runs.
    if (jl_options.trace_compile != NULL || jl_atomic_load_relaxed(&jl_force_trace_compile_timing_enabled))
        return JL_TIER_WHY_TRACE;
    if (m->source == NULL || m->source == jl_nothing || m->is_for_opaque_closure)
        return JL_TIER_WHY_OTHER;
    // For an optionally generated function the compiler runs the generator, while
    // the interpreter would run the fallback body.
    if (m->generator != NULL)
        return JL_TIER_WHY_GENERATOR;
    if (jl_options.compile_enabled != JL_OPTIONS_COMPILE_DEFAULT)
        return JL_TIER_WHY_OTHER;
    int mod_compile = jl_get_module_compile(m->module);
    if (mod_compile == JL_OPTIONS_COMPILE_OFF || mod_compile == JL_OPTIONS_COMPILE_MIN)
        return JL_TIER_WHY_OTHER; // handled by the existing --compile=min machinery
    if (jl_get_module_infer(m->module) == 0)
        return JL_TIER_WHY_OTHER;
    // The interpreter takes static parameter values from the MethodInstance;
    // a widened specialization may not know them all.
    jl_svec_t *sp = mi->sparam_vals;
    for (size_t i = 0; i < jl_svec_len(sp); i++) {
        if (jl_is_typevar(jl_svecref(sp, i)))
            return JL_TIER_WHY_OTHER;
    }
    if (!tier_method_interpretable(m))
        return JL_TIER_WHY_NEEDS_COMPILER;
    return JL_TIER_INTERPRET;
}

// The tier gate, consulted on dynamic dispatch to a MethodInstance with no
// compiled code: should this call run in the interpreter? Returns JL_TIER_INTERPRET if so,
// and otherwise why it must be compiled.
int jl_tier_gate(jl_method_instance_t *mi) JL_CANSAFEPOINT
{
    int why = tier_may_interpret(mi);
    if (why != JL_TIER_INTERPRET)
        return why;
    jl_method_t *m = mi->def.method;
    // Budget per specialization, in statements interpreted: a call can only be hot
    // inside a hot loop, which loop continuations compile together with the calls
    // they resolve statically, or inside a hot recursion, whose recursive calls are
    // dispatches that find the compiled code once the budget is spent. Cheap
    // specializations reached rarely stay interpreted; expensive ones compile soon.
    uint32_t budget = tier_budget(m);
    uint32_t work = jl_atomic_load_relaxed(&mi->tier_work);
    if (work >= budget)
        return JL_TIER_WHY_BUDGET;
    // The call itself counts as a statement, which also charges calls whose frames
    // unwind before they can add their work (see eval_body).
    work = jl_atomic_fetch_add_relaxed(&mi->tier_work, 1);
    return work < budget ? JL_TIER_INTERPRET : JL_TIER_WHY_BUDGET;
}

// Add statements interpreted in a frame of `mi` to its budget, saturating.
void jl_tier_add_work(jl_method_instance_t *mi, uint64_t n) JL_NOTSAFEPOINT
{
    uint32_t budget = tier_budget(mi->def.method);
    uint32_t work = jl_atomic_load_relaxed(&mi->tier_work);
    if (work >= budget)
        return;
    jl_atomic_fetch_add_relaxed(&mi->tier_work, n < budget ? (uint32_t)n : budget);
}

JL_DLLEXPORT uint32_t jl_tier_mi_work(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&mi->tier_work);
}

// Run the dynamic extent of a measurement in the compiled tier: code reached from it
// through dynamic dispatch is compiled as without tiering. Returns the previous state,
// to pass to jl_tier_compiled_leave.
JL_DLLEXPORT uint8_t jl_tier_compiled_enter(void) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    uint8_t old = ct->tier_compiled;
    ct->tier_compiled = 1;
    return old;
}

JL_DLLEXPORT void jl_tier_compiled_leave(uint8_t old) JL_NOTSAFEPOINT
{
    jl_current_task->tier_compiled = old;
}

// Mark that loop continuations have been cached for some specialization of
// `m`; interpreted frames of `m` then check for one on entry to a loop.
JL_DLLEXPORT void jl_tier_note_loop_cache(jl_method_t *m) JL_NOTSAFEPOINT
{
    jl_atomic_fetch_or_relaxed(&m->tier_flags, JL_TIER_HAS_LOOPS);
}

// Wrap an existing continuation method in an OpaqueClosure for the current
// world. The compiled code is found in the method's cache while it is valid,
// so this only re-infers after an invalidation.
JL_DLLEXPORT jl_value_t *jl_tier_continuation_in_world(jl_value_t *argt, jl_value_t *rt, jl_value_t *source) JL_CANSAFEPOINT
{
    return (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)argt, jl_bottom_type, rt, source, NULL, 0, 1);
}

#ifdef __cplusplus
}
#endif
