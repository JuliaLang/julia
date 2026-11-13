// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Tiered execution: interpreter + compiler.

  With tiering enabled (JULIA_TIERED=1), a method that is reached through
  dynamic dispatch and has no compiled code yet starts out in the interpreter
  instead of being inferred and compiled. Two independent counters decide when
  code moves to the compiled tier:

  - a budget per MethodInstance of statements interpreted in its frames
    (jl_tier_work_threshold, 20000). Once it is spent, the specialization is compiled
    through the normal pipeline, by a dedicated compile thread, while it goes on running
    interpreted (see "Background compilation" below). Each specialization
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

  Compiling a function normally compiles, in the same batch, every function it calls
  statically that has no code yet, and their callees in turn, including those on paths that
  never run, such as error handling. Without tiering most of them have code already, since
  everything that has run is compiled. With tiering, code that ran interpreted has none, and
  the first compile that reaches it statically compiles all of it at once. So when the
  function being compiled has run interpreted (a budget compile or a loop continuation), the
  batch only takes in callees that have run (see jl_tier_lazy_callees); calls to the others
  go through a stub that dispatches to them like the interpreter does (jl_invoke_tiered), so
  they are interpreted or compiled by their own budgets if they ever run. Once a callee is
  compiled, the stub is pointed at its code (see linkTierStubs in jitlayers.cpp), and calls
  to it cost an indirect jump more than without tiering. Inlining is decided before any of
  this, so the code compiled for each function is otherwise unchanged.
*/

#include <stdlib.h>
#include <string.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int jl_tier_enabled = 0;
JL_DLLEXPORT uint32_t jl_tier_work_threshold = 20000;
JL_DLLEXPORT uint32_t jl_tier_loop_threshold = 20000;
static uint32_t tier_small_cost = 300; // the cap on a small method's budget (see above)
static int tier_background = 1;        // off only in tests, and if the compile thread can't start
static uv_mutex_t tier_queue_mutex;    // the background compile queue (see below)
static uv_cond_t tier_queue_cond;
static __thread int tier_compiling_continuation = 0; // in jl_tier_continuation_in_world

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
    uv_mutex_init(&tier_queue_mutex);
    uv_cond_init(&tier_queue_cond);
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

// Code being compiled in the background is interpreted until it is ready, unless it can't be.
int jl_tier_interpretable(jl_method_instance_t *mi) JL_CANSAFEPOINT
{
    return tier_may_interpret(mi) == JL_TIER_INTERPRET;
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
    if (work >= budget) {
        // queued for a background compile, and still interpreted until it is done: keep
        // counting, since how fast the count grows decides its place in the queue
        if (jl_atomic_load_relaxed(&mi->flags) & MI_FLAG_TIER_QUEUED) {
            uint32_t room = UINT32_MAX - work;
            jl_atomic_fetch_add_relaxed(&mi->tier_work, n < room ? (uint32_t)n : room);
        }
        return;
    }
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
    tier_compiling_continuation++;
    jl_value_t *oc = (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)argt, jl_bottom_type, rt, source, NULL, 0, 1);
    tier_compiling_continuation--;
    return oc;
}

// Whether `mi` has run: interpreted, or compiled code for some world.
static int tier_has_run(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    if (jl_atomic_load_relaxed(&mi->tier_work) > 0)
        return 1;
    for (jl_code_instance_t *ci = jl_atomic_load_relaxed(&mi->cache); ci != NULL;
         ci = jl_atomic_load_relaxed(&ci->next)) {
        if (jl_atomic_load_relaxed(&ci->invoke) != NULL)
            return 1;
    }
    return 0;
}

// Asked by the compiler (`add_codeinsts_to_jit!`) for each callee `mi` it would compile
// together with `root`: should calls to it go through a stub instead (see the top of this
// file)? Only when `root` has run interpreted, so that there is evidence of which of its
// callees run, and `mi` has not run at all. A root compiled because it must be (it needs
// the compiler, or is a generated function) has no such evidence, and compiles its callees.
JL_DLLEXPORT int jl_tier_lazy_callees(jl_method_instance_t *root, jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    if (!jl_tier_enabled || jl_generating_output())
        return 0;
    int root_ran = jl_atomic_load_relaxed(&root->tier_work) > 0 ||
        (tier_compiling_continuation && jl_is_method(root->def.value) &&
         root->def.method->is_for_opaque_closure);
    return root_ran && !tier_has_run(mi);
}

// Background compilation. A specialization whose
// budget is spent is not compiled by the thread that finds it so: it is queued, and keeps
// running interpreted until a dedicated compile thread has compiled it, after which dispatch
// finds the compiled code as usual. Compiling still takes as long, but a cell that triggers
// a compile no longer waits for it.
//
// The compile thread is an OS thread the runtime starts on the first compile queued, adopted
// into the runtime (inference runs Julia code), in no threadpool: user tasks never run on it.
// Idle threads of the default pool help, taking jobs when they run out of tasks
// (jl_tier_steal_task).
// It takes the queued specialization whose interpreted work has grown most since it was
// queued (the queue keeps its work counter running), so that hot code compiles first.
//
// The queue mutex is a uv mutex, paired with a condition variable so that the compile thread
// can wait without spinning, and a thread blocked on it waits GC-unsafe: it is never held
// across a safepoint, so the queue is malloc'd, holding MethodInstances rooted through their
// methods' specializations. (After JuliaLang/julia#61888, whose compile thread this follows.)
//
// Loop continuations compile in the background too: a hot loop queues a job
// (Base.Tiered.ContinuationJob) and keeps running interpreted, entering the continuation once
// it is ready (see _tier_loop). The queue holds the job unrooted, as it does MethodInstances;
// the loop's cache entry keeps it alive until it is done. Continuation jobs are taken first,
// since a hot loop is waiting for each.
typedef struct {
    jl_method_instance_t *mi; // the specialization to compile, or NULL for a continuation
    jl_value_t *cont;         // the continuation job, if `mi` is NULL
    uint32_t work0; // its work counter when queued
} tier_job_t;
static tier_job_t *tier_jobs;
static size_t tier_njobs, tier_jobs_cap;
static _Atomic(size_t) tier_queued = 0; // tier_njobs, for checks without the queue mutex
static uv_thread_t tier_worker_thread;
static _Atomic(int) tier_worker_started = 0;
static _Atomic(int) tier_worker_stop = 0;
static _Atomic(int) tier_pause = 0; // holders of jl_tier_quiesce
static _Atomic(int) tier_busy = 0;  // compiles in flight on the compile thread
static __thread int tier_on_worker = 0; // compiling from the queue, on this thread
extern _Atomic(int) n_threads_running;

int jl_tier_background_on(void) JL_NOTSAFEPOINT
{
    return tier_background && !jl_atomic_load_relaxed(&tier_worker_stop) && !jl_generating_output();
}

JL_DLLEXPORT void jl_tier_set_background(int on) JL_NOTSAFEPOINT
{
    tier_background = on != 0;
}

JL_DLLEXPORT int jl_tier_get_background(void) JL_NOTSAFEPOINT
{
    return tier_background;
}

static void tier_start_worker(void) JL_NOTSAFEPOINT;

// Queue `mi`, whose budget is spent, for the compile thread, unless it is queued already.
// Returns whether help is wanted: it was queued while a compile is in progress, so an idle
// thread of the default pool could take it (jl_tier_steal_task).
int jl_tier_enqueue(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    uint8_t prior = jl_atomic_fetch_or_relaxed(&mi->flags, MI_FLAG_TIER_QUEUED);
    if (prior & (MI_FLAG_TIER_QUEUED | MI_FLAG_TIER_FAILED))
        return 0;
    uv_mutex_lock(&tier_queue_mutex);
    if (tier_njobs == tier_jobs_cap) {
        size_t cap = tier_jobs_cap ? 2 * tier_jobs_cap : 64;
        tier_job_t *jobs = (tier_job_t*)realloc_s(tier_jobs, cap * sizeof(tier_job_t));
        tier_jobs = jobs;
        tier_jobs_cap = cap;
    }
    tier_jobs[tier_njobs].mi = mi;
    tier_jobs[tier_njobs].cont = NULL;
    tier_jobs[tier_njobs].work0 = jl_atomic_load_relaxed(&mi->tier_work);
    tier_njobs++;
    jl_atomic_store_relaxed(&tier_queued, tier_njobs);
    uv_mutex_unlock(&tier_queue_mutex);
    uv_cond_signal(&tier_queue_cond);
    if (!jl_atomic_load_relaxed(&tier_worker_started))
        tier_start_worker();
    return jl_atomic_load_relaxed(&tier_busy) != 0;
}

// Queue the continuation job `job` (from Base.Tiered._tier_loop), which is queued once per loop
// and live-in types.
JL_DLLEXPORT void jl_tier_enqueue_continuation(jl_value_t *job) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&tier_queue_mutex);
    if (tier_njobs == tier_jobs_cap) {
        size_t cap = tier_jobs_cap ? 2 * tier_jobs_cap : 64;
        tier_job_t *jobs = (tier_job_t*)realloc_s(tier_jobs, cap * sizeof(tier_job_t));
        tier_jobs = jobs;
        tier_jobs_cap = cap;
    }
    tier_jobs[tier_njobs].mi = NULL;
    tier_jobs[tier_njobs].cont = job;
    tier_jobs[tier_njobs].work0 = 0;
    tier_njobs++;
    jl_atomic_store_relaxed(&tier_queued, tier_njobs);
    uv_mutex_unlock(&tier_queue_mutex);
    uv_cond_signal(&tier_queue_cond);
    if (!jl_atomic_load_relaxed(&tier_worker_started))
        tier_start_worker();
    if (jl_atomic_load_relaxed(&tier_busy) != 0)
        jl_wakeup_threadpool(JL_THREADPOOL_ID_DEFAULT); // to take it, if one is idle
}

// Whether a compile from the queue is in progress on a thread other than this one, which
// may be compiling code this thread is about to call along with what was queued.
int jl_tier_compiling_elsewhere(void) JL_NOTSAFEPOINT
{
    return !tier_on_worker && jl_atomic_load_relaxed(&tier_busy) != 0;
}

// The earliest continuation job, or else the job whose work has grown most since it was
// queued, the earliest of equals. Called with the queue mutex held, and the queue not empty.
static tier_job_t tier_take_locked(void) JL_NOTSAFEPOINT
{
    size_t best = 0;
    uint32_t most = 0;
    for (size_t i = 0; i < tier_njobs; i++) {
        if (tier_jobs[i].mi == NULL) {
            best = i;
            break;
        }
        uint32_t grown = jl_atomic_load_relaxed(&tier_jobs[i].mi->tier_work) - tier_jobs[i].work0;
        if (i == 0 || grown > most) {
            best = i;
            most = grown;
        }
    }
    tier_job_t job = tier_jobs[best];
    memmove(&tier_jobs[best], &tier_jobs[best + 1], (tier_njobs - best - 1) * sizeof(tier_job_t));
    tier_njobs--;
    jl_atomic_store_relaxed(&tier_queued, tier_njobs);
    return job;
}

// The compile thread counts as running (in n_threads_running) only while it compiles: while
// it waits for work it must not, or `jl_task_wait_empty` would wait for it forever, and it
// must while it compiles, or that would proceed with a compile in flight. As the scheduler
// does when a thread sleeps (jl_task_get_next), the thread that brings the count to zero
// wakes thread 0, which may be waiting for exactly that; and incrementing happens under
// thread 0's sleep_lock, where the scheduler's wakeup expects it. Lock order: the queue
// mutex, then thread 0's sleep_lock.
static void tier_worker_park(void) JL_NOTSAFEPOINT
{
    if (jl_atomic_fetch_add_relaxed(&n_threads_running, -1) == 1) {
        jl_ptls_t ptls0 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
        uv_mutex_lock(&ptls0->sleep_lock);
        uv_cond_signal(&ptls0->wake_signal);
        uv_mutex_unlock(&ptls0->sleep_lock);
    }
}

static void tier_worker_unpark(void) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls0 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    uv_mutex_lock(&ptls0->sleep_lock);
    jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
    uv_mutex_unlock(&ptls0->sleep_lock);
}

// Compile `mi` on the compile thread, as `_jl_invoke_gated` would have on the thread that
// spent its budget.
static void tier_compile_background(jl_method_instance_t *mi) JL_CANSAFEPOINT
{
    jl_code_instance_t *ci = NULL;
    JL_GC_PUSH1(&ci);
    jl_task_t *ct = jl_current_task;
    size_t world = jl_get_world_counter();
    ct->world_age = world;
    if (jl_method_compiled(mi, world) != NULL) {
        jl_atomic_fetch_and_relaxed(&mi->flags, (uint8_t)~MI_FLAG_TIER_QUEUED);
        JL_GC_POP();
        return;
    }
    JL_TRY {
        ci = jl_compile_method_internal(mi, world);
    }
    JL_CATCH {
        ci = NULL;
    }
    // Done: if it is invalidated later, it is interpreted again and queued again once its
    // budget is spent. If the compile failed, it stays interpreted. (Code another thread is
    // still compiling, as part of a batch, is not a failure: that thread finishes it.)
    if (ci == NULL || jl_atomic_load_relaxed(&ci->invoke) == NULL)
        jl_atomic_fetch_or_relaxed(&mi->flags, MI_FLAG_TIER_FAILED);
    jl_atomic_fetch_and_relaxed(&mi->flags, (uint8_t)~MI_FLAG_TIER_QUEUED);
    JL_GC_POP();
}

// Compile a continuation job on the compile thread (Base.Tiered._tier_compile_job), as
// _tier_loop would have on the thread whose loop got hot. It runs compiled, as the compiler
// does, rather than tiered.
static _Atomic(jl_value_t*) tier_cont_fn = NULL;
static void tier_compile_continuation(jl_value_t *job) JL_CANSAFEPOINT
{
    jl_value_t *fn = jl_atomic_load_acquire(&tier_cont_fn);
    if (fn == NULL) {
        fn = jl_get_global(jl_base_module, jl_symbol("_tier_compile_job"));
        if (fn == NULL)
            return;
        jl_atomic_store_release(&tier_cont_fn, fn);
    }
    jl_task_t *ct = jl_current_task;
    ct->world_age = jl_get_world_counter();
    uint8_t compiled = ct->tier_compiled;
    ct->tier_compiled = 1;
    JL_TRY {
        jl_value_t *args[2] = { fn, job };
        jl_apply(args, 2);
    }
    JL_CATCH {
    }
    ct->tier_compiled = compiled;
}

// Run a job taken from the queue.
static void tier_run_job(tier_job_t job) JL_CANSAFEPOINT
{
    if (job.mi != NULL)
        tier_compile_background(job.mi);
    else
        tier_compile_continuation(job.cont);
}

// The compile thread alternates waiting for work, GC-safe, and compiling, GC-unsafe, which
// the safepoint analysis cannot follow.
static void tier_worker(void *arg) JL_NO_SAFEPOINT_ANALYSIS
{
    (void)arg;
    jl_adopt_thread();
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    tier_on_worker = 1;
    // user finalizers run on user threads, not here
    jl_gc_enable_finalizers(ct, 0);
    while (1) {
        // wait, GC-safe, until there is work (or the thread is to stop)
        int8_t gc_state = jl_gc_safe_enter(ptls);
        uv_mutex_lock(&tier_queue_mutex);
        while (!jl_atomic_load_relaxed(&tier_worker_stop) &&
               (jl_atomic_load_relaxed(&tier_pause) || tier_njobs == 0)) {
            tier_worker_park();
            uv_cond_wait(&tier_queue_cond, &tier_queue_mutex);
            tier_worker_unpark();
        }
        int stop = jl_atomic_load_relaxed(&tier_worker_stop);
        uv_mutex_unlock(&tier_queue_mutex);
        if (stop)
            return; // counted as running: the thread's exit uncounts it
        jl_gc_safe_leave(ptls, gc_state);
        // Take the job GC-unsafe, after that safepoint, so that it is rooted before the next
        // one. (Until then, it is reachable from its method's specializations.) The critical
        // section has no safepoint, as everywhere the queue mutex is held.
        tier_job_t job = {NULL, NULL, 0};
        int taken = 0;
        uv_mutex_lock(&tier_queue_mutex);
        if (!jl_atomic_load_relaxed(&tier_pause) && tier_njobs != 0) {
            job = tier_take_locked();
            taken = 1;
            // under the queue mutex, so that jl_tier_quiesce sees either this or the pause
            jl_atomic_fetch_add_relaxed(&tier_busy, 1);
        }
        uv_mutex_unlock(&tier_queue_mutex);
        if (!taken)
            continue;
        JL_GC_PUSH2(&job.mi, &job.cont);
        tier_run_job(job);
        JL_GC_POP();
        jl_atomic_fetch_add_relaxed(&tier_busy, -1);
    }
}

static void tier_start_worker(void) JL_NOTSAFEPOINT
{
    if (jl_atomic_exchange_relaxed(&tier_worker_started, 1))
        return;
    if (uv_thread_create(&tier_worker_thread, tier_worker, NULL) != 0) {
        // compile on the thread that spends the budget, as without background compiling
        tier_background = 0;
        jl_safe_printf("WARNING: could not start the tiered compile thread\n");
    }
}

// Work stealing: a thread of the default pool that has run out of tasks, and would otherwise
// spin and sleep in the scheduler, is handed a task of its own that compiles queued jobs until
// none are left (Base.Tiered._tier_steal), rather than compiling on the stack of whatever
// task it was waiting in, whose wakeup a wait inside a compile could take. Threads of the
// interactive pool never compile, nor does the main thread, which is in it.
static __thread int tier_stealing = 0; // a stealing task of this thread is running
static _Atomic(jl_value_t*) tier_steal_fn = NULL;

jl_task_t *jl_tier_steal_task(jl_ptls_t ptls) JL_CANSAFEPOINT
{
    if (jl_atomic_load_relaxed(&tier_queued) == 0 || jl_atomic_load_relaxed(&tier_pause) ||
        tier_stealing || !jl_tier_background_on())
        return NULL;
    if (jl_threadpoolid(ptls->tid) != JL_THREADPOOL_ID_DEFAULT)
        return NULL;
    jl_value_t *fn = jl_atomic_load_acquire(&tier_steal_fn);
    if (fn == NULL) {
        if (jl_base_module == NULL)
            return NULL;
        fn = jl_get_global(jl_base_module, jl_symbol("_tier_steal"));
        if (fn == NULL)
            return NULL;
        jl_atomic_store_release(&tier_steal_fn, fn);
    }
    tier_stealing = 1;
    jl_task_t *t = jl_new_task(fn, jl_nothing, 0);
    // run it here, where there is nothing else to run
    t->sticky = 1;
    jl_atomic_store_relaxed(&t->tid, ptls->tid);
    return t;
}

// Take a job from the queue and compile it, on the stealing task. Returns whether it did.
JL_DLLEXPORT int jl_tier_steal_one(void) JL_CANSAFEPOINT
{
    if (jl_atomic_load_relaxed(&tier_pause))
        return 0;
    tier_job_t job = {NULL, NULL, 0};
    int taken = 0;
    uv_mutex_lock(&tier_queue_mutex);
    if (!jl_atomic_load_relaxed(&tier_pause) && tier_njobs != 0) {
        job = tier_take_locked();
        taken = 1;
        jl_atomic_fetch_add_relaxed(&tier_busy, 1);
    }
    uv_mutex_unlock(&tier_queue_mutex);
    if (!taken)
        return 0;
    JL_GC_PUSH2(&job.mi, &job.cont);
    tier_on_worker = 1;
    tier_run_job(job);
    tier_on_worker = 0;
    JL_GC_POP();
    jl_atomic_fetch_add_relaxed(&tier_busy, -1);
    return 1;
}

JL_DLLEXPORT void jl_tier_steal_done(void) JL_NOTSAFEPOINT
{
    tier_stealing = 0;
}

// Pause the compile thread, and wait for the compile in flight. Measurements that read
// process-wide counters (`@allocated`, `@time`) do this, since the compile thread allocates
// and compiles concurrently with what they measure. Pauses nest; jl_tier_resume ends one.
JL_DLLEXPORT void jl_tier_quiesce(void) JL_CANSAFEPOINT
{
    jl_atomic_fetch_add(&tier_pause, 1);
    // the compile thread itself (a generator run while compiling) has nothing to wait for
    if (!jl_atomic_load_relaxed(&tier_worker_started) || tier_on_worker)
        return;
    // after this, the compile thread has either seen the pause or counted itself busy
    uv_mutex_lock(&tier_queue_mutex);
    uv_mutex_unlock(&tier_queue_mutex);
    while (jl_atomic_load_acquire(&tier_busy) != 0) {
        jl_gc_safepoint(); // the compile in flight may need a collection to finish
        jl_cpu_pause();
    }
}

JL_DLLEXPORT void jl_tier_resume(void) JL_NOTSAFEPOINT
{
    if (jl_atomic_fetch_add(&tier_pause, -1) != 1)
        return;
    if (jl_atomic_load_relaxed(&tier_worker_started)) {
        uv_mutex_lock(&tier_queue_mutex);
        uv_cond_broadcast(&tier_queue_cond);
        uv_mutex_unlock(&tier_queue_mutex);
    }
}

// Stop the compile thread for good, before the runtime is torn down (jl_atexit_hook):
// finish the compile in flight, leave the rest of the queue, and join the thread.
JL_DLLEXPORT void jl_tier_stop_worker(void) JL_CANSAFEPOINT
{
    if (jl_atomic_exchange(&tier_worker_stop, 1) || !jl_atomic_load_relaxed(&tier_worker_started))
        return;
    uv_mutex_lock(&tier_queue_mutex);
    uv_cond_broadcast(&tier_queue_cond);
    uv_mutex_unlock(&tier_queue_mutex);
    // join GC-safe: the compile in flight may need a collection to finish
    jl_task_t *ct = jl_get_current_task();
    if (ct != NULL) {
        int8_t gc_state = jl_gc_safe_enter(ct->ptls);
        uv_thread_join(&tier_worker_thread);
        jl_gc_safe_leave(ct->ptls, gc_state);
    }
    else {
        uv_thread_join(&tier_worker_thread);
    }
}

#ifdef __cplusplus
}
#endif
