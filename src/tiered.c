// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tiered compilation runtime.
//
// Implements the one-shot enqueue CAS protocol, a bounded FIFO queue,
// the worker-pop entry point, and the writer-side promotion swap.
// See contrib/tiered_compilation_plan.md.
//
// Who enqueues: promotion candidates enter via `jl_tier_enqueue` /
// `jl_tier_enqueue_mi` from exactly two sites, both running on a normal
// Julia thread with a live task (not from frameless JIT'd code — compiled
// code never enqueues, and there is no codegen-emitted prologue):
//   - the interpreter entry `jl_fptr_interpret_call` (interpreter.c), which
//     probes every interpreted call, and
//   - the dispatch cache-miss path in `jl_mt_assoc_by_type` (gf.c), when a
//     method is parked into the T0 interpreter tier.
//
// Why enqueue stays NOTSAFEPOINT and allocation-free: not because the callers
// forbid it (both are in GC-unsafe state and the gf.c site allocates a
// CodeInstance one line earlier), but because of how `tier_queue_mutex` is
// waited on. It is a `uv_mutex_t` — it has to be, since it is paired with
// `tier_queue_cond` so the worker can *block* while idle instead of spinning.
// A thread blocked in `uv_mutex_lock` waits GC-unsafe, so if the lock holder
// hit a safepoint while another thread was blocked acquiring the lock,
// stop-the-world would deadlock on that blocked thread. The critical section
// must therefore be safepoint-free, hence allocation-free, hence the queue is
// a raw malloc-backed arraylist rather than a GC-managed array. (A
// `jl_mutex_t` safepoints while waiting and would lift this restriction, but
// then the worker could not block on a uv_cond and would have to spin.)
//
// Single-writer invariant: for any given `mi`, the `fetch_or` on `mi->flags`
// (the JL_MI_FLAGS_TIER_QUEUED bit) lets at most one caller observe the
// "first-to-set" outcome — the entire promotion pipeline downstream relies on
// it. Tier state lives on the MethodInstance; the CodeInstance keeps none.
//
// Worker wakeup is a plain uv_cond_signal on `tier_queue_cond` (paired with
// `tier_queue_mutex`); the worker is a dedicated, runtime-adopted OS thread
// (see tier_worker_threadfun) the kernel schedules preemptively, so promotion
// progresses regardless of Julia's thread count or the main thread's yield
// behavior — a libuv async wakeup would instead require the io-loop (main)
// thread to be idle, and a Julia-task worker only runs when a pool thread
// reaches a yield point.
//
// GC rooting: the queue holds raw `jl_method_instance_t *` without adding a GC
// root. A queued MI is reachable from the (never-freed) method table as a
// specialization, so the raw pointer stays valid until popped. The T0
// interpreter stub CI is EPHEMERAL — never inserted into mi->cache and never
// queued — so it is collected after the dispatch that created it; promotion
// re-derives everything from the MI (jl_compile_method_internal). Opaque-closure
// methods are the one GC-collectable jl_method_t kind, so their MIs are refused
// at enqueue.

#include "julia.h"
#include "julia_internal.h"
#include "julia_atomics.h"
#include "support/arraylist.h"
#include "threading.h" // jl_all_tls_states (worker park accounting)
#include "uv.h"

#ifdef __cplusplus
extern "C" {
#endif

// Logging control. 0 = silent (default), 1 = log every first-time enqueue.
// Toggled via the `JULIA_TIER_DEBUG` env var at startup, or by the test
// helper `jl_tier_set_debug`. Plain int — racy reads are acceptable for a
// debug toggle.
static int tier_debug = -1;

static int tier_debug_enabled(void) JL_NOTSAFEPOINT
{
    int d = tier_debug;
    if (__builtin_expect(d < 0, 0)) {
        const char *env = getenv("JULIA_TIER_DEBUG");
        d = (env && env[0] && env[0] != '0') ? 1 : 0;
        tier_debug = d;
    }
    return d;
}

JL_DLLEXPORT void jl_tier_set_debug(int enabled) JL_NOTSAFEPOINT
{
    tier_debug = enabled ? 1 : 0;
}

// Statistics for tests. Atomic counters, no synchronization with anything
// else. Read with `jl_tier_get_stats`.
static _Atomic(uint64_t) tier_enqueue_calls = 0;
static _Atomic(uint64_t) tier_enqueue_wins = 0;
static _Atomic(uint64_t) tier_queue_pushes = 0;
static _Atomic(uint64_t) tier_queue_pops = 0;
static _Atomic(uint64_t) tier_queue_drops = 0; // enqueue arrived before init
static _Atomic(uint64_t) tier_swaps = 0;       // successful promotions

JL_DLLEXPORT uint64_t jl_tier_get_swaps(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&tier_swaps);
}


// Master enable gate. T0 is the AST interpreter (methods are parked
// before inference; see the gates in gf.c); the promotion work (T1
// recompiles at the session opt level) runs on a dedicated,
// runtime-adopted OS thread (Base.start_tier_worker). Default: enabled
// iff the machine has at least two effective CPUs, so the worker has a
// spare CPU to run on. Force either way with JULIA_TIER_ENABLE=1/0.
static _Atomic(int) tier_enabled = -1;

JL_DLLEXPORT int jl_tier_enabled(void) JL_NOTSAFEPOINT
{
    int v = jl_atomic_load_relaxed(&tier_enabled);
    if (__builtin_expect(v < 0, 0)) {
        const char *env = getenv("JULIA_TIER_ENABLE");
        if (env && env[0])
            v = (env[0] != '0');
        else
            v = jl_effective_threads() >= 2;
        const char *wenv = getenv("JULIA_TIER_WORKER");
        if (v && wenv && (wenv[0] == '0' || wenv[0] == 'f' || wenv[0] == 'F' ||
                          wenv[0] == 'n' || wenv[0] == 'N'))
            v = 0;
        // Sysimage bootstrap (--output-o with no --output-ji): no Base, no
        // worker thread, nothing can ever promote a parked frame.
        if (jl_options.outputo != NULL && jl_options.outputji == NULL)
            v = 0;
        // Coverage and allocation tracking are codegen-only instrumentation;
        // interpreted frames record nothing.
        if (jl_options.code_coverage || jl_options.malloc_log)
            v = 0;
        jl_atomic_store_relaxed(&tier_enabled, v);
    }
    return v;
}

// Interpreter-T0 for LOOP-bearing bodies (JULIA_TIER_INTERP_LOOPS, default
// on). With it, the pre-inference gate parks loopy methods in the
// interpreter like everything else, but enqueues them for promotion
// IMMEDIATELY at first call instead of waiting for the entry threshold:
// the worker runs their inference + codegen off the main thread (the
// promote-by-compile path), overlapping the first interpreted execution,
// and later calls dispatch to the compiled CodeInstance. This moves the
// dominant remaining cold-start cost (measured: loop roots accounted for
// ~95% of main-thread root-inference time) off the critical path. A loop
// that runs very long on its FIRST invocation escapes the interpreter via
// on-stack replacement (the back-edge work budget below; see
// jl_tier_set_osr_hook), but the budget plus continuation compile still
// cost tens of milliseconds — workloads dominated by run-once long-running
// kernels can set JULIA_TIER_INTERP_LOOPS=0 to restore the
// compile-loops-eagerly policy.
static _Atomic(int) tier_interp_loops = -1; // -1 = uninitialized

JL_DLLEXPORT int jl_tier_interp_loops_enabled(void) JL_NOTSAFEPOINT
{
    int v = jl_atomic_load_relaxed(&tier_interp_loops);
    if (__builtin_expect(v < 0, 0)) {
        const char *env = getenv("JULIA_TIER_INTERP_LOOPS");
        v = (env && env[0]) ? (env[0] != '0') : 1;
        jl_atomic_store_relaxed(&tier_interp_loops, v);
    }
    return v;
}

JL_DLLEXPORT void jl_tier_set_interp_loops(int enabled) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&tier_interp_loops, enabled ? 1 : 0);
}

// On-stack replacement for interpreted loops (Truffle-style). The
// interpreter counts STATEMENTS EXECUTED per frame — a work budget
// weighted by loop body size, so big bodies escape after proportionally
// fewer iterations — and checks it at back-edges; at
// JULIA_TIER_OSR_THRESHOLD statements (default 131072, roughly tens of
// milliseconds of interpretation; 0 disables) it calls the hook
// registered here (a Base @cfunction) with (src, mi, target_ip, state)
// where `state` is a snapshot of the frame's slots + ssavalues. The hook
// synthesizes (and caches) a compiled continuation that executes the
// rest of the function starting at the back-edge target, called with the
// live values — the heap-frame analogue of Truffle's
// BytecodeOSRNode.tryOSR. It returns jl_nothing to decline (the frame
// keeps interpreting and stops asking) or Some(result), which the
// interpreter returns as the frame's result.
static _Atomic(void*) tier_osr_hook = NULL;
static _Atomic(int) tier_osr_threshold = -1; // -1 = uninitialized

JL_DLLEXPORT void jl_tier_set_osr_hook(void *f) JL_NOTSAFEPOINT
{
    jl_atomic_store_release(&tier_osr_hook, f);
}

JL_DLLEXPORT void *jl_tier_get_osr_hook(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_acquire(&tier_osr_hook);
}

JL_DLLEXPORT uint32_t jl_tier_get_osr_threshold(void) JL_NOTSAFEPOINT
{
    int v = jl_atomic_load_relaxed(&tier_osr_threshold);
    if (__builtin_expect(v < 0, 0)) {
        const char *env = getenv("JULIA_TIER_OSR_THRESHOLD");
        long parsed = env ? strtol(env, NULL, 10) : 131072;
        if (parsed < 0) parsed = 0;
        if (parsed > 0x7fffffff) parsed = 0x7fffffff;
        v = (int)parsed;
        jl_atomic_store_relaxed(&tier_osr_threshold, v);
    }
    return (uint32_t)v;
}

// Hotness threshold. T0 functions call `jl_tier_enqueue_mi` on
// every entry; the MethodInstance is pushed onto the promotion queue after its
// per-MI call count (mi->tier_count) reaches this threshold. Default 10. Tune
// via the `JULIA_TIER_THRESHOLD` env var or `jl_tier_set_threshold`.
static _Atomic(uint32_t) tier_threshold = 0; // 0 = uninitialized sentinel

JL_DLLEXPORT uint32_t jl_tier_get_threshold(void) JL_NOTSAFEPOINT
{
    uint32_t t = jl_atomic_load_relaxed(&tier_threshold);
    if (__builtin_expect(t == 0, 0)) {
        const char *env = getenv("JULIA_TIER_THRESHOLD");
        long parsed = env ? strtol(env, NULL, 10) : 10; // default threshold
        if (parsed < 1) parsed = 1;
        if (parsed > 0x7fffffff) parsed = 0x7fffffff;
        t = (uint32_t)parsed;
        jl_atomic_store_relaxed(&tier_threshold, t);
    }
    return t;
}

JL_DLLEXPORT void jl_tier_set_threshold(uint32_t n) JL_NOTSAFEPOINT
{
    if (n < 1) n = 1;
    jl_atomic_store_relaxed(&tier_threshold, n);
}

// Declared here (used by jl_tier_init below); defined/used by the stopgap
// loop-detection cache further down.
static uv_mutex_t tier_loop_mutex;

// Returns the post-increment T0 call count for `mi`. The count lives directly on
// the MethodInstance (mi->tier_count), so this is an exact, lock-free atomic
// increment on the interpreted hot path — no side table, no mutex, no
// hash-collision eviction that could delay a promotion. Relaxed ordering is fine:
// the count is a promotion heuristic, and the one-shot enqueue CAS
// (tier_enqueue_mi_locked) provides the actual single-winner guarantee.
static uint32_t tier_bump_count(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    return jl_atomic_fetch_add_relaxed(&mi->tier_count, 1) + 1;
}

JL_DLLEXPORT void jl_tier_get_stats(uint64_t *calls, uint64_t *wins) JL_NOTSAFEPOINT
{
    if (calls) *calls = jl_atomic_load_relaxed(&tier_enqueue_calls);
    if (wins)  *wins  = jl_atomic_load_relaxed(&tier_enqueue_wins);
}

JL_DLLEXPORT void jl_tier_get_queue_stats(uint64_t *pushes, uint64_t *pops, uint64_t *drops) JL_NOTSAFEPOINT
{
    if (pushes) *pushes = jl_atomic_load_relaxed(&tier_queue_pushes);
    if (pops)   *pops   = jl_atomic_load_relaxed(&tier_queue_pops);
    if (drops)  *drops  = jl_atomic_load_relaxed(&tier_queue_drops);
}

JL_DLLEXPORT void jl_tier_reset_stats(void) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&tier_enqueue_calls, 0);
    jl_atomic_store_relaxed(&tier_enqueue_wins, 0);
    jl_atomic_store_relaxed(&tier_queue_pushes, 0);
    jl_atomic_store_relaxed(&tier_queue_pops, 0);
    jl_atomic_store_relaxed(&tier_queue_drops, 0);
    jl_atomic_store_relaxed(&tier_swaps, 0);
}

// Queue protected by a raw uv_mutex (NOT a Julia lock) so it can be taken
// from JIT-emitted code without any Julia frame on the stack.
// `tier_initialized` is set last during init with a release store; readers
// load it with acquire and only then touch the mutex/queue.
static _Atomic(int) tier_initialized = 0;
static uv_mutex_t tier_queue_mutex;
static arraylist_t tier_queue;
// Signaled (with tier_queue_mutex held by the waiter's predicate check)
// on every push, on quiesce resume, and on worker stop. The worker
// re-checks the queue under the mutex before waiting, so wakeups cannot
// be lost.
static uv_cond_t tier_queue_cond;

// Initialize the queue. Idempotent. Called once from `julia_init` before any
// jl_tier_enqueue can win (codegen wiring is in 3d), but we don't rely on
// that — `jl_tier_enqueue` checks `tier_initialized` and drops otherwise.
JL_DLLEXPORT void jl_tier_init(void) JL_NOTSAFEPOINT
{
    if (jl_atomic_load_acquire(&tier_initialized))
        return;
    uv_mutex_init(&tier_queue_mutex);
    uv_cond_init(&tier_queue_cond);
    uv_mutex_init(&tier_loop_mutex);
    arraylist_new(&tier_queue, 64);
    jl_atomic_store_release(&tier_initialized, 1);
}

// The dedicated worker thread. Created by jl_tier_start_worker (called
// from Base.start_tier_worker once the runtime is fully up) and adopted
// into the runtime so inference/codegen (GC, safepoints, exceptions) work.
// It parks on tier_queue_cond in a GC-safe state while idle and is woken
// directly by jl_tier_enqueue's uv_cond_signal. Being a real OS thread,
// the kernel schedules it preemptively — promotion overlaps the workload
// even with --threads=1 and a main thread that never yields, the way
// HotSpot's and .NET's background compiler threads do.
//
// Stop protocol: jl_tier_stop_worker sets tier_worker_stop and parks the
// worker permanently (it never exits the thread — tearing down an adopted
// thread mid-runtime-shutdown is riskier than leaving it parked GC-safe),
// then quiesces so no promotion is in flight when teardown proceeds.
static uv_thread_t tier_worker_uvthread;
static _Atomic(int) tier_worker_running = 0;
static _Atomic(int) tier_worker_stop = 0;

// Quiesce state (documented with the quiesce machinery below). Defined
// here, not just forward-declared: a second tentative definition is valid
// C but trips -Wc++-compat ("duplicate declaration") on CI.
static _Atomic(int) tier_pause = 0;
static _Atomic(int) tier_busy = 0;

extern _Atomic(int) n_threads_running;

// Park/unpark accounting around the worker's condvar wait. Unlike the GC
// threads (which never run Julia code and uncount themselves permanently),
// this thread really does run Julia code — inference inside
// jl_tier_promote — so it must count in n_threads_running while
// working (jl_task_wait_empty, the --output exit path, waits for that
// count to reach zero and must not proceed under an in-flight promotion)
// and must NOT count while parked (or wait_empty would hang forever on an
// idle worker). This mirrors the scheduler's own sleep accounting
// (jl_task_get_next), including its last-running-thread duty: if our
// decrement is the one that reaches zero, thread 0 may be sleeping while
// waiting on wait_empty and re-checks its condition only when signaled.
static void tier_worker_park(void) JL_NOTSAFEPOINT
{
    int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, -1);
    if (wasrunning == 1) {
        int16_t tid2 = 0;
        jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid2];
        uv_mutex_lock(&ptls2->sleep_lock);
        uv_cond_signal(&ptls2->wake_signal);
        uv_mutex_unlock(&ptls2->sleep_lock);
    }
}

static void tier_worker_unpark(void) JL_NOTSAFEPOINT
{
    // Increment under tid 0's sleep_lock: the scheduler's wait_empty wake
    // path (jl_task_get_next) loads n_threads_running == 0 and then
    // re-increments while holding that lock, asserting no concurrent
    // waker landed in between. A bare increment here can hit exactly that
    // window. Lock order (tier_queue_mutex -> sleep_lock[0]) matches
    // tier_worker_park above.
    int16_t tid2 = 0;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid2];
    uv_mutex_lock(&ptls2->sleep_lock);
    (void)jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
    uv_mutex_unlock(&ptls2->sleep_lock);
}

static void tier_worker_threadfun(void *arg)
{
    (void)arg;
    jl_adopt_thread();
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    while (1) {
        // Wait for work in a GC-safe state (same shape as
        // jl_engine_reserve): stop-the-world never waits on the idle
        // worker, and the GC-unsafe transition (which contains a
        // safepoint) runs only after the mutex is dropped —
        // tier_queue_mutex must never be held across a safepoint, or a
        // blocked enqueuer holding it could deadlock stop-the-world. A
        // stopped worker parks here forever; quiesce-paused ones until resume.
        // Pop up to a small batch under one lock: the batched recompile
        // amortizes the per-module fixed costs (LLVM context, pass
        // pipeline, materialization unit, symbol-pool round trips), which
        // dominate the measured ~3ms/CI promotion cost.
        jl_method_instance_t *batch[16];
        size_t nbatch = 0;
        int8_t gc_state = jl_gc_safe_enter(ptls);
        uv_mutex_lock(&tier_queue_mutex);
        while (jl_atomic_load_relaxed(&tier_worker_stop) ||
               jl_atomic_load_relaxed(&tier_pause) || tier_queue.len == 0) {
            tier_worker_park();
            uv_cond_wait(&tier_queue_cond, &tier_queue_mutex);
            tier_worker_unpark();
        }
        while (nbatch < sizeof(batch) / sizeof(batch[0]) && tier_queue.len > 0)
            batch[nbatch++] = (jl_method_instance_t*)arraylist_pop(&tier_queue);
        // Under the queue mutex, so jl_tier_quiesce's lock/unlock barrier
        // observes either this increment or the pause flag.
        jl_atomic_fetch_add_relaxed(&tier_busy, (int)nbatch);
        uv_mutex_unlock(&tier_queue_mutex);
        jl_gc_safe_leave(ptls, gc_state);
        jl_atomic_fetch_add_relaxed(&tier_queue_pops, nbatch);
        // Each popped MI is reachable through the method table (see the queue
        // note at the top of this file).
        // Promotion must run in the latest world (the adopted root task's
        // world is stale).
        ct->world_age = jl_get_world_counter();
        uint64_t t0 = jl_hrtime();
        for (size_t i = 0; i < nbatch; i++)
            jl_tier_promote(batch[i]);
        jl_tier_add_promote_ns(jl_hrtime() - t0);
        jl_atomic_fetch_add_relaxed(&tier_busy, -(int)nbatch);
    }
}

JL_DLLEXPORT void jl_tier_start_worker(void)
{
    jl_tier_init();
    if (jl_atomic_exchange_relaxed(&tier_worker_running, 1))
        return; // already started
    if (uv_thread_create(&tier_worker_uvthread, tier_worker_threadfun, NULL) != 0) {
        jl_atomic_store_relaxed(&tier_worker_running, 0);
        jl_atomic_store_relaxed(&tier_enabled, 0);
        jl_safe_printf("[tier] failed to create worker thread; "
                       "tiered compilation disabled\n");
        return;
    }
    uv_thread_detach(&tier_worker_uvthread);
}

JL_DLLEXPORT void jl_tier_stop_worker(void)
{
    if (!jl_atomic_load_relaxed(&tier_worker_running))
        return;
    jl_atomic_store_release(&tier_worker_stop, 1);
    if (jl_atomic_load_acquire(&tier_initialized)) {
        uv_mutex_lock(&tier_queue_mutex);
        uv_cond_broadcast(&tier_queue_cond);
        uv_mutex_unlock(&tier_queue_mutex);
    }
    // Wait for any in-flight promotion so runtime teardown never races a
    // compile; the worker then stays parked (GC-safe) until process exit.
    jl_tier_quiesce();
}

// Quiesce support. The worker runs on its own OS thread, truly concurrent
// with everything else — including pkgimage/sysimage serialization, which
// walks and snapshots the same CodeInstances the promotion path mutates
// (invoke/specptr/flags/inferred). Before serializing, jl_tier_quiesce()
// parks the worker: it sets `tier_pause` (the worker stops popping) and
// waits until no popped CI is still being compiled (`tier_busy`, which
// the worker increments under the queue mutex at pop time), so after
// quiesce's lock/unlock barrier every concurrent pop has either observed
// the pause or is accounted for in tier_busy. (`tier_pause`/`tier_busy`
// are defined with the worker state above.)

static jl_method_instance_t *tier_pop_locked(int respect_pause) JL_NOTSAFEPOINT
{
    if (!jl_atomic_load_acquire(&tier_initialized))
        return NULL;
    jl_method_instance_t *mi = NULL;
    uv_mutex_lock(&tier_queue_mutex);
    if (!(respect_pause && jl_atomic_load_relaxed(&tier_pause)) && tier_queue.len > 0)
        mi = (jl_method_instance_t*)arraylist_pop(&tier_queue);
    uv_mutex_unlock(&tier_queue_mutex);
    if (mi != NULL)
        jl_atomic_fetch_add_relaxed(&tier_queue_pops, 1);
    return mi;
}

// Pop one CI synchronously (diagnostics / manual draining; the dedicated
// worker thread pops inline so it can hold the mutex across its busy
// accounting). Returns NULL if empty, paused, or not initialized.
JL_DLLEXPORT jl_method_instance_t *jl_tier_worker_pop(void) JL_NOTSAFEPOINT
{
    return tier_pop_locked(/*respect_pause*/1);
}

// Park the worker and wait for any in-flight promotion to finish.
// NOT safepoint-free: the worker's compile may need a GC to complete, so
// the wait loop must service stop-the-world requests. Must not be called
// while holding codegen/engine locks (the in-flight compile takes them).
//
// `tier_pause` is a COUNT, so quiesce/resume pairs nest: concurrent
// holders (e.g. `@allocated` measurements on several threads, or
// serialization inside an already-paused region) each take and release
// their own hold, and the worker resumes only when the last one releases.
JL_DLLEXPORT void jl_tier_quiesce(void)
{
    jl_atomic_fetch_add(&tier_pause, 1);
    if (!jl_atomic_load_acquire(&tier_initialized))
        return;
    // Barrier: after this, every concurrent pop has either seen the pause
    // or already incremented tier_busy.
    uv_mutex_lock(&tier_queue_mutex);
    uv_mutex_unlock(&tier_queue_mutex);
    while (jl_atomic_load_acquire(&tier_busy) != 0) {
        jl_gc_safepoint();
        jl_cpu_pause();
    }
}

JL_DLLEXPORT void jl_tier_resume(void) JL_NOTSAFEPOINT
{
    int prev = jl_atomic_fetch_add(&tier_pause, -1);
    assert(prev > 0 && "unbalanced jl_tier_resume");
    if (prev != 1)
        return; // another holder keeps the worker paused
    // Re-wake the worker in case work queued up while paused.
    if (jl_atomic_load_acquire(&tier_initialized)) {
        uv_mutex_lock(&tier_queue_mutex);
        uv_cond_broadcast(&tier_queue_cond);
        uv_mutex_unlock(&tier_queue_mutex);
    }
}

// Synchronously drain the tier queue on the calling thread. Used by the
// AOT save path (pkgimage / sysimage) so any in-flight T1 promotion lands
// before the artifact is written: hot CIs end up with their optimized
// specptr baked into the pkgimage instead of the unoptimized T0 code.
//
// Must be called from a normal Julia task — `jl_tier_promote`
// runs full codegen and may take internal Julia locks. We deliberately do
// NOT wait for the asynchronous worker task here; instead we steal its
// work, which guarantees forward progress even if the worker is busy on
// an unrelated CI or has not been scheduled yet.
JL_DLLEXPORT void jl_tier_drain(void)
{
    if (!jl_atomic_load_acquire(&tier_initialized))
        return;
    uint64_t promoted = 0, failed = 0;
    while (1) {
        // Bypass the quiesce pause (drain runs synchronously on the
        // quiescing/saving thread).
        jl_method_instance_t *mi = tier_pop_locked(/*respect_pause*/0);
        if (mi == NULL)
            break;
        // The popped MI stays reachable through the method table (see the queue
        // note at the top of this file), so it is GC-rooted transitively even
        // though it is not on the shadow stack; tell the GC checker.
        JL_GC_PROMISE_ROOTED(mi);
        // Count synchronous drain-path recompile time too (the worker loop
        // times the async path); drain steals most of the work in practice.
        uint64_t t0 = jl_hrtime();
        // The MI's set-once JL_MI_FLAGS_TIER_QUEUED bit (claimed at enqueue,
        // never cleared) keeps it from being re-enqueued regardless of outcome.
        int ok = jl_tier_promote(mi);
        jl_tier_add_promote_ns(jl_hrtime() - t0);
        if (ok)
            promoted++;
        else
            failed++;
    }
    if (tier_debug_enabled()) {
        jl_safe_printf("[tier] drain: %llu promoted, %llu skipped "
                       "(calls=%llu wins=%llu pushes=%llu pops=%llu drops=%llu)\n",
                       (unsigned long long)promoted, (unsigned long long)failed,
                       (unsigned long long)jl_atomic_load_relaxed(&tier_enqueue_calls),
                       (unsigned long long)jl_atomic_load_relaxed(&tier_enqueue_wins),
                       (unsigned long long)jl_atomic_load_relaxed(&tier_queue_pushes),
                       (unsigned long long)jl_atomic_load_relaxed(&tier_queue_pops),
                       (unsigned long long)jl_atomic_load_relaxed(&tier_queue_drops));
    }
}

// Inference run for a promotion is background duplicate work, not a user
// dispatch: diagnostics that attribute inference to its entrance (the
// SnoopCompile hook) must skip it, or background promotions inject
// nondeterministic entries whose backtraces only show the worker loop.
static __thread int tier_in_promotion = 0;

JL_DLLEXPORT int jl_tier_in_promotion(void) JL_NOTSAFEPOINT
{
    return tier_in_promotion;
}

// Promote a MethodInstance from the interpreter (T0) to compiled native code
// (T1) by running the ordinary compile path. The T0 interpreter stub is
// ephemeral (it was never inserted into mi->cache), so there is nothing to
// retire: jl_compile_method_internal inserts a real native CodeInstance into
// mi->cache and dispatch finds it from then on. The MI is reachable through the
// (never-freed) method table, so the raw pointer from the queue is GC-rooted.
JL_DLLEXPORT int jl_tier_promote(jl_method_instance_t *mi)
{
    if (mi == NULL)
        return 0;
    JL_GC_PROMISE_ROOTED(mi);
    jl_code_instance_t *result = NULL;
    tier_in_promotion = 1;
    JL_TRY {
        result = jl_compile_method_internal(mi, jl_get_world_counter());
    }
    JL_CATCH {
        // The QUEUED bit (set at enqueue, never cleared) already blocks re-enqueue,
        // so a failed compile is not retried — nothing to record here.
        tier_in_promotion = 0;
        return 0;
    }
    tier_in_promotion = 0;
    int ok = 0;
    jl_callptr_t got = result == NULL ? NULL : jl_atomic_load_acquire(&result->invoke);
    if (result != NULL && got != NULL && got != jl_fptr_interpret_call_addr) {
        jl_atomic_fetch_add_relaxed(&tier_swaps, 1);
        ok = 1;
        if (tier_debug_enabled())
            jl_safe_printf("[tier] promote mi=%p -> native=%p\n",
                           (void*)mi, (void*)result);
    }
    return ok;
}

// Core MI-keyed enqueue, shared by both entry points. Performs the one-shot CAS
// on the MI QUEUED bit (set once when promotion is first claimed, never cleared):
// the caller that observes it not-yet-set is the unique winner and pushes the MI
// (NOT the ephemeral T0 stub CI) onto the queue. Because QUEUED is never cleared
// it gates ALL future enqueues for this MI — dedup while in flight, and no retry
// after a completed or failed promotion. The MI is rooted by the never-freed
// method table; MI granularity is correct (all CIs of one MI share one promotion).
static void tier_enqueue_mi_locked(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    // Opaque-closure methods are the one GC-collectable jl_method_t kind, so
    // their MIs (and the source they would interpret) can be freed — never tier.
    if (mi == NULL || !jl_is_method(mi->def.method) ||
            mi->def.method->is_for_opaque_closure)
        return;
    jl_atomic_fetch_add_relaxed(&tier_enqueue_calls, 1);
    uint8_t prior = jl_atomic_fetch_or_relaxed(&mi->flags, JL_MI_FLAGS_TIER_QUEUED);
    if (prior & JL_MI_FLAGS_TIER_QUEUED)
        return; // already claimed (in flight, done, or failed) — never re-enqueue
    jl_atomic_fetch_add_relaxed(&tier_enqueue_wins, 1);
    if (tier_debug_enabled()) {
        // jl_safe_printf keeps enqueue allocation-free on the hot path.
        jl_safe_printf("[tier] enqueue mi=%p\n", (void*)mi);
    }
    // Push only if the worker is up. If init hasn't run yet we still treat this
    // as a "win" — the MI's JL_MI_FLAGS_TIER_QUEUED bit stays set so it is never
    // re-enqueued; counted as a drop for visibility.
    if (!jl_atomic_load_acquire(&tier_initialized)) {
        jl_atomic_fetch_add_relaxed(&tier_queue_drops, 1);
        return;
    }
    uv_mutex_lock(&tier_queue_mutex);
    arraylist_push(&tier_queue, mi);
    uv_mutex_unlock(&tier_queue_mutex);
    jl_atomic_fetch_add_relaxed(&tier_queue_pushes, 1);
    // Wake the dedicated worker thread. Signal-after-unlock is safe: the
    // worker re-checks the queue under the mutex before waiting.
    uv_cond_signal(&tier_queue_cond);
}

// "Stopgap": decide whether a code instance must be COMPILED rather than parked
// in the cheap T0 tier (interpreter). A body with a loop runs to completion at
// T0 — the entry counter sees calls, not iterations, so a loop-heavy function
// entered few times never promotes (catastrophic under the interpreter) — and
// the AST interpreter cannot execute a ccall at all. We reuse Julia's own
// compile-vs-interpret policy (`jl_code_info_avoid_interp` -> body_attributes:
// loop / ccall / cfunction / llvmcall / opaque-closure / @force_compile) and
// run it on the method's LOWERED source, which is deterministic (independent of
// the optimizer unrolling constant-bound loops) and identical across the
// method's type specializations.
//
// Classification stats (tests/diagnostics only).
static _Atomic(uint64_t) tier_cls_compile = 0;  // must compile (loop/ccall/opaque/forced)
static _Atomic(uint64_t) tier_cls_interp = 0;    // ok to interpret at T0
static _Atomic(uint64_t) tier_cls_unknown = 0;   // no usable source -> compile (fallback)

JL_DLLEXPORT void jl_tier_get_class_stats(uint64_t *compile, uint64_t *interp, uint64_t *unknown) JL_NOTSAFEPOINT
{
    if (compile) *compile = jl_atomic_load_relaxed(&tier_cls_compile);
    if (interp)  *interp  = jl_atomic_load_relaxed(&tier_cls_interp);
    if (unknown) *unknown = jl_atomic_load_relaxed(&tier_cls_unknown);
}

// Timing (tests/diagnostics): cumulative ns spent (1) decoding+classifying a
// method's source in jl_tier_ci_avoid_interp (the stopgap's overhead), and
// (2) in the background worker promotions (jl_tier_promote),
// reported by the Julia worker via jl_tier_add_promote_ns.
static _Atomic(uint64_t) tier_classify_ns = 0;
static _Atomic(uint64_t) tier_promote_ns = 0;

JL_DLLEXPORT void jl_tier_add_promote_ns(uint64_t ns) JL_NOTSAFEPOINT
{
    jl_atomic_fetch_add_relaxed(&tier_promote_ns, ns);
}

JL_DLLEXPORT void jl_tier_get_timing(uint64_t *classify_ns, uint64_t *promote_ns) JL_NOTSAFEPOINT
{
    if (classify_ns) *classify_ns = jl_atomic_load_relaxed(&tier_classify_ns);
    if (promote_ns)  *promote_ns  = jl_atomic_load_relaxed(&tier_promote_ns);
}

// Per-method decision cache. The decision is a property of the lowered method
// body, identical across the method's type specializations, so we decode/scan
// once per method and later specializations hit the cache without re-decoding.
// Fixed, open-addressed, no allocation; collisions just re-decode. Raw method
// pointers without a GC root (methods are not freed in practice; a stale slot
// only costs a re-decode).
#define TIER_LOOP_BUCKETS 1024
static struct { jl_method_t *m; int8_t reasons; } tier_loop_cache[TIER_LOOP_BUCKETS];

// Returns 1 if `ci` should be compiled rather than interpreted at T0.
// NB: NOT JL_NOTSAFEPOINT — it may jl_uncompress_ir (allocates) and run
// body_attributes (may resolve bindings). Callers must hold a GC root for `ci`
// (jl_mi_cache_insert does).
JL_DLLEXPORT int jl_tier_ci_avoid_interp(jl_code_instance_t *ci)
{
    if (ci == NULL)
        return 1;
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    if (mi == NULL || !jl_is_method(mi->def.method))
        return 1;
    JL_GC_PROMISE_ROOTED(mi);
    return jl_tier_method_avoid_interp(mi->def.method);
}

JL_DLLEXPORT int jl_tier_method_avoid_interp(jl_method_t *m)
{
    return jl_tier_method_interp_reasons(m) != 0;
}

// Method-level core of the classifier: returns the JL_TIER_REJECT_* bitmask
// (0 = interp-eligible). The decision is a property of the method's lowered
// body, identical across its type specializations. Callers must hold a GC
// root for `m`.
JL_DLLEXPORT int jl_tier_method_interp_reasons(jl_method_t *m)
{
    if (m == NULL)
        return JL_TIER_REJECT_NOSOURCE;
    // tier_loop_mutex is initialized in jl_tier_init; match the guard every
    // other tier_* entry point uses before touching tier-owned state.
    if (!jl_atomic_load_acquire(&tier_initialized))
        return JL_TIER_REJECT_NOSOURCE;
    JL_GC_PROMISE_ROOTED(m);

    // @generated methods: m->source is the (optional) fallback body, which may
    // not match the per-signature generated body actually run — that body could
    // contain a ccall/loop the fallback lacks. Conservatively compile.
    if (m->generator != NULL)
        return JL_TIER_REJECT_GENERATED;

    if (m->is_for_opaque_closure)
        return JL_TIER_REJECT_OPAQUE;

    // Per-method decision cache, keyed on the raw method pointer. Opaque-closure
    // methods are the only GC-collectable jl_method_t objects, so caching them by
    // address would risk a stale hit after the address is freed and reused by a
    // different method (which could then be wrongly parked in the interpreter).
    // They are transient anyway, so skip the cache and classify them fresh.
    int use_cache = !m->is_for_opaque_closure;
    uint32_t h = 0;
    if (use_cache) {
        h = (uint32_t)(((uintptr_t)m >> 4) ^ ((uintptr_t)m >> 16)) & (TIER_LOOP_BUCKETS - 1);
        uv_mutex_lock(&tier_loop_mutex);
        int cached = (tier_loop_cache[h].m == m) ? tier_loop_cache[h].reasons : -1;
        uv_mutex_unlock(&tier_loop_mutex);
        if (cached >= 0) {
            jl_atomic_fetch_add_relaxed(cached ? &tier_cls_compile : &tier_cls_interp, 1);
            return cached;
        }
    }

    // Decide from the LOWERED method source (deterministic; not the optimized
    // inferred IR, whose loops may have been unrolled away).
    uint64_t t0 = jl_hrtime();
    int result = -1;
    jl_value_t *msrc = m->source;
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    if (msrc != NULL && jl_is_code_info(msrc))
        src = (jl_code_info_t*)msrc;                       // rooted via the frame slot
    else if (msrc != NULL && jl_is_string(msrc))
        src = jl_uncompress_ir(m, NULL, msrc);
    if (src != NULL && jl_is_code_info(src))
        result = jl_code_info_interp_reject_reasons(src);
    JL_GC_POP();
    jl_atomic_fetch_add_relaxed(&tier_classify_ns, jl_hrtime() - t0);

    if (result < 0) {
        jl_atomic_fetch_add_relaxed(&tier_cls_unknown, 1);
        result = JL_TIER_REJECT_NOSOURCE; // no usable source -> conservatively compile (sticky)
    }
    else {
        jl_atomic_fetch_add_relaxed(result ? &tier_cls_compile : &tier_cls_interp, 1);
    }
    if (use_cache) {
        uv_mutex_lock(&tier_loop_mutex);
        tier_loop_cache[h].m = m;
        tier_loop_cache[h].reasons = (int8_t)result;
        uv_mutex_unlock(&tier_loop_mutex);
    }
    return result;
}

// Root-inference attribution (profiling): when interpreter-T0 is active,
// every inference tree on the critical path is rooted at a
// jl_compile_method_internal that the interp gate declined to park. Count
// those roots and their inference time per rejection reason, to answer
// "where does the remaining cold-start inference come from?". Bucket
// indices: 0..5 = first set JL_TIER_REJECT_* bit (loops, ccall, opaque,
// forced, generated, nosource), 6 = interp-eligible anyway (gate bypassed:
// macro skip-inference path, compile_option, invoke paths), 7 = not a
// method.
#define TIER_ROOT_BUCKETS 8
static _Atomic(uint64_t) tier_root_count[TIER_ROOT_BUCKETS];
static _Atomic(uint64_t) tier_root_ns[TIER_ROOT_BUCKETS];

JL_DLLEXPORT void jl_tier_note_root_infer(jl_method_t *m, uint64_t ns)
{
    int bucket = 7;
    if (m != NULL) {
        int reasons = jl_tier_method_interp_reasons(m);
        if (reasons == 0)
            bucket = 6;
        else {
            bucket = 0;
            while (bucket < 6 && !(reasons & (1 << bucket)))
                bucket++;
        }
    }
    jl_atomic_fetch_add_relaxed(&tier_root_count[bucket], 1);
    jl_atomic_fetch_add_relaxed(&tier_root_ns[bucket], ns);
}

JL_DLLEXPORT void jl_tier_get_root_infer_stats(uint64_t *counts, uint64_t *ns)
{
    for (int i = 0; i < TIER_ROOT_BUCKETS; i++) {
        if (counts) counts[i] = jl_atomic_load_relaxed(&tier_root_count[i]);
        if (ns)     ns[i]     = jl_atomic_load_relaxed(&tier_root_ns[i]);
    }
}

// Codegen-prologue / interpreter-entry enqueue. Called (once per invocation,
// unconditional under threshold=1) from JIT'd T0 function entry blocks and from
// the interpreter entry (jl_fptr_interpret_call). `mi` is the candidate. Keyed
// entirely on the MethodInstance — the T0 stub is ephemeral and not in mi->cache,
// so there is no CI to consult. Must remain JL_NOTSAFEPOINT.
JL_DLLEXPORT void jl_tier_enqueue_mi(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    if (mi == NULL)
        return;
    // Fast-path early-out on the authoritative MI one-shot bits. This is only a
    // hint; the true serialization point is the CAS in tier_enqueue_mi_locked, so
    // a stale read here at worst wastes a tier_bump_count call.
    uint8_t miflags = jl_atomic_load_relaxed(&mi->flags);
    if (miflags & JL_MI_FLAGS_TIER_QUEUED)
        return;
    uint32_t threshold = jl_tier_get_threshold();
    if (threshold > 1) {
        if (!jl_atomic_load_acquire(&tier_initialized))
            return; // count table not ready; will retry on next call
        if (tier_bump_count(mi) < threshold)
            return;
    }
    tier_enqueue_mi_locked(mi);
}

#ifdef __cplusplus
}
#endif
