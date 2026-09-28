// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <assert.h>
#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "julia.h"
#include "julia_internal.h"
#include "threading.h"

#ifdef __cplusplus
extern "C" {
#endif


// thread sleep state

// default to DEFAULT_THREAD_SLEEP_THRESHOLD; set via $JULIA_THREAD_SLEEP_THRESHOLD
uint64_t sleep_threshold;

// thread should not be sleeping--it might need to do work.
static const int16_t not_sleeping = 0;

// it is acceptable for the thread to be sleeping.
static const int16_t sleeping = 1;

// this thread is dead.
static const int16_t sleeping_like_the_dead JL_UNUSED = 2;

// a running count of how many threads are currently not_sleeping
// plus a running count of the number of in-flight wake-ups
// n.b. this may temporarily exceed jl_n_threads
_Atomic(int) n_threads_running = 0;

// A thread that finds no work keeps looking (searching) for up to
// sleep_threshold ns before it sleeps. At most half the pool searches, or all
// of it for threads whose last search found work. A new task wakes a thread
// only if nobody searches. See devdocs/scheduler-wakeup.
// So that no task is missed:
//  - a searcher stops searching before it goes to sleep, then checks the
//    queues once more ([^store_buffering_1]);
//  - the last searcher to stop wakes another thread if work may be queued.
// One per thread pool.
typedef struct {
    // threads searching, including ones being woken
    alignas(JL_CACHE_BYTE_ALIGNMENT) _Atomic(int32_t) n_spinning;
    // last thread to sleep (tid + 1, or 0), woken first: its core is warmest
    alignas(JL_CACHE_BYTE_ALIGNMENT) _Atomic(int16_t) last_parked;
    // threads marked sleeping, counted before the mark: 0 read after a fence
    // means no wake is needed ([^store_buffering_1])
    alignas(JL_CACHE_BYTE_ALIGNMENT) _Atomic(int32_t) n_sleeping;
    // thread ids [lo, lo + n)
    alignas(JL_CACHE_BYTE_ALIGNMENT) int16_t lo;
    int16_t n;
} wake_gate_t;
#define JL_N_WAKE_GATES 2
static wake_gate_t wake_gates[JL_N_WAKE_GATES];

static wake_gate_t *gate_of_pool(int8_t tpid) JL_NOTSAFEPOINT
{
    if (tpid < 0 || tpid >= JL_N_WAKE_GATES)
        return NULL;
    return &wake_gates[tpid];
}

// The gate of `tid`'s pool, or NULL for threads outside the pools.
static wake_gate_t *gate_of_tid(int16_t tid) JL_NOTSAFEPOINT
{
    for (int i = 0; i < JL_N_WAKE_GATES; i++) {
        if (tid >= wake_gates[i].lo && tid < wake_gates[i].lo + wake_gates[i].n)
            return &wake_gates[i];
    }
    return NULL;
}

// `tid` stopped sleeping.
static void leave_sleeping(int16_t tid) JL_NOTSAFEPOINT
{
    wake_gate_t *gate = gate_of_tid(tid);
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_sleeping, -1);
}

// Returns 1 if this was the last searcher.
static int release_searcher_slot(wake_gate_t *gate) JL_NOTSAFEPOINT
{
    int32_t prev = jl_atomic_fetch_add_relaxed(&gate->n_spinning, -1);
    assert(prev > 0);
    return prev == 1;
}

// A wake counts its target as a searcher up front; unaccount_searcher undoes
// that if the wake fails.
static wake_gate_t *preaccount_searcher(int16_t tid) JL_NOTSAFEPOINT
{
    wake_gate_t *gate = gate_of_tid(tid);
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_spinning, 1);
    return gate;
}

static void unaccount_searcher(wake_gate_t *gate, unsigned *pending_pools) JL_NOTSAFEPOINT
{
    // If this was the last searcher, a wake is owed (done after the scan).
    if (gate != NULL && release_searcher_slot(gate))
        *pending_pools |= 1u << (gate - wake_gates);
}

// invariant: No thread is ever asleep unless sleep_check_state is sleeping (or we have a wakeup signal pending).
// invariant: Any particular thread is not asleep unless that thread's sleep_check_state is sleeping.
// invariant: The transition of a thread state to sleeping must be followed by a check that there wasn't work pending for it.
// information: Observing thread not-sleeping is sufficient to ensure the target thread will subsequently inspect its local queue.
// information: Observing thread is-sleeping says it may be necessary to notify it at least once to wakeup. It may already be awake however for a variety of reasons.
// information: These observations require sequentially-consistent fences to be inserted between each of those operational phases.
// [^store_buffering_1]: These fences are used to avoid the cycle 2b -> 1a -> 1b -> 2a -> 2b where
// * Dequeuer:
//   * 1: `jl_atomic_store_relaxed(&ptls->sleep_check_state, sleeping)`
// * Enqueuer:
//   * 2: `jl_atomic_load_relaxed(&ptls->sleep_check_state)` in `jl_wakeup_thread` returns `not_sleeping`
// i.e., the dequeuer misses the enqueue and enqueuer misses the sleep state transition.
// [^store_buffering_2]: and also
// * Enqueuer:
//   * 1a: `jl_atomic_store_relaxed(jl_uv_n_waiters, 1)` in `JL_UV_LOCK`
//   * 1b: "cheap read" of `handle->pending` in `uv_async_send` (via `JL_UV_LOCK`) loads `0`
// * Dequeuer:
//   * 2a: store `2` to `handle->pending` in `uv_async_send` (via `JL_UV_LOCK` in `jl_task_get_next`)
//   * 2b: `jl_atomic_load_relaxed(jl_uv_n_waiters)` in `jl_task_get_next` returns `0`
// i.e., the dequeuer misses the `n_waiters` is set and enqueuer misses the `uv_stop` flag (in `signal_async`) transition to cleared

JULIA_DEBUG_SLEEPWAKE(
uint64_t wakeup_enter;
uint64_t wakeup_leave;
uint64_t io_wakeup_enter;
uint64_t io_wakeup_leave;
);

JL_DLLEXPORT int jl_set_task_tid(jl_task_t *task, int16_t tid) JL_NOTSAFEPOINT
{
    // Try to acquire the lock on this task.
    int16_t was = jl_atomic_load_relaxed(&task->tid);
    if (was == tid)
        return 1;
    if (was == -1)
        return jl_atomic_cmpswap(&task->tid, &was, tid) || was == tid;
    return 0;
}

JL_DLLEXPORT int jl_set_task_threadpoolid(jl_task_t *task, int8_t tpid) JL_NOTSAFEPOINT
{
    if (tpid < -1 || tpid >= jl_n_threadpools)
        return 0;
    task->threadpoolid = tpid;
    return 1;
}

// initialize the threading infrastructure
// (called only by the main thread)
void jl_init_threadinginfra(void)
{
    int16_t lo = 0;
    for (int i = 0; i < JL_N_WAKE_GATES && i < jl_n_threadpools; i++) {
        wake_gates[i].lo = lo;
        wake_gates[i].n = (int16_t)jl_n_threads_per_pool[i];
        lo += wake_gates[i].n;
    }
    /* initialize the synchronization trees pool */
    sleep_threshold = DEFAULT_THREAD_SLEEP_THRESHOLD;
    char *cp = getenv(THREAD_SLEEP_THRESHOLD_NAME);
    if (cp) {
        if (!strncasecmp(cp, "infinite", 8))
            sleep_threshold = UINT64_MAX;
        else
            sleep_threshold = (uint64_t)strtol(cp, NULL, 10);
    }
}

// thread function: used by all mutator threads except the main thread
void jl_threadfun(void *arg)
{
    jl_threadarg_t *targ = (jl_threadarg_t*)arg;

    // initialize this thread (set tid, create heap, set up root task)
    jl_ptls_t ptls = jl_init_threadtls(targ->tid);
    void *stack_lo, *stack_hi;
    jl_init_stack_limits(0, &stack_lo, &stack_hi);
    // warning: this changes `jl_current_task`, so be careful not to call that from this function
    jl_task_t *ct = jl_init_root_task(ptls, stack_lo, stack_hi);
    JL_GC_PROMISE_ROOTED(ct);

    // wait for all threads
#ifdef __clang_safetyanalysis__
    jl_gc_safe_enter(ptls);
#else
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, JL_GC_STATE_UNSAFE);
#endif
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    (void)jl_gc_unsafe_enter(ptls);
    jl_finish_task(ct); // noreturn
}



void jl_init_thread_scheduler(jl_ptls_t ptls)
{
    uv_mutex_init(&ptls->sleep_lock);
    uv_cond_init(&ptls->wake_signal);
    // record that there is now another thread that may be used to schedule work
    // we will decrement this again in scheduler_delete_thread, only slightly
    // in advance of pthread_join (which hopefully itself also had been
    // adopted by now and is included in n_threads_running too)
    (void)jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
    // n.b. this is the only point in the code where we ignore the invariants on the ordering of n_threads_running
    // since we are being initialized from foreign code, we could not necessarily have expected or predicted that to happen
}

JL_DLLEXPORT int jl_running_under_rr(int recheck)
{
#ifdef _OS_LINUX_
#define RR_CALL_BASE 1000
#define SYS_rrcall_check_presence (RR_CALL_BASE + 8)
    static _Atomic(int) is_running_under_rr = 0;
    int rr = jl_atomic_load_relaxed(&is_running_under_rr);
    if (rr == 0 || recheck) {
        int ret = syscall(SYS_rrcall_check_presence, 0, 0, 0, 0, 0, 0);
        if (ret == -1)
            // Should always be ENOSYS, but who knows what people do for
            // unknown syscalls with their seccomp filters, so just say
            // that we don't have rr.
            rr = 2;
        else
            rr = 1;
        jl_atomic_store_relaxed(&is_running_under_rr, rr);
    }
    return rr == 1;
#else
    return 0;
#endif
}


//  sleep_check_after_threshold() -- if sleep_threshold ns have passed, return 1
static int sleep_check_after_threshold(uint64_t *start_cycles) JL_NOTSAFEPOINT
{
    JULIA_DEBUG_SLEEPWAKE( return 1 ); // hammer on the sleep/wake logic much harder
    /**
     * This wait loop is a bit of a worst case for rr - it needs timer access,
     * which are slow and it busy loops in user space, which prevents the
     * scheduling logic from switching to other threads. Just don't bother
     * trying to wait here
     */
    if (jl_running_under_rr(0))
        return 1;
    if (!(*start_cycles)) {
        *start_cycles = jl_hrtime();
        return 0;
    }
    uint64_t elapsed_cycles = jl_hrtime() - (*start_cycles);
    if (elapsed_cycles >= sleep_threshold) {
        *start_cycles = 0;
        return 1;
    }
    return 0;
}

void surprise_wakeup(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    // This task never returns to the scheduler, so the wake must not count
    // a searcher.
    int8_t state = jl_atomic_load_relaxed(&ptls->sleep_check_state);
    if (state == sleeping) {
        if (jl_atomic_cmpswap_relaxed(&ptls->sleep_check_state, &state, not_sleeping)) {
            leave_sleeping(ptls->tid);
            // this notification will never be consumed, so we may have now
            // introduced some inaccuracy into the count, but that is
            // unavoidable with any asynchronous interruption
            jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
        }
    }
}


static int set_not_sleeping(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    // acquire: a waker's flip comes with the searcher count it added
    if (jl_atomic_load_acquire(&ptls->sleep_check_state) != not_sleeping) {
        if (jl_atomic_exchange(&ptls->sleep_check_state, not_sleeping) != not_sleeping) {
            leave_sleeping(ptls->tid);
            return 1;
        }
    }
    int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, -1); // consume in-flight wakeup
    assert(wasrunning > 1); (void)wasrunning;
    return 0;
}

// Returns 1 if we woke ourselves; otherwise we keep our waker's searcher
// count.
static int settle_wake(jl_ptls_t ptls, wake_gate_t *gate, volatile int *spinning) JL_NOTSAFEPOINT
{
    if (set_not_sleeping(ptls))
        return 1;
    if (gate != NULL)
        *spinning = 1;
    return 0;
}

// Signal a thread that may be waiting to be woken.
static void signal_thread_wake(jl_ptls_t ptls2) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&ptls2->sleep_lock);
    uv_cond_signal(&ptls2->wake_signal);
    uv_mutex_unlock(&ptls2->sleep_lock);
}

// Mark a sleeping thread as woken, counting it as a searcher. The caller
// signals it.
static int try_wake_thread(jl_ptls_t ptls, int16_t tid, unsigned *pending_pools) JL_NOTSAFEPOINT
{
    if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != sleeping)
        return 0;
    // count first, so the target never drops a count we did not add
    wake_gate_t *gate = preaccount_searcher(tid);
    int8_t state = sleeping;
    if (jl_atomic_cmpswap_release(&ptls->sleep_check_state, &state, not_sleeping)) {
        if (gate != NULL)
            jl_atomic_fetch_add_relaxed(&gate->n_sleeping, -1);
        int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
        assert(wasrunning); (void)wasrunning;
        return 1;
    }
    unaccount_searcher(gate, pending_pools);
    return 0;
}

static int wake_thread(int16_t tid, unsigned *pending_pools) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (!try_wake_thread(ptls, tid, pending_pools))
        return 0;
    JL_PROBE_RT_SLEEP_CHECK_WAKE(ptls, sleeping);
    signal_thread_wake(ptls);
    return 1;
}


static void wake_libuv(void) JL_NOTSAFEPOINT
{
    JULIA_DEBUG_SLEEPWAKE( io_wakeup_enter = cycleclock() );
    jl_wake_libuv();
    JULIA_DEBUG_SLEEPWAKE( io_wakeup_leave = cycleclock() );
}

// Leave any half-finished sleep, and stop uv_run if this thread runs it.
static void wake_self(jl_task_t *ct, jl_task_t *uvlock, unsigned *pending_pools) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = ct->ptls;
    // our scheduler loop takes the count (settle_wake)
    if (try_wake_thread(ptls, jl_atomic_load_relaxed(&ct->tid), pending_pools))
        JL_PROBE_RT_SLEEP_CHECK_WAKEUP(ptls);
    if (uvlock == ct)
        uv_stop(jl_global_event_loop());
}

// Wake `tid` if it sleeps, and kick libuv if `tid` is in uv_run.
static int wake_thread_and_uv(jl_task_t *ct, jl_task_t *uvlock, int16_t tid,
                              unsigned *pending_pools) JL_NOTSAFEPOINT
{
    if (!wake_thread(tid, pending_pools))
        return 0;
    if (ct == NULL || uvlock != ct) { // a foreign caller never owns the libuv lock
        jl_fence();
        jl_ptls_t other = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        jl_task_t *tid_task = jl_atomic_load_relaxed(&other->current_task);
        if (jl_atomic_load_relaxed(&jl_uv_mutex.owner) == tid_task)
            wake_libuv();
    }
    return 1;
}

// Round-robin start hint for jl_wakeup_threadpool, sharded across cache-line-padded
// stripes so concurrent producers don't contend on a single counter.
#define POOL_WAKE_HINT_STRIPES 64
typedef struct {
    _Atomic(uint32_t) v;
    char pad[64 - sizeof(_Atomic(uint32_t))];
} pool_wake_hint_t;
static pool_wake_hint_t pool_wake_hints[POOL_WAKE_HINT_STRIPES];

// Wake one sleeping thread, trying the last one to sleep first.
static void wake_one_in_pool(jl_task_t *ct, jl_task_t *uvlock, wake_gate_t *gate,
                             int16_t self, unsigned *pending_pools) JL_NOTSAFEPOINT
{
    int16_t lo = gate->lo;
    int16_t n = gate->n;

    int16_t hinted = (int16_t)(jl_atomic_load_relaxed(&gate->last_parked) - 1);
    if (hinted >= lo && hinted < lo + n && hinted != self &&
        wake_thread_and_uv(ct, uvlock, hinted, pending_pools))
        return;
    if (n > 0) {
        uint32_t stripe = ((uint32_t)self) & (POOL_WAKE_HINT_STRIPES - 1);
        uint32_t start = jl_atomic_fetch_add_relaxed(&pool_wake_hints[stripe].v, 1);
        for (int16_t k = 0; k < n; k++) {
            int16_t tid = lo + (int16_t)((start + (uint32_t)k) % (uint32_t)n);
            if (tid != self && wake_thread_and_uv(ct, uvlock, tid, pending_pools))
                return;
        }
    }
}

// Wake a thread in each pool of `pending_pools` that nobody searches, and
// repeat for wakes owed by failed ones. `ct` is NULL on foreign threads.
static void drain_pool_wakeups(jl_task_t *ct, unsigned pending_pools) JL_NOTSAFEPOINT
{
    if (!pending_pools)
        return;
    int16_t self = ct == NULL ? -1 : jl_atomic_load_relaxed(&ct->tid);
    while (pending_pools) {
        // wakes owed by this round wait for the next fence
        unsigned requests = pending_pools;
        pending_pools = 0;
        jl_fence(); // [^store_buffering_1], including the preceding rollbacks
        jl_task_t *uvlock = jl_atomic_load_relaxed(&jl_uv_mutex.owner);
        if (ct != NULL)
            wake_self(ct, uvlock, &pending_pools);
        for (int i = 0; i < JL_N_WAKE_GATES; i++) {
            if (!(requests & (1u << i)))
                continue;
            wake_gate_t *gate = &wake_gates[i];
            // nobody to wake
            if (jl_atomic_load_relaxed(&gate->n_sleeping) == 0)
                continue;
            if (jl_atomic_load_relaxed(&gate->n_spinning) == 0)
                wake_one_in_pool(ct, uvlock, gate, self, &pending_pools);
        }
    }
}

// Returns 1 if `tid` (or, for -1, any thread) was woken.
static int wakeup_thread(jl_task_t *ct, int16_t tid) JL_NOTSAFEPOINT
{
    int woke = 0;
    unsigned pending_pools = 0;
    int16_t self = jl_atomic_load_relaxed(&ct->tid);
    if (tid != self)
        jl_fence(); // [^store_buffering_1]
    jl_task_t *uvlock = jl_atomic_load_relaxed(&jl_uv_mutex.owner);
    JULIA_DEBUG_SLEEPWAKE( wakeup_enter = cycleclock() );
    if (tid == self || tid == -1) {
        wake_self(ct, uvlock, &pending_pools);
    }
    else {
        // something added to the sticky-queue: notify that thread
        woke = wake_thread_and_uv(ct, uvlock, tid, &pending_pools);
    }
    if (tid == -1) {
        // Legacy broadcast wake; prefer jl_wakeup_threadpool.
        int anysleep = 0;
        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        for (tid = 0; tid < nthreads; tid++) {
            if (tid != self)
                anysleep |= wake_thread(tid, &pending_pools);
        }
        woke = anysleep;
        // check if we need to notify uv_run too
        if (uvlock != ct && anysleep) {
            jl_fence();
            if (jl_atomic_load_relaxed(&jl_uv_mutex.owner) != NULL)
                wake_libuv();
        }
    }
    drain_pool_wakeups(ct, pending_pools);
    JULIA_DEBUG_SLEEPWAKE( wakeup_leave = cycleclock() );
    return woke;
}

// Make sure tid is awake; returns 1 if this call woke it.
JL_DLLEXPORT int jl_wakeup_thread(int16_t tid)
{
    jl_task_t *ct = jl_current_task;
    return wakeup_thread(ct, tid);
}

// Like jl_wakeup_thread, but callable from non-Julia threads (e.g. the signal
// listener), which have no task context.
JL_DLLEXPORT void jl_wakeup_thread_from_foreign(int16_t tid) JL_NOTSAFEPOINT
{
    if (tid < 0)
        return;
    unsigned pending_pools = 0;
    jl_fence(); // [^store_buffering_1]
    wake_thread_and_uv(NULL, NULL, tid, &pending_pools);
    drain_pool_wakeups(NULL, pending_pools);
}

// Wake a sleeping thread of the pool if nobody searches it.
static void gated_wakeup(wake_gate_t *gate) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    JULIA_DEBUG_SLEEPWAKE( wakeup_enter = cycleclock() );
    drain_pool_wakeups(ct, 1u << (gate - wake_gates));
    JULIA_DEBUG_SLEEPWAKE( wakeup_leave = cycleclock() );
}

JL_DLLEXPORT void jl_wakeup_threadpool(int8_t tpid)
{
    wake_gate_t *gate = gate_of_pool(tpid);
    if (gate == NULL) {
        wakeup_thread(jl_current_task, -1);
        return;
    }
    gated_wakeup(gate);
}

// Stop searching. The last searcher, or one owing a wake, wakes another
// thread: a new task may have skipped its wake because we were searching.
static void searcher_exit(wake_gate_t *gate, volatile int *spinning,
                          volatile int *owed) JL_NOTSAFEPOINT
{
    int wake = *owed;
    *owed = 0;
    if (*spinning) {
        *spinning = 0;
        if (release_searcher_slot(gate))
            wake = 1;
    }
    if (wake)
        gated_wakeup(gate);
}

// get the next runnable task
static jl_task_t *get_next_task(jl_value_t *trypoptask, jl_value_t *q) JL_CANSAFEPOINT
{
    jl_gc_safepoint();
    jl_task_t *task = (jl_task_t*)jl_apply_generic(trypoptask, &q, 1);
    if (jl_is_task(task)) {
        int self = jl_atomic_load_relaxed(&jl_current_task->tid);
        jl_set_task_tid(task, self);
        return task;
    }
    return NULL;
}

static int check_empty(jl_value_t *checkempty) JL_CANSAFEPOINT
{
    return jl_apply_generic(checkempty, NULL, 0) == jl_true;
}

jl_task_t *wait_empty JL_GLOBALLY_ROOTED;

void jl_task_wait_empty(void)
{
    jl_task_t *ct = jl_current_task;
    if (jl_atomic_load_relaxed(&ct->tid) == 0 && jl_base_module) {
        jl_wait_empty_begin();
        size_t lastage = ct->world_age;
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        jl_value_t *f = jl_get_global_value(jl_base_module, jl_symbol("wait"), ct->world_age);
        wait_empty = ct;
        if (f) {
            JL_GC_PUSH1(&f);
            jl_apply_generic(f, NULL, 0);
            JL_GC_POP();
        }
        // we are back from jl_task_get_next now
        ct->world_age = lastage;
        wait_empty = NULL;
        // TODO: move this lock acquire to before the wait_empty return and the
        // unlock to the caller, so that we ensure new work (from uv_unref
        // objects) didn't unexpectedly get scheduled and start running behind
        // our back during the function return
        JL_UV_LOCK();
        jl_wait_empty_end();
        JL_UV_UNLOCK();
    }
}

static int may_sleep(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    // sleep_check_state is only transitioned from not_sleeping to sleeping
    // by the thread itself. As a result, if this returns false, it will
    // continue returning false. If it returns true, we know the total
    // modification order of the fences.
    jl_fence(); // [^store_buffering_1] [^store_buffering_2]
    // acquire: see set_not_sleeping
    return jl_atomic_load_acquire(&ptls->sleep_check_state) == sleeping;
}


// Run a pending ^C dispatch pass inline on this (idle, ordinary-task-
// context) thread: the pass only wakes waiters and walks Julia state, so
// any scheduling thread can perform it, and the event loop stays out of
// the delivery path - a worker must not run the shared loop outside
// threaded regions, and the loop-owning thread may be stuck in a foreign
// call, which is exactly when ^C must still deliver. Base arbitrates
// concurrent passes (see `Base.maybe_dispatch_sigint`); errors are caught
// on the Julia side.
static void jl_dispatch_sigint_inline(void) JL_CANSAFEPOINT
{
    static _Atomic(jl_value_t *) dispatch_f = NULL;
    jl_value_t *f = jl_atomic_load_relaxed(&dispatch_f);
    if (f == NULL) {
        if (jl_base_module == NULL)
            return;
        f = jl_get_global(jl_base_module, jl_symbol("maybe_dispatch_sigint"));
        if (f == NULL)
            return;
        jl_atomic_store_relaxed(&dispatch_f, f);
    }
    jl_apply(&f, 1);
}

// Go to sleep:
//   RELEASE: stop searching
//   PUBLISH: set the sleep state
//   RECHECK: check the queues again [^store_buffering_1]
//   RETIRE:  leave the running count
//   PARK:    wait for a wake
// Returns a task if the recheck found one, NULL after a wake.
static jl_task_t *sleep_thread(jl_task_t *ct, wake_gate_t *volatile *pgate,
                               volatile int *spinning, uint64_t *start_cycles,
                               jl_value_t *trypoptask, jl_value_t *q,
                               jl_value_t *checkempty) JL_CANSAFEPOINT
{
    jl_ptls_t ptls = ct->ptls;
    wake_gate_t *gate = *pgate;
    jl_task_t *task = NULL;
    // RELEASE before PUBLISH and its fence (pairs with the enqueuer's check)
    if (*spinning) {
        *spinning = 0;
        release_searcher_slot(gate);
    }
    // acquire sleep-check lock
    assert(jl_atomic_load_relaxed(&ptls->sleep_check_state) == not_sleeping);
    // PUBLISH, counting ourselves as sleeping first (see n_sleeping)
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_sleeping, 1);
    jl_atomic_store_relaxed(&ptls->sleep_check_state, sleeping);
    jl_fence(); // [^store_buffering_1]
    JL_PROBE_RT_SLEEP_CHECK_SLEEP(ptls);
    volatile int isrunning = 1;
    JL_TRY {
        // `continue` leaves the JL_TRY for the return below.
        // RECHECK, inside the handler: checkempty may throw.
        if (!check_empty(checkempty)) { // uses relaxed loads
            if (settle_wake(ptls, gate, spinning)) {
                JL_PROBE_RT_SLEEP_CHECK_TASKQ_WAKE(ptls);
            }
            continue;
        }
        task = get_next_task(trypoptask, q); // note: this should not yield
        if (ptls != ct->ptls) {
            // sigh, a yield was detected, so let's go ahead and handle it anyway by starting over
            // (undoing our sleep on the thread we left)
            if (settle_wake(ptls, gate, spinning)) {
                JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls);
            }
            ptls = ct->ptls;
            gate = gate_of_tid(jl_atomic_load_relaxed(&ct->tid));
            *pgate = gate;
            continue;
        }
        if (task) {
            if (settle_wake(ptls, gate, spinning)) {
                JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls);
            }
            continue;
        }

        // IO is always permitted, but outside a threaded region, only
        // thread 0 will process messages.
        // Inside a threaded region, any thread can listen for IO messages,
        // and one thread should win this race and watch the event loop,
        // but we bias away from idle threads getting parked here.
        //
        // The reason this works is somewhat convoluted, and closely tied to [^store_buffering_1]:
        //  - After decrementing _threadedregion, the thread is required to
        //    call jl_wakeup_thread(0), that will kick out any thread who is
        //    already there, and then eventually thread 0 will get here.
        //  - Inside a _threadedregion, there must exist at least one
        //    thread that has a happens-before relationship on the libuv lock
        //    before reaching this decision point in the code who will see
        //    the lock as unlocked and thus must win this race here.
        int uvlock = 0;
        if (jl_atomic_load_relaxed(&_threadedregion)) {
            uvlock = jl_mutex_trylock(&jl_uv_mutex);
        }
        else if (ptls->tid == jl_atomic_load_relaxed(&io_loop_tid)) {
            uvlock = 1;
            JL_UV_LOCK();
        }
        else {
            // Since we might have started some IO work, we might need
            // to ensure tid = 0 will go watch that new event source.
            // If trylock would have succeeded, that may have been our
            // responsibility, so need to make sure thread 0 will take care
            // of us.
            if (jl_atomic_load_relaxed(&jl_uv_mutex.owner) == NULL) // aka trylock
                jl_wakeup_thread(jl_atomic_load_relaxed(&io_loop_tid));
        }
        if (uvlock) {
            int enter_eventloop = may_sleep(ptls);
            int active = 0;
            if (jl_atomic_load_relaxed(&jl_uv_n_waiters) != 0)
                // if we won the race against someone who actually needs
                // the lock to do real work, we need to let them have it instead
                enter_eventloop = 0;
            if (enter_eventloop) {
                uv_loop_t *loop = jl_global_event_loop();
                loop->stop_flag = 0;
                JULIA_DEBUG_SLEEPWAKE( ptls->uv_run_enter = cycleclock() );
                active = uv_run(loop, UV_RUN_ONCE);
                JULIA_DEBUG_SLEEPWAKE( ptls->uv_run_leave = cycleclock() );
                jl_gc_safepoint();
            }
            JL_UV_UNLOCK();
            // optimization: check again first if we may have work to do.
            // Otherwise we got a spurious wakeup since some other thread
            // that just wanted to steal libuv from us. We will just go
            // right back to sleep on the individual wake signal to let
            // them take it from us without conflict.
            if (active || !may_sleep(ptls)) {
                if (settle_wake(ptls, gate, spinning)) {
                    JL_PROBE_RT_SLEEP_CHECK_UV_WAKE(ptls);
                }
                *start_cycles = 0;
                continue;
            }
            if (!enter_eventloop && !jl_atomic_load_relaxed(&_threadedregion) && ptls->tid == jl_atomic_load_relaxed(&io_loop_tid)) {
                // thread 0 is the only thread permitted to run the event loop
                // so it needs to stay alive, just spin-looping if necessary
                if (settle_wake(ptls, gate, spinning)) {
                    JL_PROBE_RT_SLEEP_CHECK_UV_WAKE(ptls);
                }
                *start_cycles = 0;
                continue;
            }
        }

        // RETIRE; a waker that sees us sleeping counts us back in
        int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, -1);
        assert(wasrunning);
        isrunning = 0;
        if (wasrunning == 1) {
            // This was the last running thread, and there is no thread with !may_sleep
            // so make sure io_loop_tid is notified to check wait_empty
            // TODO: this also might be a good time to check again that
            // libuv's queue is truly empty, instead of during delete_thread
            int16_t tid2 = 0;
            if (ptls->tid != tid2)
                signal_thread_wake(jl_atomic_load_relaxed(&jl_all_tls_states)[tid2]);
        }

        // the other threads will just wait for an individual wake signal to resume
        if (gate != NULL)
            jl_atomic_store_relaxed(&gate->last_parked, (int16_t)(ptls->tid + 1));
        JULIA_DEBUG_SLEEPWAKE( ptls->sleep_enter = cycleclock() );
        int8_t gc_state = jl_gc_safe_enter(ptls);
        jl_safepoint_take_sleep_lock(ptls); // This puts the thread in GC_SAFE and takes the sleep lock
        while (may_sleep(ptls)) {
            if (ptls->tid == 0) {
                task = wait_empty;
                if (task && jl_atomic_load_relaxed(&n_threads_running) == 0) {
                    wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
                    assert(!wasrunning);
                    wasrunning = !set_not_sleeping(ptls);
                    assert(!wasrunning);
                    JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls);
                    if (!ptls->finalizers_inhibited)
                        ptls->finalizers_inhibited++; // this annoyingly is rather sticky (we should like to reset it at the end of jl_task_wait_empty)
                    break;
                }
                task = NULL;
            }
            // else should we warn the user of certain deadlock here if tid == 0 && n_threads_running == 0?
            // PARK
            uv_cond_wait(&ptls->wake_signal, &ptls->sleep_lock);
        }
        assert(jl_atomic_load_relaxed(&ptls->sleep_check_state) == not_sleeping);
        assert(jl_atomic_load_relaxed(&n_threads_running));
        *start_cycles = 0;
        uv_mutex_unlock(&ptls->sleep_lock);
        JULIA_DEBUG_SLEEPWAKE( ptls->sleep_leave = cycleclock() );
        jl_gc_notify_task_resume(ct);
        jl_gc_safe_leave(ptls, gc_state); // contains jl_gc_safepoint
        if (task) {
            assert(task == wait_empty);
            wait_empty = NULL;
            continue;
        }
        // woken: keep our waker's searcher count (only wait_empty above wakes
        // itself)
        if (gate != NULL)
            *spinning = 1;
    }
    JL_CATCH {
        // an error in trypoptask, checkempty, or a libuv callback
        if (!isrunning)
            jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
        gate = *pgate;
        // A wake may race the error; settle_wake keeps its searcher count in
        // *spinning for the caller's handler to drop.
        settle_wake(ptls, gate, spinning);
        // the recheck did not finish, so wake a thread in its place
        if (gate != NULL)
            gated_wakeup(gate);
        jl_rethrow();
    }
    return task;
}

JL_DLLEXPORT jl_task_t *jl_task_get_next(jl_value_t *trypoptask, jl_value_t *q, jl_value_t *checkempty) JL_CANSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    uint64_t start_cycles = 0;
    // NULL outside the pools. volatile: sleep_thread may change it, and the
    // JL_CATCH reads it.
    wake_gate_t *volatile gate = gate_of_tid(jl_atomic_load_relaxed(&ct->tid));
    // whether we are searching; volatile for the JL_CATCH
    volatile int spinning = 0;
    // whether we started searching on our own, and when we last began to
    // sleep; they set ptls->search_useful
    int own_slot = 0;
    uint64_t park_start = 0;
    // set when the recheck found work after we stopped searching: our next
    // pop must wake another thread if work remains; volatile for the JL_CATCH
    volatile int handoff_owed = 0;
    jl_task_t *task = NULL;

    // trypoptask, checkempty and libuv callbacks may throw; the handler stops
    // our search
    JL_TRY {
        while (1) {
            if (jl_atomic_load_relaxed(&jl_sigint_dispatch_pending)) {
                // The dispatch may block on a lock, so stop searching first.
                searcher_exit(gate, &spinning, &handoff_owed);
                own_slot = 0;
                jl_dispatch_sigint_inline();
            }
            // task abandonment must not unwind this loop (see task.c)
            ct->ptls->in_get_next = 1;
            task = get_next_task(trypoptask, q);
            if (task == NULL) {
                // nothing found, so no wake is owed
                handoff_owed = 0;
                jl_ptls_t ptls = ct->ptls;
                int is_io_thread = ptls->tid == jl_atomic_load_relaxed(&io_loop_tid);
                if (!spinning && gate != NULL && !is_io_thread) {
                    // at most half the pool, or all of it if our last search
                    // found work (we would likely be woken again soon) or if
                    // threads never sleep
                    int32_t cap = ptls->search_useful || sleep_threshold == UINT64_MAX ?
                        gate->n : (gate->n + 1) / 2;
                    if (jl_atomic_load_relaxed(&gate->n_spinning) < cap) {
                        jl_atomic_fetch_add_relaxed(&gate->n_spinning, 1);
                        spinning = 1;
                        own_slot = 1;
                    }
                }
                // refused: sleep without polling (sleep_thread still
                // rechecks)
                int force_park = gate != NULL && !spinning && !is_io_thread;

                // quick, race-y check to see if there seems to be any stuff in there
                jl_cpu_pause();
                if (!force_park && !check_empty(checkempty)) {
                    start_cycles = 0;
                    continue;
                }

                jl_cpu_pause();
                int timed_out = !force_park && sleep_check_after_threshold(&start_cycles);
                if (force_park || timed_out ||
                    (is_io_thread && (!jl_atomic_load_relaxed(&_threadedregion) ||
                        wait_empty))) {
                    if (timed_out)
                        ptls->search_useful = 0;
                    int held = spinning;
                    own_slot = 0;
                    park_start = jl_hrtime();
                    task = sleep_thread(ct, &gate, &spinning, &start_cycles,
                                        trypoptask, q, checkempty);
                    // unpooled threads should never spin
                    assert(!spinning || gate != NULL);
                    handoff_owed = held && !spinning && gate != NULL;
                }
                else {
                    // maybe check the kernel for new messages too
                    jl_process_events();
                }
            }
            if (task) {
                // we found work, or it came sooner than a search would last
                if (own_slot || (park_start && jl_hrtime() - park_start < sleep_threshold))
                    ct->ptls->search_useful = 1;
                // the last searcher (or one owing a wake) wakes another
                // thread if work remains and nobody searches
                int handoff = handoff_owed;
                handoff_owed = 0;
                if (spinning) {
                    spinning = 0;
                    if (release_searcher_slot(gate))
                        handoff = 1;
                }
                if (handoff) {
                    // pairs with the enqueuer's fence
                    jl_fence(); // [^store_buffering_1]
                    if (jl_atomic_load_relaxed(&gate->n_sleeping) > 0) {
                        // we hold a popped task, so an error here must not
                        // unwind: treat it as work remaining
                        volatile int more = 1;
                        JL_GC_PUSH1(&task);
                        JL_TRY {
                            more = !check_empty(checkempty);
                        }
                        JL_CATCH {
                            jl_safe_printf("WARNING: ignoring an error thrown by checkempty: %s\n",
                                           jl_typeof_str(jl_current_exception(ct)));
                        }
                        JL_GC_POP();
                        if (more)
                            gated_wakeup(gate);
                    }
                }
                break;
            }
        }
    }
    JL_CATCH {
        // no recheck when unwinding, so wake as if we had found work
        searcher_exit(gate, &spinning, &handoff_owed);
        ct->ptls->in_get_next = 0;
        jl_rethrow();
    }
    ct->ptls->in_get_next = 0;
    return task;
}

void scheduler_delete_thread(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    int8_t oldstate = jl_atomic_exchange_relaxed(&ptls->sleep_check_state, sleeping_like_the_dead);
    if (oldstate == sleeping)
        leave_sleeping(ptls->tid);
    int notsleeping = oldstate == not_sleeping;
    jl_fence();
    if (notsleeping) {
        if (jl_atomic_load_relaxed(&n_threads_running) == 1) {
            // This was the last running thread, and there is no thread with !may_sleep
            // so make sure tid 0 is notified to check wait_empty
            signal_thread_wake(jl_atomic_load_relaxed(&jl_all_tls_states)[jl_atomic_load_relaxed(&io_loop_tid)]);
        }
    }
    else {
        jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
    }
    wakeup_thread(jl_atomic_load_relaxed(&ptls->current_task), 0); // force thread 0 to see that we do not have the IO lock (and am dead)
    jl_atomic_fetch_add_relaxed(&n_threads_running, -1);
}

#ifdef __cplusplus
}
#endif
