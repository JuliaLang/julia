// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <assert.h>
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

// Searcher accounting (cf. Go's nmspinning; see devdocs/scheduler-wakeup).
// A thread that fails to pop work may become a "searcher", polling the
// queues for up to sleep_threshold ns before parking. At most half of a
// pool may search at once; above that they are parked.
// jl_wakeup_threadpool compares the number of pending tasks (n_ready)
// against the number of spinners, so we also don't have more spinners than
// work. This avoids unneeded contention on the queues.
// n_spinning is incremented by the waker but decremented by the thread that
// got woken; a waker that loses the wakeup CAS decrements the count itself.
// Lost-wakeup safety:
//  - a searcher leaving to park decrements n_spinning *before* its
//    sleeping-store + fence + queue recheck, pairing with the enqueuer's
//    store + fence + load ([^store_buffering_1]);
//  - the last searcher to exit any other way (with work, or unwinding)
//    is responsible for waking another thread if work remains (the exit
//    handoff).
// Like Go, the count may briefly overshoot the cap (load-then-add).
// One wake gate per thread pool. Threads outside both pools
// (foreign/adopted) have no gate: they cannot pop the pool queues, so they
// are not search supply and their wakes pre-account nothing.
typedef struct {
    // searchers committed to this pool: threads polling its queues plus
    // wakes in flight (wake_thread pre-accounts its target)
    _Atomic(int32_t) n_spinning;
    char pad0[64 - sizeof(_Atomic(int32_t))];
    // tasks sitting in the pool's Julia-side shared queues, maintained by
    // the queue implementation (insert increments, successful dequeue
    // decrements, via jl_sched_nready_inc/dec, both under the heap lock so
    // a task's decrement cannot precede its increment and undercount the
    // pending work at the gate)
    _Atomic(int64_t) n_ready;
    char pad1[64 - sizeof(_Atomic(int64_t))];
    // most recently parked thread (tid + 1; 0 = none), woken first: the
    // last sleeper has the warmest core (cf. Go's LIFO idle-M stack).
    // Advisory; races are benign.
    _Atomic(int16_t) last_parked;
    char pad2[64 - sizeof(_Atomic(int16_t))];
    // the pool's tid range [lo, lo + n), filled in at init
    int16_t lo;
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

// The wake gate of the pool that owns `tid`. A NULL result means "not a
// pool worker" (foreign/adopted threads, GC threads), and every
// `gate != NULL` check below is that membership test: only pool workers
// can pop the pool queues, so only they count in the searcher accounting.
static wake_gate_t *gate_of_tid(int16_t tid) JL_NOTSAFEPOINT
{
    for (int i = 0; i < JL_N_WAKE_GATES; i++) {
        if (tid >= wake_gates[i].lo && tid < wake_gates[i].lo + wake_gates[i].n)
            return &wake_gates[i];
    }
    return NULL;
}

JL_DLLEXPORT void jl_sched_nready_inc(int8_t tpid) JL_NOTSAFEPOINT
{
    wake_gate_t *gate = gate_of_pool(tpid);
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_ready, 1);
}

JL_DLLEXPORT void jl_sched_nready_dec(int8_t tpid) JL_NOTSAFEPOINT
{
    wake_gate_t *gate = gate_of_pool(tpid);
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_ready, -1);
}

// Release this thread's searcher slot; returns 1 if it was the last one.
static int spin_exit(wake_gate_t *gate) JL_NOTSAFEPOINT
{
    int32_t prev = jl_atomic_fetch_add_relaxed(&gate->n_spinning, -1);
    assert(prev > 0);
    return prev == 1;
}

// Pre-account tid as a searcher for a wake in flight (NULL for threads
// outside every pool). Undone by unaccount_searcher when the wake CAS is
// lost; taken over by the woken thread (settle_wake) when it is won.
static wake_gate_t *preaccount_searcher(int16_t tid) JL_NOTSAFEPOINT
{
    wake_gate_t *gate = gate_of_tid(tid);
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_spinning, 1);
    return gate;
}

static void unaccount_searcher(wake_gate_t *gate) JL_NOTSAFEPOINT
{
    if (gate != NULL)
        jl_atomic_fetch_add_relaxed(&gate->n_spinning, -1);
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
    // equivalent to wake_thread, without the assert on wasrunning
    int8_t state = jl_atomic_load_relaxed(&ptls->sleep_check_state);
    if (state == sleeping) {
        wake_gate_t *gate = preaccount_searcher(ptls->tid);
        if (jl_atomic_cmpswap_relaxed(&ptls->sleep_check_state, &state, not_sleeping)) {
            // this notification will never be consumed, so we may have now
            // introduced some inaccuracy into the count, but that is
            // unavoidable with any asynchronous interruption
            jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
        }
        else {
            unaccount_searcher(gate);
        }
    }
}


static int set_not_sleeping(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != not_sleeping) {
        if (jl_atomic_exchange_relaxed(&ptls->sleep_check_state, not_sleeping) != not_sleeping) {
            return 1;
        }
    }
    int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, -1); // consume in-flight wakeup
    assert(wasrunning > 1); (void)wasrunning;
    return 0;
}

// Leave the sleep transition, consuming a waker's flip: take the
// pre-accounted searcher slot. Returns 1 if we flipped ourselves (no waker
// was involved, so there is nothing to take).
static int settle_wake(jl_ptls_t ptls, wake_gate_t *gate, volatile int *spinning) JL_NOTSAFEPOINT
{
    if (set_not_sleeping(ptls))
        return 1;
    if (gate != NULL)
        *spinning = 1;
    return 0;
}

// Signal a (possibly) parked thread's wake condition.
static void signal_thread_wake(jl_ptls_t ptls2) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&ptls2->sleep_lock);
    uv_cond_signal(&ptls2->wake_signal);
    uv_mutex_unlock(&ptls2->sleep_lock);
}

static int wake_thread(int16_t tid) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];

    if (jl_atomic_load_relaxed(&ptls2->sleep_check_state) != not_sleeping) {
        // The increment comes before the CAS so the target's release cannot
        // underflow; only one waker can win the CAS, so concurrent wakers
        // cannot double-count.
        wake_gate_t *gate = preaccount_searcher(tid);
        int8_t state = sleeping;
        if (jl_atomic_cmpswap_relaxed(&ptls2->sleep_check_state, &state, not_sleeping)) {
            int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, 1); // increment in-flight wakeup count
            assert(wasrunning); (void)wasrunning;
            JL_PROBE_RT_SLEEP_CHECK_WAKE(ptls2, state);
            signal_thread_wake(ptls2);
            return 1;
        }
        unaccount_searcher(gate);
    }
    return 0;
}


static void wake_libuv(void) JL_NOTSAFEPOINT
{
    JULIA_DEBUG_SLEEPWAKE( io_wakeup_enter = cycleclock() );
    jl_wake_libuv();
    JULIA_DEBUG_SLEEPWAKE( io_wakeup_leave = cycleclock() );
}

// The current thread is awake by definition, but make sure it exits any
// partial sleep transition (accounting for the in-flight wakeup), and make
// sure it exits uv_run if it is the thread running the event loop.
static void wake_self(jl_task_t *ct, jl_task_t *uvlock) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = ct->ptls;
    if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != not_sleeping) {
        // Flipping our own mid-transition sleep state pre-accounts like
        // any other wake; our raced-detection downstream takes the slot
        // (settle_wake at one of sleep_thread's exits).
        wake_gate_t *gate = preaccount_searcher(jl_atomic_load_relaxed(&ct->tid));
        if (jl_atomic_exchange_relaxed(&ptls->sleep_check_state, not_sleeping) != not_sleeping) {
            int wasrunning = jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
            assert(wasrunning); (void)wasrunning;
            JL_PROBE_RT_SLEEP_CHECK_WAKEUP(ptls);
        }
        else {
            unaccount_searcher(gate);
        }
    }
    if (uvlock == ct)
        uv_stop(jl_global_event_loop());
}

// Wake `tid` if it is sleeping. On success, if the woken thread is the one
// blocked in uv_run, kick libuv too: now that it is not-sleeping, ensure it
// either has not yet acquired the libuv lock or will observe the state change.
static int wake_thread_and_uv(jl_task_t *ct, jl_task_t *uvlock, int16_t tid) JL_NOTSAFEPOINT
{
    if (!wake_thread(tid))
        return 0;
    if (uvlock != ct) {
        jl_fence();
        jl_ptls_t other = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        jl_task_t *tid_task = jl_atomic_load_relaxed(&other->current_task);
        if (jl_atomic_load_relaxed(&jl_uv_mutex.owner) == tid_task)
            wake_libuv();
    }
    return 1;
}

// Returns 1 if a sleeping thread was transitioned to running (i.e. the wake
// added a running thread), 0 if the target was already awake or is the caller.
static int wakeup_thread(jl_task_t *ct, int16_t tid) JL_NOTSAFEPOINT { // Pass in ptls when we have it already available to save a lookup
    int woke = 0;
    int16_t self = jl_atomic_load_relaxed(&ct->tid);
    if (tid != self)
        jl_fence(); // [^store_buffering_1]
    jl_task_t *uvlock = jl_atomic_load_relaxed(&jl_uv_mutex.owner);
    JULIA_DEBUG_SLEEPWAKE( wakeup_enter = cycleclock() );
    if (tid == self || tid == -1) {
        wake_self(ct, uvlock);
    }
    else {
        // something added to the sticky-queue: notify that thread
        woke = wake_thread_and_uv(ct, uvlock, tid);
    }
    if (tid == -1) {
        // Legacy broadcast wake; prefer jl_wakeup_threadpool.
        int anysleep = 0;
        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        for (tid = 0; tid < nthreads; tid++) {
            if (tid != self)
                anysleep |= wake_thread(tid);
        }
        woke = anysleep;
        // check if we need to notify uv_run too
        if (uvlock != ct && anysleep) {
            jl_fence();
            if (jl_atomic_load_relaxed(&jl_uv_mutex.owner) != NULL)
                wake_libuv();
        }
    }
    JULIA_DEBUG_SLEEPWAKE( wakeup_leave = cycleclock() );
    return woke;
}

/* ensure thread tid is awake if necessary; returns 1 if a sleeping thread was
   woken (a running thread was added), 0 otherwise */
JL_DLLEXPORT int jl_wakeup_thread(int16_t tid)
{
    jl_task_t *ct = jl_current_task;
    return wakeup_thread(ct, tid);
}

// Round-robin start hint for jl_wakeup_threadpool, sharded across cache-line-padded
// stripes so concurrent producers don't contend on a single counter.
#define POOL_WAKE_HINT_STRIPES 64
typedef struct {
    _Atomic(uint32_t) v;
    char pad[64 - sizeof(_Atomic(uint32_t))];
} pool_wake_hint_t;
static pool_wake_hint_t pool_wake_hints[POOL_WAKE_HINT_STRIPES];

// Wake one sleeping thread in the gate's pool, preferring the most
// recently parked one (its core is the warmest), falling back to a striped
// round-robin scan. Not gated on the searcher count.
static void wake_one_in_pool(jl_task_t *ct, jl_task_t *uvlock, wake_gate_t *gate, int16_t self) JL_NOTSAFEPOINT
{
    int16_t lo = gate->lo;
    int16_t n = gate->n;

    int16_t hinted = (int16_t)(jl_atomic_load_relaxed(&gate->last_parked) - 1);
    if (hinted >= lo && hinted < lo + n && hinted != self &&
        wake_thread_and_uv(ct, uvlock, hinted))
        return;
    if (n > 0) {
        uint32_t stripe = ((uint32_t)self) & (POOL_WAKE_HINT_STRIPES - 1);
        uint32_t start = jl_atomic_fetch_add_relaxed(&pool_wake_hints[stripe].v, 1);
        for (int16_t k = 0; k < n; k++) {
            int16_t tid = lo + (int16_t)((start + (uint32_t)k) % (uint32_t)n);
            if (tid != self && wake_thread_and_uv(ct, uvlock, tid))
                return;
        }
    }
}

// Wake at most one sleeping thread in the gate's pool, if the count gate
// allows it. See devdocs/scheduler-wakeup.
static void gated_wakeup(wake_gate_t *gate) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    int16_t self = jl_atomic_load_relaxed(&ct->tid);
    jl_fence(); // [^store_buffering_1]
    jl_task_t *uvlock = jl_atomic_load_relaxed(&jl_uv_mutex.owner);
    JULIA_DEBUG_SLEEPWAKE( wakeup_enter = cycleclock() );
    // Make sure we are awake
    wake_self(ct, uvlock);

    // Count gate: skip the wake while committed searchers (including wakes
    // in flight) cover the pending work. The skip cannot strand work: an
    // exiting searcher re-runs this gate, and a parking one re-checks the
    // queues after publishing its sleep state.
    if ((int64_t)jl_atomic_load_relaxed(&gate->n_spinning) >=
            jl_atomic_load_relaxed(&gate->n_ready)) {
        JULIA_DEBUG_SLEEPWAKE( wakeup_leave = cycleclock() );
        return;
    }
    wake_one_in_pool(ct, uvlock, gate, self);
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

// Stop being a searcher: release the slot (if held) and run the exit
// handoff -- the last searcher out is responsible for waking another
// thread if work remains.
static void searcher_exit(wake_gate_t *gate, volatile int *spinning) JL_NOTSAFEPOINT
{
    if (*spinning) {
        *spinning = 0;
        if (spin_exit(gate))
            gated_wakeup(gate);
    }
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
    return jl_atomic_load_relaxed(&ptls->sleep_check_state) == sleeping;
}


// The sleep transition:
//   RELEASE: release the searcher slot
//   PUBLISH: publish the sleep state
//   RECHECK: recheck if there is work [^store_buffering_1]
//   RETIRE:  leave the running count
//   PARK:    park the thread
// This function may find a task to run during the recheck, aborting the
// park; it returns NULL after a normal wake.
static jl_task_t *sleep_thread(jl_task_t *ct, wake_gate_t **pgate,
                               volatile int *spinning, uint64_t *start_cycles,
                               jl_value_t *trypoptask, jl_value_t *q,
                               jl_value_t *checkempty, int force_park) JL_CANSAFEPOINT
{
    jl_ptls_t ptls = ct->ptls;
    wake_gate_t *gate = *pgate;
    jl_task_t *task = NULL;
    // RELEASE: release the searcher slot
    if (*spinning) {
        // release the slot before the sleeping-store + fence: pairs with
        // the wakeup gate's n_spinning load
        *spinning = 0;
        spin_exit(gate);
    }
    // acquire sleep-check lock
    assert(jl_atomic_load_relaxed(&ptls->sleep_check_state) == not_sleeping);
    // PUBLISH: publish the sleep state
    jl_atomic_store_relaxed(&ptls->sleep_check_state, sleeping);
    jl_fence(); // [^store_buffering_1]
    JL_PROBE_RT_SLEEP_CHECK_SLEEP(ptls);
    volatile int isrunning = 1;
    JL_TRY {
        // `continue` exits the JL_TRY (popping the handler) and falls
        // through to the return below.
        // RECHECK: recheck if there is work. Runs inside the handler:
        // checkempty is a Julia callback, and a throw here would otherwise
        // unwind with our sleep state still published.
        if (!force_park && !check_empty(checkempty)) { // uses relaxed loads
            if (settle_wake(ptls, gate, spinning)) {
                JL_PROBE_RT_SLEEP_CHECK_TASKQ_WAKE(ptls);
            }
            continue;
        }
        task = get_next_task(trypoptask, q); // note: this should not yield
        if (ptls != ct->ptls) {
            // sigh, a yield was detected, so let's go ahead and handle it anyway by starting over
            ptls = ct->ptls;
            gate = gate_of_tid(jl_atomic_load_relaxed(&ct->tid));
            *pgate = gate;
            if (settle_wake(ptls, gate, spinning)) {
                JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls);
            }
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

        // RETIRE: leave the running count. Any thread which wants us
        // running again will have to observe sleep_check_state==sleeping
        // and increment n_threads_running for us.
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
            // PARK: park the thread
            uv_cond_wait(&ptls->wake_signal, &ptls->sleep_lock);
        }
        assert(jl_atomic_load_relaxed(&ptls->sleep_check_state) == not_sleeping);
        assert(jl_atomic_load_relaxed(&n_threads_running));
        *start_cycles = 0;
        uv_mutex_unlock(&ptls->sleep_lock);
        JULIA_DEBUG_SLEEPWAKE( ptls->sleep_leave = cycleclock() );
        jl_gc_safe_leave(ptls, gc_state); // contains jl_gc_safepoint
        if (task) {
            assert(task == wait_empty);
            wait_empty = NULL;
            continue;
        }
        // We got woken: take the slot the waker pre-accounted (the
        // wait_empty path above is the only self-flip out of the wait loop).
        if (gate != NULL)
            *spinning = 1;
    }
    JL_CATCH {
        // InterruptException or error in scheduler/libuv
        if (!isrunning)
            jl_atomic_fetch_add_relaxed(&n_threads_running, 1);
        // A wake can accompany the exception: an ordinary wake may race it,
        // and SIGINT delivery itself flips a parked thread (surprise_wakeup)
        // so it reaches the safepoint that throws. Either way the waker
        // pre-accounted a searcher slot we will never use: record it in
        // *spinning so the caller's handler releases it (searcher_exit in
        // jl_task_get_next). With no waker, settle_wake just un-publishes
        // our sleep state.
        settle_wake(ptls, gate, spinning);
        // An enqueue whose wake was suppressed by the slot we released at
        // entry is observed only by the recheck; if the unwind cut the
        // recheck short, hand the wake on.
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
    // NULL for threads outside every pool (foreign/adopted): legacy
    // poll/park behavior, no accounting.
    wake_gate_t *gate = gate_of_tid(jl_atomic_load_relaxed(&ct->tid));
    // Whether this search loop holds a searcher slot. Frame-local rather
    // than a ptls field because the loop runs on the task's stack and can
    // migrate threads on an unforeseen yield; tasks are pool-bound, so the
    // slot (counted on the pool's gate) stays valid across the migration.
    // volatile: read in the outer JL_CATCH after a longjmp.
    volatile int spinning = 0;
    jl_task_t *task = NULL;

    // In case we unwind we must release the searcher slot. Either an
    // InterruptException or an error in the scheduler/libuv can unwind us.
    JL_TRY {
    while (1) {
        task = get_next_task(trypoptask, q);
        if (task == NULL) {
            jl_ptls_t ptls = ct->ptls;
            int is_io_thread = ptls->tid == jl_atomic_load_relaxed(&io_loop_tid);
            if (!spinning && gate != NULL && !is_io_thread) {
                int32_t ns = jl_atomic_load_relaxed(&gate->n_spinning);
                if (2 * ns < gate->n) {
                    jl_atomic_fetch_add_relaxed(&gate->n_spinning, 1);
                    spinning = 1;
                }
            }
            // Denied a spinner slot: park without polling. The post-fence
            // get_next_task retry in sleep_thread keeps this race-free.
            int force_park = gate != NULL && !spinning && !is_io_thread;

            // quick, race-y check to see if there seems to be any stuff in there
            jl_cpu_pause();
            if (!force_park && !check_empty(checkempty)) {
                start_cycles = 0;
                continue;
            }

            jl_cpu_pause();
            if (force_park ||
                sleep_check_after_threshold(&start_cycles) ||
                (is_io_thread && (!jl_atomic_load_relaxed(&_threadedregion) ||
                    wait_empty))) {
                task = sleep_thread(ct, &gate, &spinning, &start_cycles,
                                    trypoptask, q, checkempty, force_park);
                // unpooled threads should never spin
                assert(!spinning || gate != NULL);
            }
            else {
                // maybe check the kernel for new messages too
                jl_process_events();
            }
        }
        if (task) {
            // We found a task to run
            // Perform the handoff
            searcher_exit(gate, &spinning);
            break;
        }
    }
    }
    JL_CATCH {
        // An unwinding searcher performs no post-fence queue recheck, so it
        // owes the same exit handoff as one that found work.
        searcher_exit(gate, &spinning);
        jl_rethrow();
    }
    return task;
}

void scheduler_delete_thread(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    int notsleeping = jl_atomic_exchange_relaxed(&ptls->sleep_check_state, sleeping_like_the_dead) == not_sleeping;
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
