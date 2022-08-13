// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "julia.h"
#include "julia_internal.h"
#include "gc.h"
#include "threading.h"
#include "options.h"

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
        return jl_atomic_cmpswap(&task->tid, &was, tid);
    return 0;
}

JL_DLLEXPORT int jl_set_task_threadpoolid(jl_task_t *task, int8_t tpid) JL_NOTSAFEPOINT
{
    if (tpid < 0 || tpid >= jl_n_threadpools)
        return 0;
    task->threadpoolid = tpid;
    return 1;
}

// GC functions used
extern int jl_gc_mark_queue_obj_explicit(jl_gc_mark_cache_t *gc_cache,
                                         jl_gc_markqueue_t *mq, jl_value_t *obj) JL_NOTSAFEPOINT;

// parallel task runtime
// ---

JL_DLLEXPORT uint32_t jl_rand_ptls(uint32_t max, uint32_t unbias)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    // one-extend unbias back to 64-bits
    return cong(max, -(uint64_t)-unbias, &ptls->rngseed);
}

// initialize the threading infrastructure
// (called only by the main thread)
void jl_init_threadinginfra(void)
{
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


void JL_NORETURN jl_finish_task(jl_task_t *t);

// thread function: used by all except the main thread and gc threads
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
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    (void)jl_gc_unsafe_enter(ptls);
    jl_finish_task(ct); // noreturn
}

// Thread recruitment scheme inspired by Hassanein's "spin-master",
// `Understanding and Improving JVM GC Work Stealing at the
// Data Center Scale`

const uint64_t timeout_ns = 1e5;

jl_mutex_t spinmaster_lock;
uv_mutex_t sweep_lock;
uv_cond_t sweep_cond;

extern void gc_mark_loop(jl_ptls_t ptls);
extern _Atomic(int32_t) nworkers_marking;

int jl_spinmaster_all_workers_done(jl_ptls_t ptls)
{
    return (jl_atomic_load_acquire(&nworkers_marking) == 0);
}

int64_t jl_spinmaster_count_work(jl_ptls_t ptls)
{
    int64_t work = 0;
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        jl_gc_markqueue_t *mq2 = &ptls2->mark_queue;
        ws_queue_t *q2 = &mq2->q;
        // This count can be slightly off, but it doesn't matter
        // for recruitment heuristics
        int64_t t2 = jl_atomic_load_relaxed(&q2->top);
        int64_t b2 = jl_atomic_load_relaxed(&q2->bottom);
        work += b2 - t2;
    }
    return work;
}

void jl_spinmaster_notify_all(jl_ptls_t ptls)
{
    for (int i = 0; i < jl_n_threads; i++) {
        if (i == ptls->tid)
            continue;
        uv_cond_signal(&jl_all_tls_states[i]->wake_signal);
    }
}

void jl_spinmaster_recruit_workers(jl_ptls_t ptls, size_t nworkers)
{
    for (int i = 0; i < jl_n_threads && nworkers > 0; i++) {
        if (i == ptls->tid)
            continue;
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        if (jl_atomic_load_acquire(&ptls2->gc_state) == JL_GC_STATE_WAITING) {
            uv_cond_signal(&ptls->wake_signal);
            nworkers--;
        }
    }
}

int jl_spinmaster_end_marking(jl_ptls_t ptls)
{
    // Fast path for mark-loop termination
    if (jl_spinmaster_all_workers_done(ptls)) {
        return 1;
    }
    if (jl_mutex_trylock_nogc(&spinmaster_lock)) {
        spin : {
            if (!jl_spinmaster_all_workers_done(ptls)) {
                int64_t work = jl_spinmaster_count_work(ptls);
                if (work > 1) {
                    jl_spinmaster_recruit_workers(ptls, work - 1);
                    jl_mutex_unlock_nogc(&spinmaster_lock);
                    gc_mark_loop(ptls);
                    return 0;
                }
                jl_cpu_pause();
                goto spin;
            }
        }
        jl_spinmaster_notify_all(ptls);
        jl_mutex_unlock_nogc(&spinmaster_lock);
        return 1;
    }
    return 0;
}

void jl_spinmaster_wait_pmark(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    while(!jl_spinmaster_end_marking(ptls)) {
        uv_mutex_lock(&ptls->sleep_lock);
        if (!uv_cond_timedwait(&ptls->wake_signal,
                               &ptls->sleep_lock, timeout_ns)) {
            // Stopped waiting because we got a notification
            // from spin-master: try to get recruited
            gc_mark_loop(ptls);
        }
        uv_mutex_unlock(&ptls->sleep_lock);
    }
}

void jl_spinmaster_wait_sweeping(void)
{
    // Use system mutexes rather than spin locking to minimize wasted CPU
    // time on the idle cores while we wait for the GC to finish.
    // This is particularly important when run under rr.
    uv_mutex_lock(&sweep_lock);
    uv_cond_wait(&sweep_cond, &sweep_lock);
    uv_mutex_unlock(&sweep_lock);
}

// thread function: used by gc threads
void jl_gc_threadfun(void *arg)
{
    jl_threadarg_t *targ = (jl_threadarg_t*)arg;

    // initialize this thread (set tid, create heap, set up root task)
    jl_ptls_t ptls = jl_init_threadtls(targ->tid);
    
    // wait for all threads
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    while (1) {
        // jl_spinmaster_wait_pmark();
        jl_spinmaster_wait_sweeping();
    }
}

int jl_running_under_rr(int recheck)
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
static int sleep_check_after_threshold(uint64_t *start_cycles)
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


static int wake_thread(int16_t tid)
{
    jl_ptls_t other = jl_all_tls_states[tid];
    int8_t state = sleeping;

    if (jl_atomic_load_relaxed(&other->sleep_check_state) == sleeping) {
        if (jl_atomic_cmpswap_relaxed(&other->sleep_check_state, &state, not_sleeping)) {
            JL_PROBE_RT_SLEEP_CHECK_WAKE(other, state);
            uv_mutex_lock(&other->sleep_lock);
            uv_cond_signal(&other->wake_signal);
            uv_mutex_unlock(&other->sleep_lock);
            return 1;
        }
    }
    return 0;
}


static void wake_libuv(void)
{
    JULIA_DEBUG_SLEEPWAKE( io_wakeup_enter = cycleclock() );
    jl_wake_libuv();
    JULIA_DEBUG_SLEEPWAKE( io_wakeup_leave = cycleclock() );
}

/* ensure thread tid is awake if necessary */
JL_DLLEXPORT void jl_wakeup_thread(int16_t tid)
{
    jl_task_t *ct = jl_current_task;
    int16_t self = jl_atomic_load_relaxed(&ct->tid);
    if (tid != self)
        jl_fence(); // [^store_buffering_1]
    jl_task_t *uvlock = jl_atomic_load_relaxed(&jl_uv_mutex.owner);
    JULIA_DEBUG_SLEEPWAKE( wakeup_enter = cycleclock() );
    if (tid == self || tid == -1) {
        // we're already awake, but make sure we'll exit uv_run
        jl_ptls_t ptls = ct->ptls;
        if (jl_atomic_load_relaxed(&ptls->sleep_check_state) == sleeping) {
            jl_atomic_store_relaxed(&ptls->sleep_check_state, not_sleeping);
            JL_PROBE_RT_SLEEP_CHECK_WAKEUP(ptls);
        }
        if (uvlock == ct)
            uv_stop(jl_global_event_loop());
    }
    else {
        // something added to the sticky-queue: notify that thread
        if (wake_thread(tid)) {
            // check if we need to notify uv_run too
            jl_fence();
            jl_ptls_t other = jl_all_tls_states[tid];
            jl_task_t *tid_task = jl_atomic_load_relaxed(&other->current_task);
            // now that we have changed the thread to not-sleeping, ensure that
            // either it has not yet acquired the libuv lock, or that it will
            // observe the change of state to not_sleeping
            if (uvlock != ct && jl_atomic_load_relaxed(&jl_uv_mutex.owner) == tid_task)
                wake_libuv();
        }
    }
    // check if the other threads might be sleeping
    if (tid == -1) {
        // something added to the multi-queue: notify all threads
        // in the future, we might want to instead wake some fraction of threads,
        // and let each of those wake additional threads if they find work
        int anysleep = 0;
        for (tid = 0; tid < jl_n_threads; tid++) {
            if (tid != self)
                anysleep |= wake_thread(tid);
        }
        // check if we need to notify uv_run too
        if (uvlock != ct && anysleep) {
            jl_fence();
            if (jl_atomic_load_relaxed(&jl_uv_mutex.owner) != NULL)
                wake_libuv();
        }
    }
    JULIA_DEBUG_SLEEPWAKE( wakeup_leave = cycleclock() );
}


// get the next runnable task
static jl_task_t *get_next_task(jl_value_t *trypoptask, jl_value_t *q)
{
    jl_gc_safepoint();
    jl_task_t *task = (jl_task_t*)jl_apply_generic(trypoptask, &q, 1);
    if (jl_typeis(task, jl_task_type)) {
        int self = jl_atomic_load_relaxed(&jl_current_task->tid);
        jl_set_task_tid(task, self);
        return task;
    }
    return NULL;
}

static int check_empty(jl_value_t *checkempty)
{
    return jl_apply_generic(checkempty, NULL, 0) == jl_true;
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

extern _Atomic(unsigned) _threadedregion;

JL_DLLEXPORT jl_task_t *jl_task_get_next(jl_value_t *trypoptask, jl_value_t *q, jl_value_t *checkempty)
{
    jl_task_t *ct = jl_current_task;
    uint64_t start_cycles = 0;

    while (1) {
        jl_task_t *task = get_next_task(trypoptask, q);
        if (task)
            return task;

        // quick, race-y check to see if there seems to be any stuff in there
        jl_cpu_pause();
        if (!check_empty(checkempty)) {
            start_cycles = 0;
            continue;
        }

        jl_cpu_pause();
        jl_ptls_t ptls = ct->ptls;
        if (sleep_check_after_threshold(&start_cycles) || (!jl_atomic_load_relaxed(&_threadedregion) && ptls->tid == 0)) {
            // acquire sleep-check lock
            jl_atomic_store_relaxed(&ptls->sleep_check_state, sleeping);
            jl_fence(); // [^store_buffering_1]
            JL_PROBE_RT_SLEEP_CHECK_SLEEP(ptls);
            if (!check_empty(checkempty)) { // uses relaxed loads
                if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != not_sleeping) {
                    jl_atomic_store_relaxed(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                    JL_PROBE_RT_SLEEP_CHECK_TASKQ_WAKE(ptls);
                }
                continue;
            }
            task = get_next_task(trypoptask, q); // note: this should not yield
            if (ptls != ct->ptls) {
                // sigh, a yield was detected, so let's go ahead and handle it anyway by starting over
                ptls = ct->ptls;
                if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != not_sleeping) {
                    jl_atomic_store_relaxed(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                    JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls);
                }
                if (task)
                    return task;
                continue;
            }
            if (task) {
                if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != not_sleeping) {
                    jl_atomic_store_relaxed(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                    JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls);
                }
                return task;
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
            else if (ptls->tid == 0) {
                uvlock = 1;
                JL_UV_LOCK(); // jl_mutex_lock(&jl_uv_mutex);
            }
            if (uvlock) {
                int active = 1;
                // otherwise, we block until someone asks us for the lock
                uv_loop_t *loop = jl_global_event_loop();
                while (active && may_sleep(ptls)) {
                    if (jl_atomic_load_relaxed(&jl_uv_n_waiters) != 0)
                        // but if we won the race against someone who actually needs
                        // the lock to do real work, we need to let them have it instead
                        break;
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
                if (!may_sleep(ptls)) {
                    start_cycles = 0;
                    continue;
                }
                if (!jl_atomic_load_relaxed(&_threadedregion) && active && ptls->tid == 0) {
                    // thread 0 is the only thread permitted to run the event loop
                    // so it needs to stay alive, just spin-looping if necessary
                    if (jl_atomic_load_relaxed(&ptls->sleep_check_state) != not_sleeping) {
                        jl_atomic_store_relaxed(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                        JL_PROBE_RT_SLEEP_CHECK_UV_WAKE(ptls);
                    }
                    start_cycles = 0;
                    continue;
                }
            }

            // the other threads will just wait for an individual wake signal to resume
            JULIA_DEBUG_SLEEPWAKE( ptls->sleep_enter = cycleclock() );
            int8_t gc_state = jl_gc_safe_enter(ptls);
            uv_mutex_lock(&ptls->sleep_lock);
            while (may_sleep(ptls)) {
                uv_cond_wait(&ptls->wake_signal, &ptls->sleep_lock);
            }
            assert(jl_atomic_load_relaxed(&ptls->sleep_check_state) == not_sleeping);
            uv_mutex_unlock(&ptls->sleep_lock);
            JULIA_DEBUG_SLEEPWAKE( ptls->sleep_leave = cycleclock() );
            jl_gc_safe_leave(ptls, gc_state); // contains jl_gc_safepoint
            start_cycles = 0;
        }
        else {
            // maybe check the kernel for new messages too
            jl_process_events();
        }
    }
}

#ifdef __cplusplus
}
#endif
