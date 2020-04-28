// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "julia.h"
#include "julia_internal.h"
#include "gc.h"
#include "threading.h"

#ifdef __cplusplus
extern "C" {
#endif


// thread sleep state

// thread should not be sleeping--it might need to do work.
static const int16_t not_sleeping = 0;

// it is acceptable for the thread to be sleeping.
static const int16_t sleeping = 1;

// invariant: No thread is ever asleep unless sleep_check_state is sleeping (or we have a wakeup signal pending).
// invariant: Any particular thread is not asleep unless that thread's sleep_check_state is sleeping.
// invariant: The transition of a thread state to sleeping must be followed by a check that there wasn't work pending for it.
// information: Observing thread not-sleeping is sufficient to ensure the target thread will subsequently inspect its local queue.
// information: Observing thread is-sleeping says it may be necessary to notify it at least once to wakeup. It may already be awake however for a variety of reasons.

JULIA_DEBUG_SLEEPWAKE(
uint64_t wakeup_enter;
uint64_t wakeup_leave;
uint64_t io_wakeup_enter;
uint64_t io_wakeup_leave;
);


JL_DLLEXPORT int jl_set_task_tid(jl_task_t *task, int tid) JL_NOTSAFEPOINT
{
    // Try to acquire the lock on this task.
    int16_t was = task->tid;
    if (was == tid)
        return 1;
    if (was == -1)
        return jl_atomic_bool_compare_exchange(&task->tid, -1, tid);
    return 0;
}

// GC functions used
extern int jl_gc_mark_queue_obj_explicit(jl_gc_mark_cache_t *gc_cache,
                                         jl_gc_mark_sp_t *sp, jl_value_t *obj) JL_NOTSAFEPOINT;

// multiq
// ---

/* a task heap */
typedef struct taskheap_tag {
    jl_mutex_t lock;
    jl_task_t **tasks;
    int32_t ntasks;
    int16_t prio;
} taskheap_t;

/* multiqueue parameters */
static const int32_t heap_d = 8;
static const int heap_c = 2;

/* size of each heap */
static const int tasks_per_heap = 65536; // TODO: this should be smaller by default, but growable!

/* the multiqueue's heaps */
static taskheap_t *heaps;
static int32_t heap_p;

/* unbias state for the RNG */
static uint64_t cong_unbias;


static inline void multiq_init(void)
{
    heap_p = heap_c * jl_n_threads;
    heaps = (taskheap_t *)calloc(heap_p, sizeof(taskheap_t));
    for (int32_t i = 0; i < heap_p; ++i) {
        jl_mutex_init(&heaps[i].lock);
        heaps[i].tasks = (jl_task_t **)calloc(tasks_per_heap, sizeof(jl_task_t*));
        heaps[i].ntasks = 0;
        heaps[i].prio = INT16_MAX;
    }
    unbias_cong(heap_p, &cong_unbias);
}


static inline void sift_up(taskheap_t *heap, int32_t idx)
{
    if (idx > 0) {
        int32_t parent = (idx-1)/heap_d;
        if (heap->tasks[idx]->prio < heap->tasks[parent]->prio) {
            jl_task_t *t = heap->tasks[parent];
            heap->tasks[parent] = heap->tasks[idx];
            heap->tasks[idx] = t;
            sift_up(heap, parent);
        }
    }
}


static inline void sift_down(taskheap_t *heap, int32_t idx)
{
    if (idx < heap->ntasks) {
        for (int32_t child = heap_d*idx + 1;
                child < tasks_per_heap && child <= heap_d*idx + heap_d;
                ++child) {
            if (heap->tasks[child]
                    && heap->tasks[child]->prio <= heap->tasks[idx]->prio) {
                jl_task_t *t = heap->tasks[idx];
                heap->tasks[idx] = heap->tasks[child];
                heap->tasks[child] = t;
                sift_down(heap, child);
            }
        }
    }
}


static inline int multiq_insert(jl_task_t *task, int16_t priority)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    uint64_t rn;

    task->prio = priority;
    do {
        rn = cong(heap_p, cong_unbias, &ptls->rngseed);
    } while (!jl_mutex_trylock_nogc(&heaps[rn].lock));

    if (heaps[rn].ntasks >= tasks_per_heap) {
        jl_mutex_unlock_nogc(&heaps[rn].lock);
        // multiq insertion failed, increase #tasks per heap
        return -1;
    }

    heaps[rn].tasks[heaps[rn].ntasks++] = task;
    sift_up(&heaps[rn], heaps[rn].ntasks-1);
    int16_t prio = jl_atomic_load(&heaps[rn].prio);
    if (task->prio < prio)
        jl_atomic_store(&heaps[rn].prio, task->prio);
    jl_mutex_unlock_nogc(&heaps[rn].lock);

    return 0;
}


static inline jl_task_t *multiq_deletemin(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    uint64_t rn1 = 0, rn2;
    int32_t i;
    int16_t prio1, prio2;
    jl_task_t *task;
 retry:
    for (i = 0; i < heap_p; ++i) {
        rn1 = cong(heap_p, cong_unbias, &ptls->rngseed);
        rn2 = cong(heap_p, cong_unbias, &ptls->rngseed);
        prio1 = jl_atomic_load(&heaps[rn1].prio);
        prio2 = jl_atomic_load(&heaps[rn2].prio);
        if (prio1 > prio2) {
            prio1 = prio2;
            rn1 = rn2;
        }
        else if (prio1 == prio2 && prio1 == INT16_MAX)
            continue;
        if (jl_mutex_trylock_nogc(&heaps[rn1].lock)) {
            if (prio1 == heaps[rn1].prio)
                break;
            jl_mutex_unlock_nogc(&heaps[rn1].lock);
        }
    }
    if (i == heap_p)
        return NULL;

    task = heaps[rn1].tasks[0];
    if (!jl_set_task_tid(task, ptls->tid)) {
        jl_mutex_unlock_nogc(&heaps[rn1].lock);
        goto retry;
    }
    heaps[rn1].tasks[0] = heaps[rn1].tasks[--heaps[rn1].ntasks];
    heaps[rn1].tasks[heaps[rn1].ntasks] = NULL;
    prio1 = INT16_MAX;
    if (heaps[rn1].ntasks > 0) {
        sift_down(&heaps[rn1], 0);
        prio1 = heaps[rn1].tasks[0]->prio;
    }
    jl_atomic_store(&heaps[rn1].prio, prio1);
    jl_mutex_unlock_nogc(&heaps[rn1].lock);

    return task;
}


void jl_gc_mark_enqueued_tasks(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp)
{
    int32_t i, j;
    for (i = 0; i < heap_p; ++i)
        for (j = 0; j < heaps[i].ntasks; ++j)
            jl_gc_mark_queue_obj_explicit(gc_cache, sp, (jl_value_t *)heaps[i].tasks[j]);
}


static int multiq_check_empty(void)
{
    int32_t i;
    for (i = 0; i < heap_p; ++i) {
        if (heaps[i].ntasks != 0)
            return 0;
    }
    return 1;
}



// parallel task runtime
// ---

// initialize the threading infrastructure
void jl_init_threadinginfra(void)
{
    /* initialize the synchronization trees pool and the multiqueue */
    multiq_init();

    jl_ptls_t ptls = jl_get_ptls_states();
    uv_mutex_init(&ptls->sleep_lock);
    uv_cond_init(&ptls->wake_signal);
}


void JL_NORETURN jl_finish_task(jl_task_t *t, jl_value_t *resultval JL_MAYBE_UNROOTED);

// thread function: used by all except the main thread
void jl_threadfun(void *arg)
{
    jl_threadarg_t *targ = (jl_threadarg_t*)arg;

    // initialize this thread (set tid, create heap, set up root task)
    jl_init_threadtls(targ->tid);
    void *stack_lo, *stack_hi;
    jl_init_stack_limits(0, &stack_lo, &stack_hi);
    jl_init_root_task(stack_lo, stack_hi);

    jl_ptls_t ptls = jl_get_ptls_states();

    // set up sleep mechanism for this thread
    uv_mutex_init(&ptls->sleep_lock);
    uv_cond_init(&ptls->wake_signal);

    // wait for all threads
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    (void)jl_gc_unsafe_enter(ptls);
    jl_current_task->exception = jl_nothing;
    jl_finish_task(jl_current_task, jl_nothing); // noreturn
}


// enqueue the specified task for execution
JL_DLLEXPORT int jl_enqueue_task(jl_task_t *task)
{
    if (multiq_insert(task, task->prio) == -1)
        return 1;
    return 0;
}


static int running_under_rr(void)
{
#ifdef _OS_LINUX_
#define RR_CALL_BASE 1000
#define SYS_rrcall_check_presence (RR_CALL_BASE + 8)
    static int checked_running_under_rr = 0;
    static int is_running_under_rr = 0;
    if (!checked_running_under_rr) {
        int ret = syscall(SYS_rrcall_check_presence, 0, 0, 0, 0, 0, 0);
        if (ret == -1) {
            // Should always be ENOSYS, but who knows what people do for
            // unknown syscalls with their seccomp filters, so just say
            // that we don't have rr.
            is_running_under_rr = 0;
        }
        else {
            is_running_under_rr = 1;
        }
        checked_running_under_rr = 1;
    }
    return is_running_under_rr;
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
    if (running_under_rr())
        return 1;
    if (!(*start_cycles)) {
        *start_cycles = jl_hrtime();
        return 0;
    }
    uint64_t elapsed_cycles = jl_hrtime() - (*start_cycles);
    if (elapsed_cycles >= DEFAULT_THREAD_SLEEP_THRESHOLD) {
        *start_cycles = 0;
        return 1;
    }
    return 0;
}


static void wake_thread(int16_t tid)
{
    jl_ptls_t other = jl_all_tls_states[tid];
    if (jl_atomic_load(&other->sleep_check_state) != not_sleeping) {
        int16_t state = jl_atomic_exchange(&other->sleep_check_state, not_sleeping); // prohibit it from sleeping
        if (state == sleeping) { // see if it was possibly sleeping before now
            uv_mutex_lock(&other->sleep_lock);
            uv_cond_signal(&other->wake_signal);
            uv_mutex_unlock(&other->sleep_lock);
        }
    }
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
    jl_ptls_t ptls = jl_get_ptls_states();
    int16_t uvlock = jl_atomic_load(&jl_uv_mutex.owner);
    int16_t self = ptls->tid;
    unsigned long system_self = jl_all_tls_states[self]->system_id;
    JULIA_DEBUG_SLEEPWAKE( wakeup_enter = cycleclock() );
    if (tid == self || tid == -1) {
        // we're already awake, but make sure we'll exit uv_run
        if (ptls->sleep_check_state != not_sleeping)
            jl_atomic_store(&ptls->sleep_check_state, not_sleeping);
        if (uvlock == system_self)
            uv_stop(jl_global_event_loop());
    }
    else {
        // something added to the sticky-queue: notify that thread
        wake_thread(tid);
        // check if we need to notify uv_run too
        unsigned long system_tid = jl_all_tls_states[tid]->system_id;
        if (uvlock != system_self && jl_atomic_load(&jl_uv_mutex.owner) == system_tid)
            wake_libuv();
    }
    // check if the other threads might be sleeping
    if (tid == -1) {
        // something added to the multi-queue: notify all threads
        // in the future, we might want to instead wake some fraction of threads,
        // and let each of those wake additional threads if they find work
        for (tid = 0; tid < jl_n_threads; tid++) {
            if (tid != self)
                wake_thread(tid);
        }
        // check if we need to notify uv_run too
        if (uvlock != system_self && jl_atomic_load(&jl_uv_mutex.owner) != 0)
            wake_libuv();
    }
    JULIA_DEBUG_SLEEPWAKE( wakeup_leave = cycleclock() );
}


// get the next runnable task from the multiq
static jl_task_t *get_next_task(jl_value_t *trypoptask, jl_value_t *q)
{
    jl_gc_safepoint();
    jl_value_t *args[2] = { trypoptask, q };
    jl_task_t *task = (jl_task_t*)jl_apply(args, 2);
    if (jl_typeis(task, jl_task_type)) {
        int self = jl_get_ptls_states()->tid;
        jl_set_task_tid(task, self);
        return task;
    }
    jl_gc_safepoint();
    return multiq_deletemin();
}

static int may_sleep(jl_ptls_t ptls)
{
    return ptls->sleep_check_state == sleeping;
}

extern volatile unsigned _threadedregion;

JL_DLLEXPORT jl_task_t *jl_task_get_next(jl_value_t *trypoptask, jl_value_t *q)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    uint64_t start_cycles = 0;
    jl_task_t *task;

    while (1) {
        task = get_next_task(trypoptask, q);
        if (task)
            return task;

        // quick, race-y check to see if there seems to be any stuff in there
        jl_cpu_pause();
        if (!multiq_check_empty()) {
            start_cycles = 0;
            continue;
        }

        jl_cpu_pause();
        if (sleep_check_after_threshold(&start_cycles) || (!_threadedregion && ptls->tid == 0)) {
            jl_atomic_store(&ptls->sleep_check_state, sleeping); // acquire sleep-check lock
            if (!multiq_check_empty()) {
                if (ptls->sleep_check_state != not_sleeping)
                    jl_atomic_store(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                continue;
            }
            task = get_next_task(trypoptask, q);
            if (task) {
                if (ptls->sleep_check_state != not_sleeping)
                    jl_atomic_store(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                return task;
            }

            // one thread should win this race and watch the event loop
            // inside a threaded region, any thread can listen for IO messages,
            // although none are allowed to create new ones
            // outside of threaded regions, all IO is permitted,
            // but only on thread 1
            int uvlock = 0;
            if (_threadedregion) {
                uvlock = jl_mutex_trylock(&jl_uv_mutex);
            }
            else if (ptls->tid == 0) {
                uvlock = 1;
                JL_UV_LOCK(); // jl_mutex_lock(&jl_uv_mutex);
            }
            if (uvlock) {
                int active = 1;
                if (jl_atomic_load(&jl_uv_n_waiters) != 0) {
                    // but if we won the race against someone who actually needs
                    // the lock to do real work, we need to let them have it instead
                    JL_UV_UNLOCK();
                }
                else {
                    // otherwise, we may block until someone asks us for the lock
                    uv_loop_t *loop = jl_global_event_loop();
                    jl_gc_safepoint();
                    if (may_sleep(ptls)) {
                        loop->stop_flag = 0;
                        JULIA_DEBUG_SLEEPWAKE( ptls->uv_run_enter = cycleclock() );
                        active = uv_run(loop, UV_RUN_ONCE);
                        JULIA_DEBUG_SLEEPWAKE( ptls->uv_run_leave = cycleclock() );
                    }
                    JL_UV_UNLOCK();
                    // optimization: check again first if we may have work to do
                    if (!may_sleep(ptls)) {
                        if (ptls->sleep_check_state != not_sleeping)
                            jl_atomic_store(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                        start_cycles = 0;
                        continue;
                    }
                    // otherwise, we got a spurious wakeup since some other
                    // thread that just wanted to steal libuv from us,
                    // just go right back to sleep on the other wake signal
                    // to let them take it from us without conflict
                    // TODO: this relinquishes responsibility for all event
                    //       to the last thread to do an explicit operation,
                    //       which may starve other threads of critical work
                    if (jl_atomic_load(&jl_uv_n_waiters) == 0) {
                        continue;
                    }
                }
                if (!_threadedregion && active && ptls->tid == 0) {
                    // thread 0 is the only thread permitted to run the event loop
                    // so it needs to stay alive
                    if (ptls->sleep_check_state != not_sleeping)
                        jl_atomic_store(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
                    start_cycles = 0;
                    continue;
                }
            }

            // the other threads will just wait for on signal to resume
            JULIA_DEBUG_SLEEPWAKE( ptls->sleep_enter = cycleclock() );
            int8_t gc_state = jl_gc_safe_enter(ptls);
            uv_mutex_lock(&ptls->sleep_lock);
            while (may_sleep(ptls)) {
                uv_cond_wait(&ptls->wake_signal, &ptls->sleep_lock);
                // TODO: help with gc work here, if applicable
            }
            if (ptls->sleep_check_state != not_sleeping)
                jl_atomic_store(&ptls->sleep_check_state, not_sleeping); // let other threads know they don't need to wake us
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
