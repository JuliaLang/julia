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

static int16_t sleep_check_state; // status of the multi-queue. possible values:

// no thread should be sleeping--there might be work in the multi-queue.
static const int16_t not_sleeping = 0;

// it is acceptable for a thread to be sleeping if its sticky queue is empty.
// sleep_check_state == sleeping + 1 + tid means thread tid is checking the multi-queue
// to see if it is safe to transition to sleeping.
static const int16_t sleeping = 1;


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
                    &&  heap->tasks[child]->prio < heap->tasks[idx]->prio) {
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
    if (jl_atomic_load_acquire(&task->tid) != ptls->tid) {
        if (jl_atomic_compare_exchange(&task->tid, -1, ptls->tid) != -1) {
            jl_mutex_unlock_nogc(&heaps[rn1].lock);
            goto retry;
        }
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


static int sleep_check_now(int16_t tid)
{
    while (1) {
        int16_t state = jl_atomic_load(&sleep_check_state);
        if (state > sleeping) {
            // if some thread is already checking, the decision of that thread
            // is correct for us also
            do {
                state = jl_atomic_load(&sleep_check_state);
            } while (state > sleeping);
            if (state == not_sleeping)
                return 0;
        }
        else if (state == not_sleeping) {
            int16_t checking_for_sleeping = sleeping + 1 + tid;
            // transition from sleeping ==> checking
            if (jl_atomic_bool_compare_exchange(&sleep_check_state, not_sleeping,
                                                checking_for_sleeping)) {
                if (multiq_check_empty()) {
                    // transition from checking ==> sleeping
                    if (jl_atomic_bool_compare_exchange(&sleep_check_state, checking_for_sleeping,
                                                        sleeping))
                        return 1;
                }
                else {
                    // transition from checking ==> not_sleeping
                    jl_atomic_store(&sleep_check_state, not_sleeping);
                    return 0;
                }
            }
            continue;
        }
        assert(state == sleeping);
        return 1;
    }
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
    sleep_check_state = not_sleeping;
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


//  sleep_check_after_threshold() -- if sleep_threshold ns have passed, return 1
static int sleep_check_after_threshold(uint64_t *start_cycles)
{
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
    int16_t state = jl_atomic_exchange(&other->sleep_check_state, not_sleeping);
    if (state == sleeping) {
        uv_mutex_lock(&other->sleep_lock);
        uv_cond_signal(&other->wake_signal);
        uv_mutex_unlock(&other->sleep_lock);
    }
}


/* ensure thread tid is awake if necessary */
JL_DLLEXPORT void jl_wakeup_thread(int16_t tid)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    int16_t self = ptls->tid;
    unsigned long system_self = jl_all_tls_states[self]->system_id;
    int16_t uvlock = jl_atomic_load_acquire(&jl_uv_mutex.owner);
    if (tid == self || tid == -1) {
        // we're already awake, but make sure we'll exit uv_run
        jl_atomic_store(&ptls->sleep_check_state, not_sleeping);
        if (uvlock == system_self)
            uv_stop(jl_global_event_loop());
    }
    else {
        // something added to the sticky-queue: notify that thread
        wake_thread(tid);
        // check if we need to notify uv_run too
        if (uvlock != system_self)
            jl_wake_libuv();
    }
    if (tid == -1) {
        // check if the other threads might be sleeping
        if (jl_atomic_load_acquire(&sleep_check_state) != not_sleeping) {
            // something added to the multi-queue: notify all threads
            // in the future, we might want to instead wake some fraction of threads,
            // and let each of those wake additional threads if they find work
            int16_t state = jl_atomic_exchange(&sleep_check_state, not_sleeping);
            if (state == sleeping) {
                for (tid = 0; tid < jl_n_threads; tid++)
                    if (tid != self)
                        wake_thread(tid);
                // check if we need to notify uv_run too
                if (uvlock != system_self)
                    jl_wake_libuv();
            }
        }
    }
}


JL_DLLEXPORT void jl_set_task_tid(jl_task_t *task, int tid) JL_NOTSAFEPOINT
{
    // Try to acquire the lock on this task.
    // If this fails, we'll check for that error later (in jl_switchto).
    if (jl_atomic_load_acquire(&task->tid) != tid) {
        jl_atomic_compare_exchange(&task->tid, -1, tid);
    }
}

// get the next runnable task from the multiq
static jl_task_t *get_next_task(jl_value_t *getsticky)
{
    jl_gc_safepoint();
    jl_task_t *task = (jl_task_t*)jl_apply(&getsticky, 1);
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
    return jl_atomic_load(&sleep_check_state) == sleeping && jl_atomic_load(&ptls->sleep_check_state) == sleeping;
}

extern volatile unsigned _threadedregion;

JL_DLLEXPORT jl_task_t *jl_task_get_next(jl_value_t *getsticky)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    uint64_t start_cycles = 0;
    jl_task_t *task;

    while (1) {
        task = get_next_task(getsticky);
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
            if (!sleep_check_now(ptls->tid))
                continue;
            jl_atomic_store(&ptls->sleep_check_state, sleeping); // acquire sleep-check lock
            task = get_next_task(getsticky);
            if (task)
                return task;
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
                JL_UV_LOCK();
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
                        active = uv_run(loop, UV_RUN_ONCE);
                    }
                    JL_UV_UNLOCK();
                    // optimization: check again first if we may have work to do
                    if (!may_sleep(ptls)) {
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
                }
                if (!_threadedregion && active && ptls->tid == 0) {
                    // thread 0 is the only thread permitted to run the event loop
                    // so it needs to stay alive
                    start_cycles = 0;
                    continue;
                }
            }

            // the other threads will just wait for on signal to resume
            int8_t gc_state = jl_gc_safe_enter(ptls);
            uv_mutex_lock(&ptls->sleep_lock);
            while (may_sleep(ptls)) {
                uv_cond_wait(&ptls->wake_signal, &ptls->sleep_lock);
            }
            uv_mutex_unlock(&ptls->sleep_lock);
            jl_gc_safe_leave(ptls, gc_state); // contains jl_gc_safepoint
            start_cycles = 0;
        }
        else {
#ifndef JL_HAVE_ASYNCIFY
            // maybe check the kernel for new messages too
            if (jl_atomic_load(&jl_uv_n_waiters) == 0)
                jl_process_events(jl_global_event_loop());
#else
            // Yield back to browser event loop
            return ptls->root_task;
#endif
        }
    }
}

#ifdef __cplusplus
}
#endif
