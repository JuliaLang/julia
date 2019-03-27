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

#define JULIA_ENABLE_PARTR

#ifdef JULIA_ENABLE_THREADING

// GC functions used
extern int jl_gc_mark_queue_obj_explicit(jl_gc_mark_cache_t *gc_cache,
                                         jl_gc_mark_sp_t *sp, jl_value_t *obj);

// multiq
// ---

/* a task heap */
typedef struct taskheap_tag {
    jl_mutex_t lock;
    jl_task_t **tasks;
    int16_t ntasks, prio;
} taskheap_t;

/* multiqueue parameters */
static const int16_t heap_d = 8;
static const int heap_c = 4;

/* size of each heap */
static const int tasks_per_heap = 8192; // TODO: this should be smaller by default, but growable!

/* the multiqueue's heaps */
static taskheap_t *heaps;
static int16_t heap_p;

/* unbias state for the RNG */
static uint64_t cong_unbias;

/* for atomic snapshot */
static uint64_t snapshot_id = -1;

/* for thread sleeping */
static const int16_t not_sleeping = 0;
static const int16_t checking_for_sleeping = 1;
static const int16_t sleeping = 2;
static int16_t sleep_state = not_sleeping;
static uv_mutex_t **all_sleep_locks;
static uv_cond_t **all_wake_signals;


/*  multiq_init()
 */
static inline void multiq_init(void)
{
    heap_p = heap_c * jl_n_threads;
    heaps = (taskheap_t *)calloc(heap_p, sizeof(taskheap_t));
    for (int16_t i = 0; i < heap_p; ++i) {
        jl_mutex_init(&heaps[i].lock);
        heaps[i].tasks = (jl_task_t **)calloc(tasks_per_heap, sizeof(jl_task_t*));
        heaps[i].ntasks = 0;
        heaps[i].prio = INT16_MAX;
    }
    unbias_cong(heap_p, &cong_unbias);
}


/*  sift_up()
 */
static inline void sift_up(taskheap_t *heap, int16_t idx)
{
    if (idx > 0) {
        int16_t parent = (idx-1)/heap_d;
        if (heap->tasks[idx]->prio < heap->tasks[parent]->prio) {
            jl_task_t *t = heap->tasks[parent];
            heap->tasks[parent] = heap->tasks[idx];
            heap->tasks[idx] = t;
            sift_up(heap, parent);
        }
    }
}


/*  sift_down()
 */
static inline void sift_down(taskheap_t *heap, int16_t idx)
{
    if (idx < heap->ntasks) {
        for (int16_t child = heap_d*idx + 1;
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


/*  multiq_insert()
 */
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
        jl_error("multiq insertion failed, increase #tasks per heap");
        return -1;
    }

    heaps[rn].tasks[heaps[rn].ntasks++] = task;
    sift_up(&heaps[rn], heaps[rn].ntasks-1);
    jl_mutex_unlock_nogc(&heaps[rn].lock);
    int16_t prio = jl_atomic_load(&heaps[rn].prio);
    if (task->prio < prio)
        jl_atomic_compare_exchange(&heaps[rn].prio, prio, task->prio);

    return 0;
}


/*  multiq_deletemin()
 */
static inline jl_task_t *multiq_deletemin(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    uint64_t rn1 = 0, rn2;
    int16_t i, prio1, prio2;
    jl_task_t *task;

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


/*  just_sleep()
 */
static void just_sleep(uv_mutex_t *lock, uv_cond_t *wakeup)
{
    pthread_mutex_lock(lock);
    if (__atomic_load_n(&sleep_check_state, __ATOMIC_SEQ_CST) == sleeping)
        pthread_cond_wait(wakeup, lock);
    else
        pthread_mutex_unlock(lock);
}


/*  snapshot_and_sleep()
 */
static void snapshot_and_sleep(pthread_mutex_t *lock, pthread_cond_t *wakeup)
{
    uint64_t snapshot_id = cong(UINT64_MAX, UINT64_MAX, &rngseed), previous = -1;
    if (!__atomic_compare_exchange_n(&snapshot_owner, &previous, snapshot_id, 0,
                                     __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)) {
        LOG_ERR(plog, "  snapshot has previous owner!\n");
        return;
    }

    int16_t i;
    for (i = 0;  i < heap_p;  ++i) {
        if (heaps[i].ntasks != 0)
            break;
    }
    if (i != heap_p) {
        LOG_INFO(plog, "  heap has tasks, snapshot aborted\n");
        return;
    }

    if (!__atomic_compare_exchange_n(&snapshot_owner, &snapshot_id, previous, 0,
                                     __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)) {
        LOG_INFO(plog, "  snapshot owner changed, snapshot aborted\n");
        return;
    }
    if (!__atomic_compare_exchange_n(&sleep_check_state, (int16_t *)&checking_for_sleeping,
                                     sleeping, 0,
                                     __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)) {
        LOG_ERR(plog, "  sleep aborted at snapshot end\n");
        return;
    }
    just_sleep(lock, wakeup);
}


/*  multiq_sleep_if_empty()
 */
void multiq_sleep_if_empty(pthread_mutex_t *lock, pthread_cond_t *wakeup)
{
    int16_t state;

sleep_start:
    state = __atomic_load_n(&sleep_check_state, __ATOMIC_SEQ_CST);
    if (state == checking_for_sleeping) {
        for (; ;) {
            cpu_pause();
            state = __atomic_load_n(&sleep_check_state, __ATOMIC_SEQ_CST);
            if (state == not_sleeping)
                break;
            else if (state == sleeping) {
                just_sleep(lock, wakeup);
                break;
            }
        }
    }
    else if (state == not_sleeping) {
        if (!__atomic_compare_exchange_n(&sleep_check_state, (int16_t *)&not_sleeping,
                                         checking_for_sleeping, 0,
                                         __ATOMIC_SEQ_CST, __ATOMIC_RELAXED))
            goto sleep_start;
        snapshot_and_sleep(lock, wakeup);
        if (!__atomic_compare_exchange_n(&sleep_check_state, (int16_t *)&sleeping,
                                         not_sleeping, 0,
                                         __ATOMIC_SEQ_CST, __ATOMIC_RELAXED))
            LOG_ERR(plog, "  sleep check state update failed\n");
    }
    else /* state == sleeping */
        just_sleep(lock, wakeup);
}


// parallel task runtime
// ---

// initialize the threading infrastructure
void jl_init_threadinginfra(void)
{
    /* initialize the synchronization trees pool and the multiqueue */
    multiq_init();

    /* initialize the sleep mechanism */
    all_sleep_locks = (uv_mutex_t **)calloc(jl_n_threads, sizeof(uv_mutex_t *));
    all_wake_signals = (uv_cond_t **)calloc(jl_n_threads, sizeof(uv_cond_t *));
    all_sleep_locks[0] = (uv_mutex_t *)calloc(1, sizeof(uv_mutex_t));
    all_wake_signals[0] = (uv_cond_t *)calloc(1, sizeof(uv_cond_t));
    uv_mutex_init(all_sleep_locks[0]);
    uv_cond_init(all_wake_signals[0]);
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

    all_sleep_locks[ptls->tid] = (uv_mutex_t *)calloc(1, sizeof(uv_mutex_t));
    all_wake_signals[ptls->tid] = (uv_cond_t *)calloc(1, sizeof(uv_cond_t));
    uv_mutex_init(all_sleep_locks[ptls->tid]);
    uv_cond_init(all_wake_signals[ptls->tid]);

    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    (void)jl_gc_unsafe_enter(ptls);
    jl_current_task->exception = jl_nothing;
    jl_finish_task(jl_current_task, jl_nothing); // noreturn
}


//  sleep_after_threshold() -- if sleep_threshold cycles have passed, sleep the thread
static void sleep_after_threshold(uint64_t *start_cycles)
{
    if (sleep_threshold) {
        if (!(*start_cycles)) {
            *start_cycles = rdtscp();
            return;
        }
        uint64_t elapsed_cycles = rdtscp() - (*start_cycles);
        if (elapsed_cycles >= sleep_threshold) {
            multiq_sleep_if_empty(sleep_lock, wake_signal);
            *start_cycles = 0;
        }
    }
}

JL_DLLEXPORT void jl_wakeup_thread(int16_t tid)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    /* ensure thread tid is awake if necessary */
    if (ptls->tid != tid && !_threadedregion && tid != -1) {
        uv_mutex_lock(all_sleep_locks[tid]);
        uv_cond_signal(all_wake_signals[tid]);
        uv_mutex_unlock(all_sleep_locks[tid]);
    }
    /* stop the event loop too, if on thread 1 and alerting thread 1 */
    if (ptls->tid == 0 && (tid == 0 || tid == -1))
        uv_stop(jl_global_event_loop());
}


// enqueue the specified task for execution
JL_DLLEXPORT void jl_enqueue_task(jl_task_t *task)
{
    multiq_insert(task, task->prio);
}


// get the next runnable task from the multiq
static jl_task_t *get_next_task(jl_value_t *getsticky)
{
    jl_task_t *task = (jl_task_t*)jl_apply(&getsticky, 1);
    if (jl_typeis(task, jl_task_type))
        return task;
    return multiq_deletemin();
}


JL_DLLEXPORT jl_task_t *jl_task_get_next(jl_value_t *getsticky)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *task;

    while (1) {
        jl_gc_safepoint();
        task = get_next_task(getsticky);
        if (task)
            return task;

        if (ptls->tid == 0) {
            if (!_threadedregion) {
                if (jl_run_once(jl_global_event_loop()) == 0) {
                    task = get_next_task(getsticky);
                    if (task)
                        return task;
#ifdef _OS_WINDOWS_
                    Sleep(INFINITE);
#else
                    pause();
#endif
                }
            }
            else {
                jl_process_events(jl_global_event_loop());
            }
        }
        else {
            int sleepnow = 0;
            if (!_threadedregion) {
                uv_mutex_lock(&sleep_lock);
                if (!_threadedregion) {
                    sleepnow = 1;
                }
                else {
                    uv_mutex_unlock(&sleep_lock);
                }
            }
            else {
                jl_cpu_pause();
            }
            if (sleepnow) {
                int8_t gc_state = jl_gc_safe_enter(ptls);
                uv_cond_wait(&sleep_alarm, &sleep_lock);
                uv_mutex_unlock(&sleep_lock);
                jl_gc_safe_leave(ptls, gc_state);
            }
        }
    }
}


void jl_gc_mark_enqueued_tasks(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp)
{
    for (int16_t i = 0; i < heap_p; ++i)
        for (int16_t j = 0; j < heaps[i].ntasks; ++j)
            jl_gc_mark_queue_obj_explicit(gc_cache, sp, (jl_value_t *)heaps[i].tasks[j]);
}

#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif
