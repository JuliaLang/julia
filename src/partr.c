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

#ifdef JULIA_ENABLE_THREADING
#ifdef JULIA_ENABLE_PARTR

// GC functions used
extern int jl_gc_mark_queue_obj_explicit(jl_gc_mark_cache_t *gc_cache,
                                         jl_gc_mark_sp_t *sp, jl_value_t *obj);

// thread sleep threshold
extern uint64_t jl_thread_sleep_threshold;

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

/* for thread sleeping */
static uv_mutex_t sleep_lock;
static uv_cond_t  sleep_alarm;


/*  multiq_init()
 */
static inline void multiq_init(void)
{
    heap_p = heap_c * jl_n_threads;
    heaps = (taskheap_t *)calloc(heap_p, sizeof(taskheap_t));
    for (int16_t i = 0; i < heap_p; ++i) {
        jl_mutex_init(&heaps[i].lock);
        heaps[i].tasks = (jl_task_t **)calloc(tasks_per_heap, sizeof(jl_task_t *));
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



// parallel task runtime
// ---

// sticky task queues need to be visible to all threads
jl_taskq_t *sticky_taskqs;


// initialize the threading infrastructure
void jl_init_threadinginfra(void)
{
    /* initialize the synchronization trees pool and the multiqueue */
    multiq_init();

    /* allocate sticky task queues */
    sticky_taskqs = (jl_taskq_t *)jl_malloc_aligned(jl_n_threads * sizeof(jl_taskq_t), 64);

    /* initialize the sleep mechanism */
    uv_mutex_init(&sleep_lock);
    uv_cond_init(&sleep_alarm);

    // master thread final initialization
    init_started_thread();
}


// helper for final thread initialization
static void init_started_thread(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    /* allocate this thread's sticky task queue pointer and initialize the lock */
    ptls->sticky_taskq = &sticky_taskqs[ptls->tid];
    ptls->sticky_taskq->head = NULL;
    JL_MUTEX_INIT(&ptls->sticky_taskq->lock);
}


static int run_next(void);


// thread function: used by all except the main thread
void jl_threadfun(void *arg)
{
    jl_threadarg_t *targ = (jl_threadarg_t*)arg;

    // initialize this thread (set tid, create heap, set up root task)
    jl_init_threadtls(targ->tid);
    void *stack_lo, *stack_hi;
    jl_init_stack_limits(0, &stack_lo, &stack_hi);
    init_started_thread();
    jl_init_root_task(stack_lo, stack_hi);

    // Assuming the functions called below don't contain unprotected GC
    // critical region. In general, the following part of this function
    // shouldn't call any managed code without calling `jl_gc_unsafe_enter`
    // first.
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    jl_current_task->state = done_sym;
    run_next();

    // shouldn't get here
    gc_debug_critical_error();
    abort();
}


// enqueue the specified task for execution
static void enqueue_task(jl_task_t *task)
{
    /* sticky tasks go to the thread's sticky queue */
    if (task->sticky_tid != -1) {
        jl_taskq_t *taskq = &sticky_taskqs[task->sticky_tid];
        JL_LOCK(&taskq->lock);
        if (!taskq->head)
            taskq->head = task;
        else {
            jl_task_t *pt = taskq->head;
            while (pt->next)
                pt = pt->next;
            pt->next = task;
        }
        JL_UNLOCK(&taskq->lock);
    }

    /* all others go back into the multiq */
    else
        multiq_insert(task, task->prio);

    /* stop the event loop */
    uv_stop(jl_global_event_loop());

    /* wake up threads */
    if (jl_thread_sleep_threshold) {
        uv_mutex_lock(&sleep_lock);
        uv_cond_broadcast(&sleep_alarm);
        uv_mutex_unlock(&sleep_lock);
    }
}


// get the next runnable task
static jl_task_t *get_next_task(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *task = NULL;
    JL_GC_PUSH1(&task);

    /* first check for sticky tasks */
    JL_LOCK(&ptls->sticky_taskq->lock);
    task = ptls->sticky_taskq->head;
    if (task) {
        ptls->sticky_taskq->head = task->next;
        task->next = NULL;
    }
    JL_UNLOCK(&ptls->sticky_taskq->lock);

    /* no sticky tasks, go to the multiq */
    if (!task) task = multiq_deletemin();

    JL_GC_POP();
    return task;
}


// run the next available task
// TODO: deal with the case where another thread gets the task from which a thread is
// still trying to switch away
static int run_next(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *task = NULL;
    JL_GC_PUSH1(&task);

    uint64_t spin_ns, spin_start = 0;
    while (!task) {
        if (jl_thread_sleep_threshold) {
            if (spin_start == 0) {
                spin_start = uv_hrtime();
                continue;
            }
        }

        task = get_next_task();

        if (!task) {
            if (ptls->tid == 0)
                jl_process_events(jl_global_event_loop());
            else
                jl_cpu_pause();

            if (jl_thread_sleep_threshold) {
                spin_ns = uv_hrtime() - spin_start;
                if (spin_ns > jl_thread_sleep_threshold) {
                    uv_mutex_lock(&sleep_lock);
                    task = get_next_task();
                    if (!task) {
                        // thread 0 makes a blocking call to the event loop
                        if (ptls->tid == 0) {
                            uv_mutex_unlock(&sleep_lock);
                            jl_run_once(jl_global_event_loop());
                        }
                        // other threads just sleep
                        else {
                            uv_cond_wait(&sleep_alarm, &sleep_lock);
                            uv_mutex_unlock(&sleep_lock);
                        }
                    }
                    else uv_mutex_unlock(&sleep_lock);
                    spin_start = 0;
                }
            }
        }
    }

    jl_switchto(&task);
    if (ptls->tid == 0)
        jl_process_events(jl_global_event_loop());

    JL_GC_POP();
    return 1;
}


void jl_gc_mark_enqueued_tasks(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp)
{
    for (int16_t i = 0; i < heap_p; ++i)
        for (int16_t j = 0; j < heaps[i].ntasks; ++j)
            jl_gc_mark_queue_obj_explicit(gc_cache, sp, (jl_value_t *)heaps[i].tasks[j]);
    for (int16_t i = 0; i < jl_n_threads; ++i) {
        jl_task_t *t = sticky_taskqs[i].head;
        while (t) {
            jl_gc_mark_queue_obj_explicit(gc_cache, sp, (jl_value_t *)t);
            t = t->next;
        }
    }
}

#else
void jl_init_threadinginfra(void) { }
void jl_threadfun(void *arg) { abort(); }
void jl_task_done_hook_partr(jl_task_t *task) { }
#endif
#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif
