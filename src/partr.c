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

// empirically, finish_task needs about 64k stack space to infer/run
// and additionally, gc-stack reserves 64k for the guard pages
#if defined(MINSIGSTKSZ) && MINSIGSTKSZ > 131072
#define MINSTKSZ MINSIGSTKSZ
#else
#define MINSTKSZ 131072
#endif

// task states and stack switching
extern jl_sym_t *done_sym;
extern jl_sym_t *failed_sym;
extern jl_sym_t *runnable_sym;
extern void jl_switchto(jl_task_t **pt);
extern char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner);

// the lovely task-done-hook hack
extern jl_function_t *task_done_hook_func;

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
    for (int16_t i = 0;  i < heap_p;  ++i) {
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

    for (i = 0;  i < heap_p;  ++i) {
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


// sync trees
// ---

/* arrival tree */
struct _arriver_t {
    int16_t index, next_avail;
    int16_t **tree;
};

/* reduction tree */
struct _reducer_t {
    int16_t index, next_avail;
    jl_value_t ***tree;
};


/* pool of arrival trees */
static arriver_t *arriverpool;
static int16_t num_arrivers, num_arriver_tree_nodes, next_arriver;

/* pool of reduction trees */
static reducer_t *reducerpool;
static int16_t num_reducers, num_reducer_tree_nodes, next_reducer;


/*  synctreepool_init()
 */
static inline void synctreepool_init(void)
{
    num_arriver_tree_nodes = (GRAIN_K * jl_n_threads) - 1;
    num_reducer_tree_nodes = (2 * GRAIN_K * jl_n_threads) - 1;

    /* num_arrivers = ((GRAIN_K * jl_n_threads) ^ ARRIVERS_P) + 1 */
    num_arrivers = GRAIN_K * jl_n_threads;
    for (int i = 1;  i < ARRIVERS_P;  ++i)
        num_arrivers = num_arrivers * num_arrivers;
    ++num_arrivers;

    num_reducers = num_arrivers * REDUCERS_FRAC;

    /* allocate */
    arriverpool = (arriver_t *)calloc(num_arrivers, sizeof (arriver_t));
    next_arriver = 0;
    for (int i = 0;  i < num_arrivers;  ++i) {
        arriverpool[i].index = i;
        arriverpool[i].next_avail = i + 1;
        arriverpool[i].tree = (int16_t **)
                jl_malloc_aligned(num_arriver_tree_nodes * sizeof (int16_t *), 64);
        for (int j = 0;  j < num_arriver_tree_nodes;  ++j)
            arriverpool[i].tree[j] = (int16_t *)jl_malloc_aligned(sizeof (int16_t), 64);
    }
    arriverpool[num_arrivers - 1].next_avail = -1;

    reducerpool = (reducer_t *)calloc(num_reducers, sizeof (reducer_t));
    next_reducer = 0;
    for (int i = 0;  i < num_reducers;  ++i) {
        reducerpool[i].index = i;
        reducerpool[i].next_avail = i + 1;
        reducerpool[i].tree = (jl_value_t ***)
                jl_malloc_aligned(num_reducer_tree_nodes * sizeof (jl_value_t **), 64);
        for (int j = 0;  j < num_reducer_tree_nodes;  ++j)
            reducerpool[i].tree[j] = (jl_value_t **)jl_malloc_aligned(sizeof (jl_value_t *), 64);
    }
    if (num_reducers > 0)
        reducerpool[num_reducers - 1].next_avail = -1;
    else
        next_reducer = -1;
}


/*  arriver_alloc()
 */
static inline arriver_t *arriver_alloc(void)
{
    int16_t candidate;
    arriver_t *arr;

    do {
        candidate = jl_atomic_load(&next_arriver);
        if (candidate == -1)
            return NULL;
        arr = &arriverpool[candidate];
    } while (!jl_atomic_bool_compare_exchange(&next_arriver,
                candidate, arr->next_avail));
    return arr;
}


/*  arriver_free()
 */
static inline void arriver_free(arriver_t *arr)
{
    for (int i = 0;  i < num_arriver_tree_nodes;  ++i)
        *arr->tree[i] = 0;

    jl_atomic_exchange_generic(&next_arriver, &arr->index, &arr->next_avail);
}


/*  reducer_alloc()
 */
static inline reducer_t *reducer_alloc(void)
{
    int16_t candidate;
    reducer_t *red;

    do {
        candidate = jl_atomic_load(&next_reducer);
        if (candidate == -1)
            return NULL;
        red = &reducerpool[candidate];
    } while (!jl_atomic_bool_compare_exchange(&next_reducer,
                     candidate, red->next_avail));
    return red;
}


/*  reducer_free()
 */
static inline void reducer_free(reducer_t *red)
{
    for (int i = 0;  i < num_reducer_tree_nodes;  ++i)
        *red->tree[i] = 0;

    jl_atomic_exchange_generic(&next_reducer, &red->index, &red->next_avail);
}


/*  last_arriver()
 */
static inline int last_arriver(arriver_t *arr, int idx)
{
    int arrived, aidx = idx + (GRAIN_K * jl_n_threads) - 1;

    while (aidx > 0) {
        --aidx;
        aidx >>= 1;
        arrived = jl_atomic_fetch_add(arr->tree[aidx], 1);
        if (!arrived) return 0;
    }

    return 1;
}


#if 0
/*  reduce()
 */
static inline jl_value_t *reduce(arriver_t *arr, reducer_t *red, jl_function_t *redfun,
                                 jl_value_t *val, int idx)
{
    int arrived, aidx = idx + (GRAIN_K * jl_n_threads) - 1, ridx = aidx, nidx;

    *red->tree[ridx] = val;
    while (aidx > 0) {
        --aidx;
        aidx >>= 1;
        arrived = jl_atomic_fetch_add(arr->tree[aidx], 1);
        if (!arrived) return NULL;

        /* neighbor has already arrived, get its value and reduce it */
        nidx = ridx & 0x1 ? ridx + 1 : ridx - 1;
        /* TODO: need to pass in val and red->tree[nidx] */
        JL_TRY {
            val = fptr(mfunc, rargs, nrargs);
        }
        JL_CATCH {
            val = jl_current_exception();
        }

        /* move up the tree */
        --ridx;
        ridx >>= 1;
        *red->tree[ridx] = val;
    }

    return val;
}
#endif

// parallel task runtime
// ---

// sticky task queues need to be visible to all threads
jl_taskq_t *sticky_taskqs;


// initialize the threading infrastructure
void jl_init_threadinginfra(void)
{
    /* initialize the synchronization trees pool and the multiqueue */
    synctreepool_init();
    multiq_init();

    /* allocate sticky task queues */
    sticky_taskqs = (jl_taskq_t *)jl_malloc_aligned(jl_n_threads * sizeof(jl_taskq_t), 64);

    /* initialize the sleep mechanism */
    uv_mutex_init(&sleep_lock);
    uv_cond_init(&sleep_alarm);
}


// initialize the thread function argument
void jl_init_threadarg(jl_threadarg_t *targ) { }


// helper for final thread initialization
static void init_started_thread(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    /* allocate this thread's sticky task queue pointer and initialize the lock */
    seed_cong(&ptls->rngseed);
    ptls->sticky_taskq = &sticky_taskqs[ptls->tid];
    ptls->sticky_taskq->head = NULL;
    JL_MUTEX_INIT(&ptls->sticky_taskq->lock);
}


// once the threads are started, perform any final initializations
void jl_init_started_threads(jl_threadarg_t **targs)
{
    // master thread final initialization
    init_started_thread();
}


static int run_next(void);


// thread function: used by all except the main thread
void jl_threadfun(void *arg)
{
    jl_threadarg_t *targ = (jl_threadarg_t *)arg;

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


// parfor grains must synchronize/reduce as they end
static void sync_grains(jl_task_t *task)
{
    int was_last = 0;

    /* TODO kp: fix */
    /* TODO kp: cascade exception(s) if any */

    /* reduce... */
    if (task->red) {
        //task->result = reduce(task->arr, task->red, task->rfptr, task->mredfunc,
        //                      task->rargs, task->result, task->grain_num);
        jl_gc_wb(task, task->result);

        /*  if this task is last, set the result in the parent task */
        if (task->result) {
            task->parent->redresult = task->result;
            jl_gc_wb(task->parent, task->parent->redresult);
            was_last = 1;
        }
    }
    /* ... or just sync */
    else {
        if (last_arriver(task->arr, task->grain_num))
            was_last = 1;
    }

    /* the last task to finish needs to finish up the loop */
    if (was_last) {
        /* a non-parent task must wake up the parent */
        if (task->grain_num > 0)
            enqueue_task(task->parent);

        /* this is the parent task which was last; it can just end */
        if (task->red)
            reducer_free(task->red);
        arriver_free(task->arr);
    }
    else {
        /* the parent task needs to wait */
        if (task->grain_num == 0) {
            jl_task_yield(0);
            task->result = task->redresult;
            jl_gc_wb(task, task->result);
        }
    }
}


// all tasks except the root task start and exit here
void NOINLINE JL_NORETURN start_task(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *task = ptls->current_task;
    task->started = 1;

    jl_sym_t *new_state;

    if (task->exception != jl_nothing) {
        ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE);
        jl_push_excstack(&task->excstack, task->exception,
                         ptls->bt_data, ptls->bt_size);
        task->result = task->exception;
        jl_gc_wb(task, task->result);
        new_state = failed_sym;
    }
    else {
        JL_TRY {
            if (ptls->defer_signal) {
                ptls->defer_signal = 0;
                jl_sigint_safepoint(ptls);
            }
            JL_TIMING(ROOT);
            ptls->world_age = jl_world_counter;
            task->result = jl_apply(&task->taskentry, 1);
            jl_gc_wb(task, task->result);
            new_state = done_sym;
        }
        JL_CATCH {
            task->result = task->exception = jl_current_exception();
            jl_gc_wb(task, task->exception);
            jl_gc_wb(task, task->result);
            new_state = failed_sym;
            goto skip_pop_exception;
        }
skip_pop_exception:;
    }

    /* grain tasks must synchronize */
    if (task->grain_num >= 0)
        sync_grains(task);

    /* add back any tasks in this one's completion queue */
    JL_LOCK(&task->cq.lock);
    jl_task_t *qtask = task->cq.head;
    task->cq.head = NULL;
    JL_UNLOCK(&task->cq.lock);
    jl_task_t *qnext;
    while (qtask) {
        qnext = qtask->next;
        qtask->next = NULL;
        enqueue_task(qtask);
        qtask = qnext;
    }

    JL_SIGATOMIC_BEGIN();

    task->state = new_state;

    if (task->copy_stack) // early free of stack
        task->stkbuf = NULL;

    /* clear thread state */
    ptls->in_finalizer = 0;
    ptls->in_pure_callback = 0;
    ptls->world_age = jl_world_counter;

    /* run the task-is-done hook(s) */
    if (task_done_hook_func == NULL)
        task_done_hook_func = (jl_function_t *)jl_get_global(jl_base_module,
                                                             jl_symbol("task_done_hook"));
    if (task_done_hook_func != NULL) {
        jl_value_t *args[2] = {task_done_hook_func, (jl_value_t *)task};
        JL_TRY {
            jl_apply(args, 2);
        }
        JL_CATCH {
            jl_no_exc_handler(jl_current_exception());
        }
    }

    JL_SIGATOMIC_END();

    /* next task */
    run_next();

    /* shouldn't reach here */
    gc_debug_critical_error();
    abort();
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

    JL_GC_POP();
    return 1;
}


// initialize a task
static void init_task(jl_task_t *task, size_t ssize)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    task->started = 0;
    task->storage = jl_nothing;
    task->state = runnable_sym;
    task->result = jl_nothing;
    task->exception = jl_nothing;
    task->backtrace = jl_nothing;
    task->logstate = jl_nothing;
    task->taskentry = NULL;
    task->redentry = NULL;
    task->cq.head = NULL;
    JL_MUTEX_INIT(&task->cq.lock);
    task->next = NULL;
    //task->parent = ptls->current_task;
    task->parent = NULL;
    task->redresult = jl_nothing;

    task->stkbuf = NULL;
    task->copy_stack = 0;
    if (ssize == 0) {
        // stack size unspecified; use default
#if defined(COPY_STACKS) && defined(ALWAYS_COPY_STACKS)
        task->copy_stack = 1;
        task->bufsz = 0;
#else
        task->bufsz = JL_STACK_SIZE;
#endif
    }
    else {
        // user requested stack of a certain size
        if (ssize < MINSTKSZ)
            ssize = MINSTKSZ;
        task->bufsz = ssize;
        task->stkbuf = jl_alloc_fiber(&task->ctx, &task->bufsz, task);
        if (task->stkbuf == NULL)
            jl_throw(jl_memory_exception);
    }
#if defined(JL_DEBUG_BUILD)
    if (!task->copy_stack)
        memset(&task->ctx, 0, sizeof(task->ctx));
#endif
#ifdef COPY_STACKS
    if (task->copy_stack)
        memcpy(&task->ctx, &ptls->base_ctx, sizeof(task->ctx));
#endif

    arraylist_new(&task->locks, 0);
    task->eh = NULL;
    task->gcstack = NULL;
    task->excstack = NULL;
    task->world_age = ptls->world_age;
    task->current_tid = -1;
    task->arr = NULL;
    task->red = NULL;
    task->sticky_tid = -1;
    task->grain_num = -1;

#ifdef ENABLE_TIMINGS
    task->timing_stack = NULL;
#endif
}


/*  jl_new_task() -- create a task for `f(arg)`

    The created task can then be spawned.
 */
JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t *_taskentry, size_t ssize)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    jl_task_t *task = (jl_task_t *)jl_gc_alloc(ptls, sizeof (jl_task_t), jl_task_type);
    init_task(task, ssize);
    task->taskentry = _taskentry;

    return task;
}


/*  jl_task_spawn() -- enqueue a task for execution

    If `sticky` is set, the task will only run on the current thread. Continues
    the current task if `unyielding` is set or in a few other cases, otherwise
    yields.
 */
JL_DLLEXPORT jl_task_t *jl_task_spawn(jl_task_t *task, jl_value_t *arg, int8_t err,
                                      int8_t unyielding, int8_t sticky)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    if (task->state != runnable_sym)
        jl_error("schedule: Task not runnable");

    if (!task->started) {
        task->prio = ptls->tid;
        if (sticky) task->sticky_tid = ptls->tid;
    }
    if (err) {
        task->exception = arg;
        jl_gc_wb(task, task->exception);
    }
    else {
        task->result = arg;
        if (arg != jl_nothing)
            jl_gc_wb(task, task->result);
    }
    enqueue_task(task);

    /* Yielding here is important -- this is what allows depth first
       scheduling. However, this breaks some assumptions made by parts of
       the Julia runtime -- I/O and channels. So, we have to allow the caller
       to disallow yielding. Also, if the task being scheduled has already
       been started, we don't yield.
     */
    if (!unyielding
            &&  !ptls->in_finalizer  // allow e.g. async printing from finalizers
            &&  !task->started)
        jl_task_yield(1);

    return task;
}


/*  jl_task_new_multi() -- create multiple tasks for `f(arg)`

    Create multiple tasks, each of which invokes `f(arg, start, end)` such
    that the sum of `end-start` for all tasks is `count`. If `_redentry` is
    specified, the return values from the tasks are reduced; the result can
    be retrieved by sync'ing on the parent task which is returned. All the
    tasks can be spawned by passing the parent task to `jl_task_spawn_multi()`.
 */
JL_DLLEXPORT jl_task_t *jl_task_new_multi(jl_function_t *_taskentry, size_t ssize,
                                          int64_t count,
                                          jl_function_t *_redentry)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    int64_t n = GRAIN_K * jl_n_threads;
    lldiv_t each = lldiv(count, n);

    /* allocate synchronization tree(s) */
    arriver_t *arr = arriver_alloc();
    if (arr == NULL)
        return NULL;
    reducer_t *red = NULL;
    if (_redentry != NULL) {
        red = reducer_alloc();
        if (red == NULL) {
            arriver_free(arr);
            return NULL;
        }
    }

    /* allocate (GRAIN_K * nthreads) tasks */
    int64_t start = 0, end = start + each.quot + (each.rem ? 1 : 0);
    jl_task_t *parent = (jl_task_t *)jl_gc_alloc(ptls, sizeof (jl_task_t), jl_task_type);
    JL_GC_PUSH1(&parent);
    init_task(parent, ssize);
    parent->taskentry = _taskentry;
    parent->redentry = _redentry;
    parent->start = start;
    parent->end = end;
    parent->grain_num = 0;
    parent->arr = arr;
    parent->red = red;

    jl_task_t *prev = parent, *task = NULL;
    start = end;
    for (int64_t i = 1;  i < n;  ++i) {
        end = start + each.quot + (i < each.rem ? 1 : 0);

        task = (jl_task_t *)jl_gc_alloc(ptls, sizeof (jl_task_t), jl_task_type);
        prev->next = task;
        jl_gc_wb(prev, prev->next);
        init_task(task, ssize);
        task->parent = parent;
        task->taskentry = _taskentry;
        task->redentry = _redentry;
        task->start = start;
        task->end = end;
        task->grain_num = i;
        task->arr = arr;
        task->red = red;

        prev = task;
        start = end;
    }

    JL_GC_POP();
    return parent;
}


/*  jl_task_spawn_multi() -- spawn multiple tasks

    Spawns multiple tasks that were previously created with `jl_task_new_multi()`.
    Yields.
 */
JL_DLLEXPORT int jl_task_spawn_multi(jl_task_t *task)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    /* enqueue (GRAIN_K * nthreads) tasks */
    jl_task_t *t = task;
    for (int64_t i = 0;  i < GRAIN_K * jl_n_threads;  ++i) {
        if (!t) // TODO: this should never happen
            return -1;
        if (multiq_insert(t, ptls->tid) != 0) // TODO: raise an error?
            return -2;
        t = t->next;
    }

    /* yield to allow depth-first scheduling */
    jl_task_yield(1);

    return 0;
}


static void taskq_delete(jl_task_t **pnext, jl_task_t *tgt)
{
    jl_task_t *pt = *pnext;
    while (pt) {
        if (pt == tgt) {
            *pnext = pt->next;
            break;
        }
        pnext = &pt->next;
        pt = *pnext;
    }
    tgt->next = NULL;
}


/*  jl_task_sync() -- get the return value of task `t`

    Returns only when task `t` has completed.
 */
JL_DLLEXPORT jl_value_t *jl_task_sync(jl_task_t *task)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    if (task == ptls->current_task)
        jl_error("cannot sync on self");

    /* if the target task has not finished, add the current task to its
       completion queue; the thread that runs the target task will add
       this task back to the ready queue
     */
    if (task->state != done_sym  &&  task->state != failed_sym) {
        // TODO: problem if a grain task does a sync?
        ptls->current_task->next = NULL;
        JL_LOCK(&task->cq.lock);

        /* ensure the task didn't finish before we got the lock */
        if (task->state != done_sym  &&  task->state != failed_sym) {
            /* add the current task to the CQ */
            if (!task->cq.head) {
                task->cq.head = ptls->current_task;
                jl_gc_wb(task, task->cq.head);
            }
            else {
                jl_task_t *pt = task->cq.head;
                while (pt->next)
                    pt = pt->next;
                pt->next = ptls->current_task;
                jl_gc_wb(pt, pt->next);
            }

            JL_UNLOCK(&task->cq.lock);
            JL_TRY {
                jl_task_yield(0);
            }
            JL_CATCH {
                taskq_delete(&task->cq.head, ptls->current_task);
                jl_rethrow();
            }
        }

        /* the task finished before we could add to its CQ */
        else
            JL_UNLOCK(&task->cq.lock);
    }

    if (task->state == failed_sym)
        jl_throw(task->exception);

    return task->grain_num >= 0 && task->red ?  task->redresult : task->result;
}


/*  jl_task_yield() -- cause the invoking task to yield

    If `requeue` is set, the task is inserted into the relevant queue
    (sticky or multiqueue), otherwise it is assumed it will be re-queued
    in some other way (e.g. from another task's completion queue).
 */
JL_DLLEXPORT jl_value_t *jl_task_yield(int requeue)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    if (ptls->in_finalizer)
        jl_error("task switch not allowed from inside gc finalizer");
    if (ptls->in_pure_callback)
        jl_error("task switch not allowed from inside staged nor pure functions");

    if (requeue)
        enqueue_task(ptls->current_task);

    // run the next available task
    run_next();

    // yielding task (eventually) continues
    jl_value_t *exc = ptls->current_task->exception;
    if (exc != jl_nothing) {
        ptls->current_task->exception = jl_nothing;
        jl_throw(exc);
    }

    jl_value_t *res = ptls->current_task->result;
    ptls->current_task->result = jl_nothing;
    return res;
}


/*  jl_condition_new() -- create a new Condition
 */
JL_DLLEXPORT jl_condition_t *jl_condition_new(void)
{
    jl_condition_t *cond = (jl_condition_t *)
            jl_new_struct_uninit(jl_condition_type);
    cond->head = NULL;
    JL_GC_PUSH1(&cond);
    JL_MUTEX_INIT(&cond->lock);
    JL_GC_POP();

    return cond;
}


/*  jl_task_wait() -- deschedules the task until the specified condition is
        triggered
 */
JL_DLLEXPORT jl_value_t *jl_task_wait(jl_condition_t *c)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_LOCK(&c->lock);
    if (!c->head) {
        c->head = ptls->current_task;
        jl_gc_wb(c, c->head);
    }
    else {
        jl_task_t *pt = c->head;
        while (pt->next)
            pt = pt->next;
        pt->next = ptls->current_task;
        jl_gc_wb(pt, pt->next);
    }
    JL_UNLOCK(&c->lock);
    jl_value_t *val = NULL;
    JL_TRY {
        val = jl_task_yield(0);
    }
    JL_CATCH {
        taskq_delete(&c->head, ptls->current_task);
        jl_rethrow();
    }
    return val;
}


/*  jl_task_notify() -- triggers the specified condition, causing all tasks
        waiting on it to become schedulable
 */
JL_DLLEXPORT void jl_task_notify(jl_condition_t *c, jl_value_t *arg, int8_t all, int8_t err)
{
    JL_LOCK(&c->lock);
    jl_task_t *qtask = c->head;
    if (all)
        c->head = NULL;
    else {
        if (c->head) {
            c->head = c->head->next;
            qtask->next = NULL;
        }
    }
    JL_UNLOCK(&c->lock);

    jl_task_t *qnext;
    while (qtask) {
        qnext = qtask->next;
        qtask->next = NULL;
        if (err) {
            qtask->exception = arg;
            jl_gc_wb(qtask, qtask->exception);
        }
        else {
            qtask->result = arg;
            jl_gc_wb(qtask, qtask->result);
        }
        enqueue_task(qtask);
        qtask = qnext;
    }
}


JL_DLLEXPORT int jl_condition_isempty(jl_condition_t *c)
{
    return c->head ? 0 : 1;
}


void jl_gc_mark_enqueued_tasks(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp)
{
    for (int16_t i = 0;  i < heap_p;  ++i)
        for (int16_t j = 0;  j < heaps[i].ntasks;  ++j)
            jl_gc_mark_queue_obj_explicit(gc_cache, sp, (jl_value_t *)heaps[i].tasks[j]);
    for (int16_t i = 0;  i < jl_n_threads;  ++i) {
        jl_task_t *t = sticky_taskqs[i].head;
        while (t) {
            jl_gc_mark_queue_obj_explicit(gc_cache, sp, (jl_value_t *)t);
            t = t->next;
        }
    }
}

#endif // JULIA_ENABLE_PARTR
#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif
