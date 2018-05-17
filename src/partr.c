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

#include <sys/mman.h> // for mprotect

// task states
extern jl_sym_t *done_sym;
extern jl_sym_t *failed_sym;
extern jl_sym_t *runnable_sym;

// the lovely task-done-hook hack
extern jl_function_t *task_done_hook_func;

// task/stack switch functions used
extern void init_task_entry(jl_task_t *t, char *stack);

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


/*  reduce()
 */
static inline jl_value_t *reduce(arriver_t *arr, reducer_t *red, jl_callptr_t fptr,
                                 jl_method_instance_t *mfunc, jl_value_t *_rargs,
                                 jl_value_t *val, int idx)
{
    int arrived, aidx = idx + (GRAIN_K * jl_n_threads) - 1, ridx = aidx, nidx;

    uint32_t nrargs;
    jl_value_t **rargs;
    if (!jl_is_svec(_rargs)) {
        nrargs = 1;
        rargs = &_rargs;
    }
    else {
        nrargs = jl_svec_len(_rargs);
        rargs = jl_svec_data(_rargs);
    }

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
            val = jl_get_ptls_states()->exception_in_transit;
        }

        /* move up the tree */
        --ridx;
        ridx >>= 1;
        *red->tree[ridx] = val;
    }

    return val;
}


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


static void run_next(void);


// thread function: used by all except the main thread
void jl_threadfun(void *arg)
{
    jl_threadarg_t *targ = (jl_threadarg_t *)arg;

    // initialize this thread (set tid, create heap, set up root task)
    jl_init_threadtls(targ->tid);
    jl_init_stack_limits(0);
    init_started_thread();
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_init_root_task(ptls->stack_lo, ptls->stack_hi - ptls->stack_lo);

    // Assuming the functions called below don't contain unprotected GC
    // critical region. In general, the following part of this function
    // shouldn't call any managed code without calling `jl_gc_unsafe_enter`
    // first.
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(targ->barrier);

    // free the thread argument here
    free(targ);

    // set a jump context for this root task
    jl_setjmp(ptls->current_task->ctx, 0);

    /* get the highest priority task and run it */
    run_next();
}


// enqueue the specified task for execution
static void enqueue_task(jl_task_t *task)
{
    /* sticky tasks go to the thread's sticky queue */
    if (task->settings & TASK_IS_STICKY) {
        assert(task->sticky_tid != -1);
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

    uv_stop(jl_global_event_loop());
}


// parfor grains must synchronize/reduce as they end
static void sync_grains(jl_task_t *task)
{
    int was_last = 0;

    /* reduce... */
    if (task->red) {
        task->result = reduce(task->arr, task->red, task->rfptr, task->mredfunc,
                              task->rargs, task->result, task->grain_num);
        jl_gc_wb(task, task->result);

        /*  if this task is last, set the result in the parent task */
        if (task->result) {
            task->parent->red_result = task->result;
            jl_gc_wb(task->parent, task->parent->red_result);
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
            task->result = task->red_result;
            jl_gc_wb(task, task->result);
        }
    }
}


// all tasks except the root task start and exit here
void NOINLINE JL_NORETURN task_wrapper(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *task = ptls->current_task;
    task->started = 1;

    uint32_t nargs;
    jl_value_t **args;
    if (!jl_is_svec(task->args)) {
        nargs = 1;
        args = &task->args;
    }
    else {
        nargs = jl_svec_len(task->args);
        args = jl_svec_data(task->args);
    }

    jl_sym_t *new_state;
    JL_TRY {
        if (ptls->defer_signal) {
            ptls->defer_signal = 0;
            jl_sigint_safepoint(ptls);
        }
        JL_TIMING(ROOT);
        ptls->world_age = jl_world_counter;
        task->result = task->fptr(task->mfunc, args, nargs);
        jl_gc_wb(task, task->result);
        new_state = done_sym;
    }
    JL_CATCH {
        task->result = task->exception = ptls->exception_in_transit;
        jl_gc_wb(task, task->exception);
        jl_gc_wb(task, task->result);
        new_state = failed_sym;
    }

    /* grain tasks must synchronize */
    if (task->grain_num >= 0)
        sync_grains(task);

    /* add back any tasks in this one's completion queue */
    if (!(task->settings & TASK_IS_DETACHED)) {
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
    }

    JL_SIGATOMIC_BEGIN();

    task->state = new_state;

    /* clear thread state */
    ptls->in_finalizer = 0;
    ptls->in_pure_callback = 0;
    ptls->world_age = jl_world_counter;

    /* run the task-is-done hook(s) */
    if (task_done_hook_func == NULL)
        task_done_hook_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                            jl_symbol("task_done_hook"));
    if (task_done_hook_func != NULL) {
        jl_value_t *args[2] = {task_done_hook_func, (jl_value_t*)task};
        JL_TRY {
            jl_apply(args, 2);
        }
        JL_CATCH {
            jl_no_exc_handler(jl_exception_in_transit);
        }
    }

    JL_SIGATOMIC_END();

    /* next task */
    run_next();

    /* unreachable */
    gc_debug_critical_error();
    abort();
}


// get the next available task and run it
static void JL_NORETURN run_next(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *task = NULL;

    /* TODO: threads should sleep after spinning for some time */
    do {
        /* first check for sticky tasks */
        // TODO: fix this to round-robin through all sticky tasks
        JL_LOCK(&ptls->sticky_taskq->lock);
        task = ptls->sticky_taskq->head;
        if (task) {
            ptls->sticky_taskq->head = task->next;
            task->next = NULL;
        }
        JL_UNLOCK(&ptls->sticky_taskq->lock);

        /* no sticky tasks, go to the multiq */
        if (!task) {
            task = multiq_deletemin();

            if (task) {
                /* a sticky task will only come out of the multiq if it has not been run */
                if (task->settings & TASK_IS_STICKY) {
                    assert(task->sticky_tid == -1);
                    task->sticky_tid = ptls->tid;
                }
            }
        }

        if (!task) {
            // TODO: add support for allowing any thread to run the libuv event loop
            if (ptls->tid == 0)
                //TODO: can only make blocking call to libuv if we're sure there are no
                //tasks in the multiqueue, AND there's a way to interrupt the call when
                //a new task is added
                //jl_run_once(jl_global_event_loop());
                jl_process_events(jl_global_event_loop());
            else
                jl_cpu_pause();
        }
    } while (!task);

    /* run/resume the task */
    ptls->pgcstack = task->gcstack;
    ptls->world_age = task->world_age;
    task->gcstack = NULL;

    jl_task_t *last = task;
    while (last->current_module == NULL  &&  last != ptls->root_task)
        last = last->parent;
    if (last->current_module != NULL)
        ptls->current_module = last->current_module;

    ptls->current_task = task;
    task->current_tid = ptls->tid;

    jl_longjmp(task->ctx, 1);

    /* unreachable */
    gc_debug_critical_error();
    abort();
}


// specialize and compile the user function
static void setup_task_fun(jl_value_t *_args,
                           jl_method_instance_t **mfunc,
                           jl_callptr_t *fptr)
{
    uint32_t nargs;
    jl_value_t **args;
    if (!jl_is_svec(_args)) {
        nargs = 1;
        args = &_args;
    }
    else {
        nargs = jl_svec_len(_args);
        args = jl_svec_data(_args);
    }

    size_t world = jl_get_ptls_states()->world_age;
    *mfunc = jl_lookup_generic(args, nargs,
                               jl_int32hash_fast(jl_return_address()),
                               world);

    // Ignore constant return value for now.
    *fptr = jl_compile_method_internal(mfunc, world);
}


/*  jl_task_new() -- create a task for `f(arg)`

    The created task can then be spawned.
 */
JL_DLLEXPORT jl_task_t *jl_task_new(jl_value_t *_args)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    jl_task_t *task = (jl_task_t *)jl_gc_alloc(ptls, sizeof (jl_task_t), jl_task_type);
    JL_GC_PUSH1(&task);

    task->args = _args;
    task->storage = jl_nothing;
    task->state = runnable_sym;
    task->result = jl_nothing;
    task->exception = jl_nothing;
    task->backtrace = jl_nothing;
    task->logstate = jl_nothing;
    task->rargs = jl_nothing;
    task->mredfunc = NULL;
    task->cq.head = NULL;
    JL_MUTEX_INIT(&task->cq.lock);
    task->next = NULL;
    task->parent = ptls->current_task;
    task->red_result = jl_nothing;
    task->started = 0;
    arraylist_new(&task->locks, 0);
    task->rfptr = NULL;
    task->eh = NULL;
    task->gcstack = NULL;
    task->current_module = NULL;
    task->world_age = ptls->world_age;
    task->current_tid = -1;
    task->arr = NULL;
    task->red = NULL;
    task->settings = 0;
    task->sticky_tid = -1;
    task->grain_num = -1;
#ifdef ENABLE_TIMINGS
    task->timing_stack = NULL;
#endif
    task->stkbuf = NULL;

    setup_task_fun(_args, &task->mfunc, &task->fptr);
    jl_gc_wb(task, task->mfunc);

    // TODO: need stack management
    task->ssize = 128*1024;
    task->stkbuf = (void *)jl_gc_alloc_buf(ptls, task->ssize);
    jl_gc_wb_buf(task, task->stkbuf, task->ssize);

    // set up entry point for this task
    init_task_entry(task, (char *)task->stkbuf);

    JL_GC_POP();
    return task;
}


/*  jl_task_spawn() -- enqueue a task for execution

    If `sticky` is set, the task will only run on the current thread. If `detach`
    is set, the spawned task cannot be synced. Yields.
 */
JL_DLLEXPORT jl_task_t *jl_task_spawn(jl_task_t *task, int8_t sticky, int8_t detach)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    if (!task->started) {
        task->prio = ptls->tid;
        if (sticky) {
            task->settings |= TASK_IS_STICKY;
            task->sticky_tid = ptls->tid;
        }
        if (detach)
            task->settings |= TASK_IS_DETACHED;
    }
    enqueue_task(task);

    /* only yield if we're running a non-sticky task */
    if (!task->started // need a better solution to prevent yields in callbacks
            &&  ptls->current_task  &&  !(ptls->current_task->settings & TASK_IS_STICKY))
        jl_task_yield(1);

    return task;
}


/*  jl_task_new_multi() -- create multiple tasks for `f(arg)`

    Create multiple tasks, each of which invokes `f(arg, start, end)` such
    that the sum of `end-start` for all tasks is `count`. If `_rargs` is
    specified, the return values from the tasks are reduced; the result can
    be retrieved by sync'ing on the parent task which is returned. All the
    tasks can be spawned by passing the parent task to `jl_task_spawn_multi()`.
 */
JL_DLLEXPORT jl_task_t *jl_task_new_multi(jl_value_t *_args, int64_t count, jl_value_t *_rargs)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    int64_t n = GRAIN_K * jl_n_threads;
    lldiv_t each = lldiv(count, n);

    /* allocate synchronization tree(s) */
    arriver_t *arr = arriver_alloc();
    if (arr == NULL)
        return NULL;
    reducer_t *red = NULL;
    jl_method_instance_t *mredfunc = NULL;
    jl_callptr_t rfptr;
    if (_rargs != NULL) {
        red = reducer_alloc();
        if (red == NULL) {
            arriver_free(arr);
            return NULL;
        }
        setup_task_fun(_rargs, &mredfunc, &rfptr);
    }

    /* allocate (GRAIN_K * nthreads) tasks */
    jl_task_t *task = NULL, *prev = task, *t = NULL;
    int64_t start = 0, end;
    for (int64_t i = 0;  i < n;  ++i) {
        end = start + each.quot + (i < each.rem ? 1 : 0);
        if (task == NULL)
            t = task = jl_task_new(_args);
        else {
            t = (jl_task_t *)jl_gc_alloc(ptls, sizeof (jl_task_t),
                                         jl_task_type);
            memcpy(t, task, sizeof (jl_task_t));
        }
        if (t == NULL)
            return NULL;

        t->start = start;
        t->end = end;
        t->parent = task;
        t->grain_num = i;
        t->arr = arr;
        if (_rargs != NULL) {
            t->rargs = _rargs;
            t->mredfunc = mredfunc;
            t->rfptr = rfptr;
            t->red = red;
        }

        if (t != task) {
            prev->next = t;
            prev = t;
        }
        start = end;
    }

    return task;
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

    /* only yield if we're running a non-sticky task */
    if (ptls->current_task  &&  !(ptls->current_task->settings & TASK_IS_STICKY))
        jl_task_yield(1);

    return 0;
}


/*  jl_task_sync() -- get the return value of task `t`

    Returns NULL immediately if task was created detached. Otherwise,
    returns only when task `t` has completed.
 */
JL_DLLEXPORT jl_value_t *jl_task_sync(jl_task_t *task)
{
    if (task->settings & TASK_IS_DETACHED)
        return jl_nothing;

    jl_ptls_t ptls = jl_get_ptls_states();

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
            jl_task_yield(0);
        }

        /* the task finished before we could add to its CQ */
        else
            JL_UNLOCK(&task->cq.lock);
    }

    if (task->state == failed_sym)
        jl_throw(task->exception);

    return task->grain_num >= 0 && task->red ?  task->red_result : task->result;
}


/*  jl_task_yield() -- cause the invoking task to yield

    If `requeue` is set, the task is inserted into the relevant queue
    (sticky or multiqueue), otherwise it is assumed it will be re-queued
    in some other way (e.g. from another task's completion queue).
 */
JL_DLLEXPORT jl_value_t *jl_task_yield(int requeue)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *ytask = ptls->current_task;

    if (ptls->in_finalizer)
        jl_error("task switch not allowed from inside gc finalizer");
    if (ptls->in_pure_callback)
        jl_error("task switch not allowed from inside staged nor pure functions");

    sig_atomic_t defer_signal = ptls->defer_signal;
    int8_t gc_state = jl_gc_unsafe_enter(ptls);

#ifdef ENABLE_TIMINGS
    jl_timing_block_t *blk = ytask->timing_stack;
    if (blk)
        jl_timing_block_stop(blk);
#endif

    if (ytask  &&  !jl_setjmp(ytask->ctx, 0)) {
        if (ytask != ptls->root_task)
            ytask->current_tid = -1;
        //ptls->current_task = NULL;

        // backtraces don't survive task switches, see issue #12485
        ptls->bt_size = 0;

        // save state into yielding task
        ytask->gcstack = ptls->pgcstack;
        ytask->world_age = ptls->world_age;

        // If the current task is not holding any locks, free the locks list
        // so that it can be GC'd without leaking memory.
        // TODO: this will be too slow!
        arraylist_t *locks = &ytask->locks;
        if (locks->len == 0  &&  locks->items != locks->_space) {
            arraylist_free(locks);
            arraylist_new(locks, 0);
        }

        // re-enqueue the task
        if (requeue)
            enqueue_task(ytask);

        // run the next available task
        run_next();

        // unreachable
        gc_debug_critical_error();
        abort();
    }

    // TODO: add support for allowing any thread to run the event loop
    if (ptls->tid == 0)
        jl_process_events(jl_global_event_loop());

#ifdef ENABLE_TIMINGS
    assert(blk == jl_current_task->timing_stack);
    if (blk)
        jl_timing_block_start(blk);
#endif

    jl_gc_unsafe_leave(ptls, gc_state);
    sig_atomic_t other_defer_signal = ptls->defer_signal;
    if (other_defer_signal  &&  !defer_signal)
        jl_sigint_safepoint(ptls);

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
    return jl_task_yield(0);
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


// interface to other parts of the Julia runtime

/*  jl_mark_enqueued_tasks()
 */
void jl_mark_enqueued_tasks(jl_gc_mark_cache_t *gc_cache, gc_mark_sp_t *sp)
{
    for (int16_t i = 0;  i < heap_p;  ++i)
        for (int16_t j = 0;  j < heaps[i].ntasks;  ++j)
            jl_gc_mark_obj(gc_cache, sp, heaps[i].tasks[j]);
    for (int16_t i = 0;  i < jl_n_threads;  ++i) {
        jl_task_t *t = sticky_taskqs[i].head;
        while (t) {
            jl_gc_mark_obj(gc_cache, sp, t);
            t = t->next;
        }
    }
}


#endif // JULIA_ENABLE_PARTR
#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif
