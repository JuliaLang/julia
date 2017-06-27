// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "options.h"
#include "threading.h"

#ifdef JULIA_ENABLE_THREADING
#ifdef JULIA_ENABLE_FORKJOIN_TI

// for the barrier
typedef struct {
    int sense;
} ti_thread_sense_t;

// thread group
typedef struct {
    int16_t *tid_map, num_threads, added_threads;
    uint8_t num_sockets, num_cores, num_threads_per_core;

    // fork/join/barrier
    uint8_t group_sense; // Written only by master thread
    ti_thread_sense_t **thread_sense;
    void              *envelope;

    // to let threads sleep
    uv_mutex_t alarm_lock;
    uv_cond_t  alarm;
    uint64_t   sleep_threshold;
} ti_threadgroup_t;

// thread state
enum {
    TI_THREAD_INIT,
    TI_THREAD_WORK
};

// passed to thread function
typedef struct {
    int16_t volatile state;
    ti_threadgroup_t *tg;
} ti_threadarg_t;

// work command to thread function
typedef struct {
    jl_method_instance_t *mfunc;
    jl_callptr_t fptr;
    jl_value_t **args;
    uint32_t nargs;
    jl_value_t *ret;
    jl_module_t *current_module;
    size_t world_age;
} ti_threadwork_t;

// for broadcasting work to threads
static ti_threadwork_t threadwork;

// only one thread group for now
static ti_threadgroup_t *tgworld;


// threadgroup functions
// ---
static int ti_threadgroup_create(uint8_t num_sockets, uint8_t num_cores,
                                 uint8_t num_threads_per_core,
                                 ti_threadgroup_t **newtg)
{
    int i;
    ti_threadgroup_t *tg;
    int num_threads = num_sockets * num_cores * num_threads_per_core;
    char *cp;

    tg = (ti_threadgroup_t*)jl_malloc_aligned(sizeof(ti_threadgroup_t), 64);
    tg->tid_map = (int16_t*)jl_malloc_aligned(num_threads * sizeof(int16_t), 64);
    for (i = 0;  i < num_threads;  ++i)
        tg->tid_map[i] = -1;
    tg->num_sockets = num_sockets;
    tg->num_cores = num_cores;
    tg->num_threads_per_core = num_threads_per_core;
    tg->num_threads = num_threads;
    tg->added_threads = 0;
    tg->thread_sense = (ti_thread_sense_t**)
        jl_malloc_aligned(num_threads * sizeof(ti_thread_sense_t*), 64);
    for (i = 0;  i < num_threads;  i++)
        tg->thread_sense[i] = NULL;
    jl_atomic_store_release(&tg->group_sense, 0);

    uv_mutex_init(&tg->alarm_lock);
    uv_cond_init(&tg->alarm);

    tg->sleep_threshold = DEFAULT_THREAD_SLEEP_THRESHOLD;
    cp = getenv(THREAD_SLEEP_THRESHOLD_NAME);
    if (cp) {
        if (!strncasecmp(cp, "infinite", 8))
            tg->sleep_threshold = 0;
        else
            tg->sleep_threshold = (uint64_t)strtol(cp, NULL, 10);
    }

    *newtg = tg;
    return 0;
}

static int ti_threadgroup_addthread(ti_threadgroup_t *tg, int16_t ext_tid,
                                    int16_t *tgtid)
{
    if (ext_tid < 0 || ext_tid >= tg->num_threads)
        return -1;
    if (tg->tid_map[ext_tid] != -1)
        return -2;
    if (tg->added_threads == tg->num_threads)
        return -3;

    tg->tid_map[ext_tid] = tg->added_threads++;
    if (tgtid) *tgtid = tg->tid_map[ext_tid];

    return 0;
}

static int ti_threadgroup_initthread(ti_threadgroup_t *tg, int16_t ext_tid)
{
    ti_thread_sense_t *ts;

    if (ext_tid < 0 || ext_tid >= tg->num_threads)
        return -1;
    if (tg->thread_sense[tg->tid_map[ext_tid]] != NULL)
        return -2;
    if (tg->num_threads == 0)
        return -3;

    ts = (ti_thread_sense_t*)jl_malloc_aligned(sizeof(ti_thread_sense_t), 64);
    ts->sense = 1;
    tg->thread_sense[tg->tid_map[ext_tid]] = ts;

    return 0;
}

static int ti_threadgroup_fork(ti_threadgroup_t *tg, int16_t ext_tid, void **bcast_val, int init)
{
    uint8_t *group_sense = &tg->group_sense;
    int16_t tid = tg->tid_map[ext_tid];
    int thread_sense = tg->thread_sense[tid]->sense;
    if (tid == 0) {
        tg->envelope = bcast_val ? *bcast_val : NULL;
        // synchronize `tg->envelope` and `tg->group_sense`
        jl_atomic_store_release(group_sense, thread_sense);

        // if it's possible that threads are sleeping, signal them
        if (tg->sleep_threshold) {
            uv_mutex_lock(&tg->alarm_lock);
            uv_cond_broadcast(&tg->alarm);
            uv_mutex_unlock(&tg->alarm_lock);
        }
    }
    else {
        // spin up to threshold ns (count sheep), then sleep
        uint64_t spin_ns;
        uint64_t spin_start = 0;
        // synchronize `tg->envelope` and `tg->group_sense`
        while (jl_atomic_load_acquire(group_sense) != thread_sense) {
            if (tg->sleep_threshold) {
                if (!spin_start) {
                    // Lazily initialize spin_start since uv_hrtime is expensive
                    spin_start = uv_hrtime();
                    continue;
                }
                spin_ns = uv_hrtime() - spin_start;
                // In case uv_hrtime is not monotonic, we'll sleep earlier
                if (init || spin_ns >= tg->sleep_threshold) {
                    uv_mutex_lock(&tg->alarm_lock);
                    if (jl_atomic_load_acquire(group_sense) != thread_sense) {
                        uv_cond_wait(&tg->alarm, &tg->alarm_lock);
                    }
                    uv_mutex_unlock(&tg->alarm_lock);
                    spin_start = 0;
                    init = 0;
                    continue;
                }
            }
            jl_cpu_pause();
        }
        if (bcast_val)
            *bcast_val = tg->envelope;
    }

    return 0;
}

static int ti_threadgroup_join(ti_threadgroup_t *tg, int16_t ext_tid)
{
    int *p_thread_sense = &tg->thread_sense[tg->tid_map[ext_tid]]->sense;
    jl_atomic_store_release(p_thread_sense, !*p_thread_sense);
    if (tg->tid_map[ext_tid] == 0) {
        jl_ptls_t ptls = jl_get_ptls_states();
        int8_t group_sense = tg->group_sense;
        for (int i = 1; i < tg->num_threads; ++i) {
            while (jl_atomic_load_acquire(&tg->thread_sense[i]->sense) == group_sense) {
                jl_gc_safepoint_(ptls);
                jl_cpu_pause();
            }
        }
    }

    return 0;
}


// threading interface
// ---
void jl_init_threadinginfra(void) { }

void jl_init_threadarg(jl_threadarg_t *targ)
{
    ti_threadarg_t *tiarg = (ti_threadarg_t *)malloc(sizeof (ti_threadarg_t));
    tiarg->state = TI_THREAD_INIT;
    targ->arg = (void *)tiarg;
}

void jl_init_started_threads(jl_threadarg_t **targs)
{
    // set up the world thread group
    ti_threadgroup_create(1, jl_n_threads, 1, &tgworld);
    for (int i = 0;  i < jl_n_threads;  ++i)
        ti_threadgroup_addthread(tgworld, i, NULL);

    jl_ptls_t ptls = jl_get_ptls_states();
    ti_threadgroup_initthread(tgworld, ptls->tid);

    // give the threads the world thread group; they will block waiting for fork
    for (int i = 0;  i < jl_n_threads - 1;  ++i) {
        ti_threadarg_t *tiarg = (ti_threadarg_t *)targs[i]->arg;
        tiarg->tg = tgworld;
        jl_atomic_store_release(&tiarg->state, TI_THREAD_WORK);
    }
}

// thread function: used by all except the main thread
void jl_threadfun(void *arg)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_threadarg_t *targ = (jl_threadarg_t *)arg;
    ti_threadarg_t *tiarg = (ti_threadarg_t *)targ->arg;
    ti_threadgroup_t *tg;
    ti_threadwork_t *work;

    // initialize this thread (set tid, create heap, etc.)
    jl_init_threadtls(targ->tid);
    jl_init_stack_limits(0);

    // set up tasking
    jl_init_root_task(ptls->stack_lo, ptls->stack_hi - ptls->stack_lo);
#ifdef COPY_STACKS
    jl_set_base_ctx((char*)&arg);
#endif

    // wait for a thread group
    while (jl_atomic_load_acquire(&tiarg->state) == TI_THREAD_INIT)
        jl_cpu_pause();

    // Assuming the functions called below don't contain unprotected GC
    // critical region. In general, the following part of this function
    // shouldn't call any managed code without calling `jl_gc_unsafe_enter`
    // first.
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(targ->barrier);

    // initialize this thread in the thread group
    tg = tiarg->tg;
    ti_threadgroup_initthread(tg, ptls->tid);

    // free the thread argument here
    free(tiarg);
    free(targ);

    int init = 1;

    // work loop
    for (; ;) {
        ti_threadgroup_fork(tg, ptls->tid, (void **)&work, init);
        init = 0;

        if (work) {
            // TODO: before we support getting return value from
            //       the work, and after we have proper GC transition
            //       support in the codegen and runtime we don't need to
            //       enter GC unsafe region when starting the work.
            int8_t gc_state = jl_gc_unsafe_enter(ptls);
            // This is probably always NULL for now
            jl_module_t *last_m = ptls->current_module;
            size_t last_age = ptls->world_age;
            JL_GC_PUSH1(&last_m);
            ptls->current_module = work->current_module;
            ptls->world_age = work->world_age;
            jl_thread_run_fun(work->fptr, work->mfunc, work->args, work->nargs);
            ptls->current_module = last_m;
            ptls->world_age = last_age;
            JL_GC_POP();
            jl_gc_unsafe_leave(ptls, gc_state);
        }

        ti_threadgroup_join(tg, ptls->tid);
    }
}

// interface to user code: specialize and compile the user thread function
// and run it in all threads
JL_DLLEXPORT jl_value_t *jl_threading_run(jl_value_t *_args)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // GC safe
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

    int8_t gc_state = jl_gc_unsafe_enter(ptls);

    size_t world = jl_get_ptls_states()->world_age;

    threadwork.mfunc = jl_lookup_generic(args, nargs,
                                         jl_int32hash_fast(jl_return_address()), ptls->world_age);
    // Ignore constant return value for now.
    threadwork.fptr = jl_compile_method_internal(&threadwork.mfunc, world);
    if (threadwork.fptr == jl_fptr_const_return)
        return jl_nothing;
    threadwork.args = args;
    threadwork.nargs = nargs;
    threadwork.ret = jl_nothing;
    threadwork.current_module = ptls->current_module;
    threadwork.world_age = world;

    // fork the world thread group
    ti_threadwork_t *tw = &threadwork;
    ti_threadgroup_fork(tgworld, ptls->tid, (void **)&tw, 0);

    // this thread must do work too
    tw->ret = jl_thread_run_fun(threadwork.fptr, threadwork.mfunc, args, nargs);

    // wait for completion
    ti_threadgroup_join(tgworld, ptls->tid);

    jl_gc_unsafe_leave(ptls, gc_state);

    return tw->ret;
}

#endif // JULIA_ENABLE_FORKJOIN_TI
#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif
