// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  threading infrastructure
  . thread and threadgroup creation
  . thread function
  . invoke Julia function from multiple threads

TODO:
  . fix interface to properly support thread groups
  . add queue per thread for tasks
  . add reduction; reduce values returned from thread function
  . make code generation thread-safe and remove the lock
*/


#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
#include <unistd.h>
#include <sched.h>
#else
#define sleep(x) Sleep(1000*x)
#endif

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "ia_misc.h"
#include "threadgroup.h"
#include "threading.h"

#if !defined(_CPU_X86_64_) && !defined(_CPU_X86_) && defined(__linux__)

static int _get_perf_fd(void)
{
    static int fd = -1;
    if (fd < 0) {
        static struct perf_event_attr attr;
        attr.type = PERF_TYPE_HARDWARE;
        attr.config = PERF_COUNT_HW_CPU_CYCLES;
        fd = syscall(__NR_perf_event_open, &attr, 0, -1, -1, 0);
    }
    return fd;
}

__attribute__((destructor)) static void
_close_perf_fd(void)
{
    close(_get_perf_fd());
}

long long
rdtsc(void)
{
    long long result = 0;
    if (read(_get_perf_fd(), &result, sizeof(result)) < sizeof(result))
        return 0;
    return result;
}
#endif

#ifdef JULIA_ENABLE_THREADING
// fallback provided for embedding
static JL_CONST_FUNC jl_tls_states_t *jl_get_ptls_states_fallback(void)
{
#  if !defined(_COMPILER_MICROSOFT_)
    static __thread jl_tls_states_t tls_states;
#  else
    static __declspec(thread) jl_tls_states_t tls_states;
#  endif
    return &tls_states;
}
static jl_tls_states_t *jl_get_ptls_states_init(void);
static jl_get_ptls_states_func jl_tls_states_cb = jl_get_ptls_states_init;
static jl_tls_states_t *jl_get_ptls_states_init(void)
{
    // This is clearly not thread safe but should be fine since we
    // make sure the tls states callback is finalized before adding
    // multiple threads
    jl_tls_states_cb = jl_get_ptls_states_fallback;
    return jl_get_ptls_states_fallback();
}
JL_DLLEXPORT JL_CONST_FUNC jl_tls_states_t *(jl_get_ptls_states)(void)
{
    return (*jl_tls_states_cb)();
}
JL_DLLEXPORT void jl_set_ptls_states_getter(jl_get_ptls_states_func f)
{
    // only allow setting this once
    if (f && f != jl_get_ptls_states_init &&
        jl_tls_states_cb == jl_get_ptls_states_init) {
        jl_tls_states_cb = f;
    }
}
jl_get_ptls_states_func jl_get_ptls_states_getter(void)
{
    if (jl_tls_states_cb == jl_get_ptls_states_init)
        jl_get_ptls_states_init();
    // for codegen
    return jl_tls_states_cb;
}
#else
JL_DLLEXPORT jl_tls_states_t jl_tls_states;
JL_DLLEXPORT JL_CONST_FUNC jl_tls_states_t *(jl_get_ptls_states)(void)
{
    return &jl_tls_states;
}
#endif

// thread ID
JL_DLLEXPORT int jl_n_threads;     // # threads we're actually using
JL_DLLEXPORT int jl_max_threads;   // # threads possible
jl_thread_task_state_t *jl_all_task_states;
jl_gcframe_t ***jl_all_pgcstacks;

// return calling thread's ID
JL_DLLEXPORT int16_t jl_threadid(void) { return ti_tid; }

struct _jl_thread_heap_t *jl_mk_thread_heap(void);
// must be called by each thread at startup

static void ti_initthread(int16_t tid)
{
    ti_tid = tid;
    jl_pgcstack = NULL;
    jl_all_pgcstacks[tid] = &jl_pgcstack;
#ifdef JULIA_ENABLE_THREADING
    jl_all_heaps[tid] = jl_mk_thread_heap();
#else
    jl_mk_thread_heap();
#endif

    jl_all_task_states[tid].ptls = jl_get_ptls_states();
    jl_all_task_states[tid].signal_stack = jl_install_thread_signal_handler();
}

static void ti_init_master_thread(void)
{
#ifdef _OS_WINDOWS_
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                         GetCurrentProcess(), &hMainThread, 0,
                         TRUE, DUPLICATE_SAME_ACCESS)) {
        jl_printf(JL_STDERR, "WARNING: failed to access handle to main thread\n");
        hMainThread = INVALID_HANDLE_VALUE;
    }
    jl_all_task_states[0].system_id = hMainThread;
#else
    jl_all_task_states[0].system_id = pthread_self();
#endif
    ti_initthread(0);
}

// all threads call this function to run user code
static jl_value_t *ti_run_fun(jl_function_t *f, jl_svec_t *args)
{
    JL_TRY {
        jl_apply(f, jl_svec_data(args), jl_svec_len(args));
    }
    JL_CATCH {
        return jl_exception_in_transit;
    }
    return jl_nothing;
}


#ifdef JULIA_ENABLE_THREADING

// lock for code generation
JL_DEFINE_MUTEX(codegen);
JL_DEFINE_MUTEX(typecache);

// thread heap
struct _jl_thread_heap_t **jl_all_heaps;

// only one thread group for now
ti_threadgroup_t *tgworld;

// for broadcasting work to threads
ti_threadwork_t threadwork;

#if PROFILE_JL_THREADING
double cpu_ghz;
uint64_t prep_ticks;
uint64_t *fork_ticks;
uint64_t *user_ticks;
uint64_t *join_ticks;
#endif

static uv_barrier_t thread_init_done;

// thread function: used by all except the main thread
void ti_threadfun(void *arg)
{
    ti_threadarg_t *ta = (ti_threadarg_t *)arg;
    ti_threadgroup_t *tg;
    ti_threadwork_t *work;

    // initialize this thread (set tid, create heap, etc.)
    ti_initthread(ta->tid);

    // set up tasking
    jl_init_root_task(0,0);
#ifdef COPY_STACKS
    jl_set_base_ctx((char*)&arg);
#endif

    // set the thread-local tid and wait for a thread group
    while (ta->state == TI_THREAD_INIT)
        cpu_pause();
    cpu_lfence();
    uv_barrier_wait(&thread_init_done);
    // initialize this thread in the thread group
    tg = ta->tg;
    ti_threadgroup_initthread(tg, ti_tid);

    // free the thread argument here
    free(ta);

    // work loop
    for (; ;) {
#if PROFILE_JL_THREADING
        uint64_t tstart = rdtsc();
#endif

        ti_threadgroup_fork(tg, ti_tid, (void **)&work);

#if PROFILE_JL_THREADING
        uint64_t tfork = rdtsc();
        fork_ticks[ti_tid] += tfork - tstart;
#endif

        if (work) {
            if (work->command == TI_THREADWORK_DONE)
                break;
            else if (work->command == TI_THREADWORK_RUN)
                // TODO: return value? reduction?
                ti_run_fun(work->fun, work->args);
        }

#if PROFILE_JL_THREADING
        uint64_t tuser = rdtsc();
        user_ticks[ti_tid] += tuser - tfork;
#endif

        ti_threadgroup_join(tg, ti_tid);

#if PROFILE_JL_THREADING
        uint64_t tjoin = rdtsc();
        join_ticks[ti_tid] += tjoin - tuser;
#endif

        // TODO:
        // nowait should skip the join, but confirm that fork is reentrant
    }
}

#if PROFILE_JL_THREADING
void ti_reset_timings(void);
#endif

// interface to Julia; sets up to make the runtime thread-safe
void jl_init_threading(void)
{
    char *cp;

    // how many threads available, usable
    jl_max_threads = jl_cpu_cores();
    jl_n_threads = DEFAULT_NUM_THREADS;
    cp = getenv(NUM_THREADS_NAME);
    if (cp) {
        jl_n_threads = (uint64_t)strtol(cp, NULL, 10);
    }
    if (jl_n_threads > jl_max_threads)
        jl_n_threads = jl_max_threads;

    // set up space for per-thread heaps
    jl_all_heaps = (struct _jl_thread_heap_t **)malloc(jl_n_threads * sizeof(void*));
    jl_all_pgcstacks = (jl_gcframe_t ***)malloc(jl_n_threads * sizeof(void*));
    jl_all_task_states = (jl_thread_task_state_t *)malloc(jl_n_threads * sizeof(jl_thread_task_state_t));

#if PROFILE_JL_THREADING
    // estimate CPU speed
    uint64_t cpu_tim = rdtsc();
    sleep(1);
    cpu_ghz = ((double)(rdtsc() - cpu_tim)) / 1e9;

    // set up space for profiling information
    fork_ticks = (uint64_t*)jl_malloc_aligned(jl_n_threads * sizeof(uint64_t), 64);
    user_ticks = (uint64_t*)jl_malloc_aligned(jl_n_threads * sizeof(uint64_t), 64);
    join_ticks = (uint64_t*)jl_malloc_aligned(jl_n_threads * sizeof(uint64_t), 64);
    ti_reset_timings();
#endif

    // initialize this master thread (set tid, create heap, etc.)
    ti_init_master_thread();
}

void jl_start_threads(void)
{
    char *cp, mask[UV_CPU_SETSIZE];
    int i, exclusive;
    uv_thread_t uvtid;
    ti_threadarg_t **targs;

    // do we have exclusive use of the machine? default is no
    exclusive = DEFAULT_MACHINE_EXCLUSIVE;
    cp = getenv(MACHINE_EXCLUSIVE_NAME);
    if (cp)
        exclusive = strtol(cp, NULL, 10);

    // exclusive use: affinitize threads, master thread on proc 0, rest
    // according to a 'compact' policy
    // non-exclusive: no affinity settings; let the kernel move threads about
    if (exclusive) {
        memset(mask, 0, UV_CPU_SETSIZE);
        mask[0] = 1;
        uvtid = (uv_thread_t)uv_thread_self();
        uv_thread_setaffinity(&uvtid, mask, NULL, UV_CPU_SETSIZE);
    }

    // create threads
    targs = (ti_threadarg_t **)malloc((jl_n_threads - 1) * sizeof (ti_threadarg_t *));

    uv_barrier_init(&thread_init_done, jl_n_threads);

    for (i = 0;  i < jl_n_threads - 1;  ++i) {
        targs[i] = (ti_threadarg_t *)malloc(sizeof (ti_threadarg_t));
        targs[i]->state = TI_THREAD_INIT;
        targs[i]->tid = i + 1;
        uv_thread_create(&uvtid, ti_threadfun, targs[i]);
        if (exclusive) {
            memset(mask, 0, UV_CPU_SETSIZE);
            mask[i+1] = 1;
            uv_thread_setaffinity(&uvtid, mask, NULL, UV_CPU_SETSIZE);
        }
        uv_thread_detach(&uvtid);
        jl_all_task_states[i + 1].system_id = uvtid;
    }

    // set up the world thread group
    ti_threadgroup_create(1, jl_n_threads, 1, &tgworld);
    for (i = 0;  i < jl_n_threads;  ++i)
        ti_threadgroup_addthread(tgworld, i, NULL);
    ti_threadgroup_initthread(tgworld, ti_tid);

    // give the threads the world thread group; they will block waiting for fork
    for (i = 0;  i < jl_n_threads - 1;  ++i) {
        targs[i]->tg = tgworld;
        cpu_sfence();
        targs[i]->state = TI_THREAD_WORK;
    }

    uv_barrier_wait(&thread_init_done);

    // free the argument array; the threads will free their arguments
    free(targs);
}

// TODO: is this needed? where/when/how to call it?
void jl_shutdown_threading(void)
{
    // stop the spinning threads by sending them a command
    ti_threadwork_t *work = &threadwork;

    work->command = TI_THREADWORK_DONE;
    ti_threadgroup_fork(tgworld, ti_tid, (void **)&work);

    sleep(1);

    // destroy the world thread group
    ti_threadgroup_destroy(tgworld);

    // TODO: clean up and free the per-thread heaps

#if PROFILE_JL_THREADING
    jl_free_aligned(join_ticks);
    jl_free_aligned(user_ticks);
    jl_free_aligned(fork_ticks);
    fork_ticks = user_ticks = join_ticks = NULL;
#endif
}

// return thread's thread group
JL_DLLEXPORT void *jl_threadgroup(void) { return (void *)tgworld; }

// utility
JL_DLLEXPORT void jl_cpu_pause(void) { cpu_pause(); }

// interface to user code: specialize and compile the user thread function
// and run it in all threads
JL_DLLEXPORT jl_value_t *jl_threading_run(jl_function_t *f, jl_svec_t *args)
{
#if PROFILE_JL_THREADING
    uint64_t tstart = rdtsc();
#endif

    jl_tupletype_t *argtypes = NULL;
    jl_function_t *fun = NULL;
    if ((jl_value_t*)args == jl_emptytuple)
        args = jl_emptysvec;
    JL_TYPECHK(jl_threading_run, function, (jl_value_t*)f);
    JL_TYPECHK(jl_threading_run, simplevector, (jl_value_t*)args);

    JL_GC_PUSH2(&argtypes, &fun);
    if (jl_svec_len(args) == 0)
        argtypes = (jl_tupletype_t*)jl_typeof(jl_emptytuple);
    else
        argtypes = arg_type_tuple(jl_svec_data(args), jl_svec_len(args));
    fun = jl_get_specialization(f, argtypes);
    if (fun == NULL)
        fun = f;
    jl_generate_fptr(fun);

    threadwork.command = TI_THREADWORK_RUN;
    threadwork.fun = fun;
    threadwork.args = args;
    threadwork.ret = jl_nothing;

#if PROFILE_JL_THREADING
    uint64_t tcompile = rdtsc();
    prep_ticks += (tcompile - tstart);
#endif

    // fork the world thread group
    ti_threadwork_t *tw = (ti_threadwork_t *)&threadwork;
    ti_threadgroup_fork(tgworld, ti_tid, (void **)&tw);

#if PROFILE_JL_THREADING
    uint64_t tfork = rdtsc();
    fork_ticks[ti_tid] += (tfork - tcompile);
#endif

    // this thread must do work too (TODO: reduction?)
    tw->ret = ti_run_fun(fun, args);

#if PROFILE_JL_THREADING
    uint64_t trun = rdtsc();
    user_ticks[ti_tid] += (trun - tfork);
#endif

    // wait for completion (TODO: nowait?)
    ti_threadgroup_join(tgworld, ti_tid);

#if PROFILE_JL_THREADING
    uint64_t tjoin = rdtsc();
    join_ticks[ti_tid] += (tjoin - trun);
#endif

    JL_GC_POP();

    return tw->ret;
}

#if PROFILE_JL_THREADING

void ti_reset_timings(void)
{
    int i;
    prep_ticks = 0;
    for (i = 0;  i < jl_n_threads;  i++)
        fork_ticks[i] = user_ticks[i] = join_ticks[i] = 0;
}

void ti_timings(uint64_t *times, uint64_t *min, uint64_t *max, uint64_t *avg)
{
    int i;
    *min = UINT64_MAX;
    *max = *avg = 0;
    for (i = 0;  i < jl_n_threads;  i++) {
        if (times[i] < *min)
            *min = times[i];
        if (times[i] > *max)
            *max = times[i];
        *avg += times[i];
    }
    *avg /= jl_n_threads;
}

#define TICKS_TO_SECS(t)        (((double)(t)) / (cpu_ghz * 1e9))

JL_DLLEXPORT void jl_threading_profile(void)
{
    if (!fork_ticks) return;

    printf("\nti profile:\n");
    printf("prep: %g (%llu)\n", TICKS_TO_SECS(prep_ticks), (unsigned long long)prep_ticks);

    uint64_t min, max, avg;
    ti_timings(fork_ticks, &min, &max, &avg);
    printf("fork: %g (%g - %g)\n", TICKS_TO_SECS(min), TICKS_TO_SECS(max),
            TICKS_TO_SECS(avg));
    ti_timings(user_ticks, &min, &max, &avg);
    printf("user: %g (%g - %g)\n", TICKS_TO_SECS(min), TICKS_TO_SECS(max),
            TICKS_TO_SECS(avg));
    ti_timings(join_ticks, &min, &max, &avg);
    printf("join: %g (%g - %g)\n", TICKS_TO_SECS(min), TICKS_TO_SECS(max),
            TICKS_TO_SECS(avg));
}

#else //!PROFILE_JL_THREADING

JL_DLLEXPORT void jl_threading_profile(void)
{
}

#endif //!PROFILE_JL_THREADING

#else // !JULIA_ENABLE_THREADING

JL_DLLEXPORT jl_value_t *jl_threading_run(jl_function_t *f, jl_svec_t *args)
{
    if ((jl_value_t*)args == jl_emptytuple)
        args = jl_emptysvec;
    JL_TYPECHK(jl_threading_run, function, (jl_value_t*)f);
    JL_TYPECHK(jl_threading_run, simplevector, (jl_value_t*)args);
    return ti_run_fun(f, args);
}

void jl_init_threading(void)
{
    static jl_thread_task_state_t _jl_all_task_states;
    jl_all_task_states = &_jl_all_task_states;
    jl_max_threads = 1;
    jl_n_threads = 1;
    jl_all_pgcstacks = (jl_gcframe_t***) malloc(jl_n_threads * sizeof(jl_gcframe_t**));

#if defined(__linux__) && defined(JL_USE_INTEL_JITEVENTS)
    if (jl_using_intel_jitevents)
        // Intel VTune Amplifier needs at least 64k for alternate stack.
        if (SIGSTKSZ < 1<<16)
            sig_stack_size = 1<<16;
#endif

    ti_init_master_thread();
}

void jl_start_threads(void) { }

#endif // !JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif
