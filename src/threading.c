/*
Copyright (c) 2014, Intel Corporation

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
  threading infrastructure
  . thread and threadgroup creation
  . thread function
  . invoke Julia function from multiple threads
*/


#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>

#include "julia.h"
#include "julia_internal.h"
#include "uv.h"

#include "ia_misc.h"
#include "threadgroup.h"
#include "threading.h"


/* TODO:
  . ugly mixture of uv_thread* and pthread*; fix with patch to libuv?
  . fix interface to properly support thread groups
  . add queue per thread for tasks
  . add reduction; reduce values returned from thread function
  . make code generation thread-safe and remove the lock
*/

// lock for code generation
JL_DEFINE_MUTEX(codegen);

// thread ID
JL_THREAD int16_t ti_tid = 0;

// thread heap
struct _jl_thread_heap_t **jl_all_heaps;
jl_gcframe_t ***jl_all_pgcstacks;
jl_thread_task_state_t *jl_all_task_states;

// only one thread group for now
ti_threadgroup_t *tgworld;

// for broadcasting work to threads
ti_threadwork_t threadwork;

DLLEXPORT int jl_max_threads;	// # threads possible
DLLEXPORT int jl_n_threads;	// # threads we're actually using

#if PROFILE_JL_THREADING
double cpu_ghz;
uint64_t prep_ticks;
uint64_t *fork_ticks;
uint64_t *user_ticks;
uint64_t *join_ticks;
#endif


// create a thread and affinitize it if proc_num is specified
int ti_threadcreate(uint64_t *pthread_id, int proc_num,
                    void *(*thread_fun)(void *), void *thread_arg)
{
    pthread_attr_t attr;
    pthread_attr_init(&attr);

#ifdef _OS_LINUX_
    cpu_set_t cset;
    if (proc_num >= 0) {
	CPU_ZERO(&cset);
	CPU_SET(proc_num, &cset);
	pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cset);
    }
#endif

    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    return pthread_create(pthread_id, &attr, thread_fun, thread_arg);
}


// set thread affinity
void ti_threadsetaffinity(uint64_t pthread_id, int proc_num)
{
#ifdef _OS_LINUX_
    cpu_set_t cset;

    CPU_ZERO(&cset);
    CPU_SET(proc_num, &cset);
    pthread_setaffinity_np(pthread_id, sizeof(cpu_set_t), &cset);
#endif
}


struct _jl_thread_heap_t *jl_mk_thread_heap(void);

// must be called by each thread at startup
void ti_initthread(int16_t tid)
{
    ti_tid = tid;
    jl_pgcstack = NULL;
    jl_all_pgcstacks[tid] = &jl_pgcstack;
    jl_all_heaps[tid] = jl_mk_thread_heap();

    jl_all_task_states[tid].pcurrent_task = &jl_current_task;
    jl_all_task_states[tid].proot_task = &jl_root_task;
    jl_all_task_states[tid].pexception_in_transit = &jl_exception_in_transit;
    jl_all_task_states[tid].ptask_arg_in_transit = &jl_task_arg_in_transit;
}


// all threads call this function to run user code
static jl_value_t *ti_run_fun(jl_function_t *f, jl_tuple_t *args)
{
    JL_TRY {
        jl_apply(f, jl_tuple_data(args), jl_tuple_len(args));
    }
    JL_CATCH {
        return jl_exception_in_transit;
    }
    return jl_nothing;
}


// thread function: used by all except the main thread
void *ti_threadfun(void *arg)
{
    ti_threadarg_t *ta = (ti_threadarg_t *)arg;
    ti_threadgroup_t *tg;
    ti_threadwork_t *work;

    // initialize this thread (set tid, create heap, etc.)
    ti_initthread(ta->tid);

    // set up tasking
    jl_init_root_task(0,0);
    jl_set_stackbase((char*)&arg);
    jl_set_base_ctx((char*)&arg);

    // set the thread-local tid and wait for a thread group
    while (ta->state == TI_THREAD_INIT)
        cpu_pause();
    cpu_lfence();

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

    return NULL;
}


#if PROFILE_JL_THREADING
void ti_reset_timings();
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
    jl_all_heaps = malloc(jl_n_threads * sizeof(void*));
    jl_all_pgcstacks = malloc(jl_n_threads * sizeof(void*));
    jl_all_task_states = malloc(jl_n_threads * sizeof(jl_thread_task_state_t));

#if PROFILE_JL_THREADING
    // estimate CPU speed
    uint64_t cpu_tim = rdtsc();
    sleep(1);
    cpu_ghz = ((double)(rdtsc() - cpu_tim)) / 1e9;

    // set up space for profiling information
    fork_ticks = (uint64_t *)_mm_malloc(jl_n_threads * sizeof (uint64_t), 64);
    user_ticks = (uint64_t *)_mm_malloc(jl_n_threads * sizeof (uint64_t), 64);
    join_ticks = (uint64_t *)_mm_malloc(jl_n_threads * sizeof (uint64_t), 64);
    ti_reset_timings();
#endif

    // initialize this master thread (set tid, create heap, etc.)
    ti_initthread(0);
}

void jl_start_threads(void)
{
    char *cp;
    int i, exclusive;
    uint64_t ptid;
    ti_threadarg_t **targs;

    // do we have exclusive use of the machine? default is no
    exclusive = DEFAULT_MACHINE_EXCLUSIVE;
    cp = getenv(MACHINE_EXCLUSIVE_NAME);
    if (cp)
	exclusive = strtol(cp, NULL, 10);

    // exclusive use: affinitize threads, master thread on proc 0, rest
    // according to a 'compact' policy
    // non-exclusive: no affinity settings; let the kernel move threads about
    if (exclusive)
	ti_threadsetaffinity(uv_thread_self(), 0);

    // create threads
    targs = malloc((jl_n_threads - 1) * sizeof (ti_threadarg_t *));
    for (i = 0;  i < jl_n_threads - 1;  ++i) {
        targs[i] = (ti_threadarg_t *)malloc(sizeof (ti_threadarg_t));
        targs[i]->state = TI_THREAD_INIT;
        targs[i]->tid = i + 1;
        ti_threadcreate(&ptid, exclusive ? i+1 : -1, ti_threadfun, targs[i]);
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
    _mm_free(join_ticks);
    _mm_free(user_ticks);
    _mm_free(fork_ticks);
    fork_ticks = user_ticks = join_ticks = NULL;
#endif
}


// return calling thread's ID
int16_t jl_threadid()
{
    return ti_tid;
}


// return thread's thread group
void *jl_threadgroup()
{
    return (void *)tgworld;
}


// utility
void jl_cpu_pause()
{
    cpu_pause();
}


// interface to user code: specialize and compile the user thread function
// and run it in all threads
DLLEXPORT jl_value_t *jl_threading_run(jl_function_t *f, jl_tuple_t *args)
{
#if PROFILE_JL_THREADING
    uint64_t tstart = rdtsc();
#endif

    jl_tuple_t *argtypes = NULL;
    jl_function_t *fun = NULL;
    JL_GC_PUSH2(&argtypes, &fun);
    argtypes = arg_type_tuple(jl_tuple_data(args), jl_tuple_len(args));
    fun = jl_get_specialization(f, argtypes);
    if (fun == NULL)
        fun = f;
    jl_compile(fun);
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

void ti_reset_timings()
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

void jl_threading_profile()
{
    if (!fork_ticks) return;

    printf("\nti profile:\n");
    printf("prep: %g (%lu)\n", TICKS_TO_SECS(prep_ticks), prep_ticks);

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

#else

void jl_threading_profile()
{
}

#endif
