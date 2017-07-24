// This file is a part of Julia. License is MIT: https://julialang.org/license


/*
 * Run julia code using libuv's threadpool
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "julia.h"
#include "julia_internal.h"
#include "threading.h"

#ifdef _OS_LINUX_
#include <sys/syscall.h>
#endif


__thread int16_t jl_tp_init_done = 0;
JL_DLLEXPORT int jl_n_uv_threads;  // # threads in the UV threadpool
JL_DLLEXPORT int tp_next_tid;      // # julia thread id

typedef void (*tp_workfun_t)(void *);
typedef void (*tp_notify_cb_t)(int);

JL_DLLEXPORT void init_exec_on_main_queue();

// init once
void jl_init_uv_threadpool()
{
    tp_next_tid = jl_n_threads + 1; // initial tid for a uv threadpool thread starts after
                                    // the set of julia threads.
    init_exec_on_main_queue();      // init machinery to forward calls to the main thread.
}

// init a threadpool worker thread
static void tp_initthread()
{
    jl_ptls_t ptls = jl_get_ptls_states();
#ifndef _OS_WINDOWS_
    ptls->system_id = pthread_self();
#endif
    int16_t tid = jl_atomic_fetch_add(&tp_next_tid, 1);
    //printf("tid : %" PRIu16 "\n", tid);

    // Init code copied over from threading.c
    // TODO : Fix it
    ptls->tid = tid;
    ptls->pgcstack = NULL;
    ptls->gc_state = 0; // GC unsafe
    // Conditionally initialize the safepoint address. See comment in
    // `safepoint.c`
    ptls->safepoint = (size_t*)(jl_safepoint_pages + jl_page_size * 2 + sizeof(size_t));

    ptls->defer_signal = 0;
    ptls->current_module = NULL;
    void *bt_data = malloc(sizeof(uintptr_t) * (JL_MAX_BT_SIZE + 1));
    if (bt_data == NULL) {
        jl_printf(JL_STDERR, "could not allocate backtrace buffer\n");
        gc_debug_critical_error();
        abort();
    }
    ptls->bt_data = (uintptr_t*)bt_data;
    jl_init_thread_heap(ptls);
    jl_install_thread_signal_handler(ptls);

    jl_all_tls_states[tid] = ptls;
    jl_tp_init_done = 1;
}

// struct queued and executed on worker threads (via the UV API).
typedef struct {
    uv_work_t       req;           // libuv reference
    tp_workfun_t    workfun;       // Julia function run on worker thread
    void            *workfun_arg;  // arg to workfun (actually a jl_value_t*)

    tp_notify_cb_t  notify_cb;     // libuv calls this when workfun completes in the main thread
    int             notify_ref;    // arg to notify_cb

    jl_module_t     *current_module;
    size_t          world_age;
} tp_work_t;


// libuv calls this on a worker thread
void tp_run_work(uv_work_t *req)
{
    if (!jl_tp_init_done)
        tp_initthread();

    // Stuff copied over from the current thread implemention.
    // TODO : cleanup
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_init_stack_limits(0);

    // set up tasking
    jl_init_root_task(ptls->stack_lo, ptls->stack_hi - ptls->stack_lo);
#ifdef COPY_STACKS
//    jl_set_base_ctx((char*)&arg);
#endif
   jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);


    tp_work_t *p_work = (tp_work_t *) req->data;

    int8_t gc_state = jl_gc_unsafe_enter(ptls);
    // This is probably always NULL for now
    jl_module_t *last_m = ptls->current_module;
    size_t last_age = ptls->world_age;
    JL_GC_PUSH1(&last_m);
    ptls->current_module = p_work->current_module;
    ptls->world_age = p_work->world_age;

    //printf("Running workfun %p %p\n", p_work->workfun, p_work->workfun_arg);
    p_work->workfun(p_work->workfun_arg);

    ptls->current_module = last_m;
    ptls->world_age = last_age;
    JL_GC_POP();
    jl_gc_unsafe_leave(ptls, gc_state);
}

// libuv calls this in the main thread when the queued workfun completes
void tp_work_done(uv_work_t *req, int status)
{
    tp_work_t *p_work = (tp_work_t *) req->data;
    p_work->notify_cb(p_work->notify_ref);
    free(p_work);
}

// Called from the main thread when tp_run_work exits
JL_DLLEXPORT void jl_tp_queue(tp_workfun_t workfun, void * workfun_arg, tp_notify_cb_t notify_cb, int notify_ref)
{
    tp_work_t *p_work = (tp_work_t *)malloc(sizeof(tp_work_t));
    p_work->req.data = (void*) p_work;

    jl_ptls_t ptls = jl_get_ptls_states();

    p_work->workfun = workfun;
    p_work->workfun_arg = workfun_arg;
    p_work->notify_cb = notify_cb;
    p_work->notify_ref = notify_ref;
    p_work->current_module = ptls->current_module;
    p_work->world_age = ptls->world_age;

    // add to queue and return. The julia task waits on a Condition variable
    // notified by tp_work_done
    uv_queue_work(jl_io_loop, &p_work->req, tp_run_work, tp_work_done);

    return;
}

/*
 * Enable Julia code running in worker threads to forward a f(args)
 * for execution on the main thread
 */


#include "../deps/srccache/libuv/src/queue.h"

// QUEUE to process work items in the main thread
static QUEUE       q_exec_on_main;
static uv_mutex_t  q_mut;       // safe access to q_exec_on_main

static void jl_on_main_async_cb(uv_async_t * h);

// struct to pass requests to be executed on the main thread.
typedef struct exec_on_main_task_s {
    void        *f;     // function object, anonymous functions are not supported
    void        *args;
    void        *retval;

    // Notify waiter on calling thread of run and exit of f(args) run on main thread
    int         isdone;
    uv_mutex_t  mut;
    uv_cond_t   cond;

    // Shared list between all threads.
    QUEUE node;
} exec_on_main_task_t;

uv_async_t   on_main_async;  // uv_async_send handle
typedef jl_value_t* (*tp_onmainfun_t)(void *, void *);
tp_onmainfun_t      jl_onmain_cb;      // Julia function the actual requested forwarded function


JL_DLLEXPORT void init_on_main_cb(void * cb)
{
    jl_onmain_cb = (tp_onmainfun_t) cb;
}

JL_DLLEXPORT void init_exec_on_main_queue()
{
    QUEUE_INIT(&q_exec_on_main);
    uv_mutex_init(&q_mut);
    uv_async_init(jl_io_loop, &on_main_async, jl_on_main_async_cb);
}

// wakeup the main thread which then calls jl_on_main_async_cb
void tickle_libuv(void)
{
    uv_async_send(&on_main_async);
}

// Entry function, called from worker threads to request execution of
// f(args) on the main thread. Anonymous functions are not supported.
JL_DLLEXPORT jl_value_t* jl_exec_on_main(void * f, void * args)
{
    exec_on_main_task_t on_main;

    on_main.f = f;
    on_main.args = args;
    uv_mutex_init(&on_main.mut);
    uv_cond_init(&on_main.cond);

    // Add to queue
    on_main.isdone = 0;
    QUEUE_INIT(&on_main.node);

    uv_mutex_lock(&q_mut);
    QUEUE_INSERT_TAIL(&q_exec_on_main, &on_main.node);
    uv_mutex_unlock(&q_mut);

    // wake-up the main thread
    tickle_libuv();

    // wait for completion of request
    uv_mutex_lock(&on_main.mut);
    if (!on_main.isdone) {
        uv_cond_wait(&on_main.cond, &on_main.mut);
    }
    uv_mutex_unlock(&on_main.mut);

    uv_mutex_destroy(&on_main.mut);
    uv_cond_destroy(&on_main.cond);

    return on_main.retval;
}

// libuv calls this on the main thread when tickle_libuv is called
static void jl_on_main_async_cb(uv_async_t * h)
{
    QUEUE* q;
    exec_on_main_task_t * p_work;

    // process q
    uv_mutex_lock(&q_mut);

    // TODO : Each entry must be executed asynchronously using Julia's Task infrastructure
    while (!QUEUE_EMPTY(&q_exec_on_main))
    {
        q = QUEUE_HEAD(&q_exec_on_main);
        p_work = QUEUE_DATA(q, exec_on_main_task_t, node);

        p_work->retval = jl_onmain_cb(p_work->f, p_work->args);

        uv_mutex_lock(&p_work->mut);
        p_work->isdone = 1;
        uv_cond_signal(&p_work->cond);
        uv_mutex_unlock(&p_work->mut);

        QUEUE_REMOVE(q);
    }

    uv_mutex_unlock(&q_mut);
}
