// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  task.c
  lightweight processes (symmetric coroutines)
*/

// need this to get the real definition of ucontext_t,
// if we're going to use the ucontext_t implementation there
//#if defined(__APPLE__) && defined(JL_HAVE_UCONTEXT)
//#pragma push_macro("_XOPEN_SOURCE")
//#define _XOPEN_SOURCE
//#include <ucontext.h>
//#pragma pop_macro("_XOPEN_SOURCE")
//#endif

// this is needed for !COPY_STACKS to work on linux
#ifdef _FORTIFY_SOURCE
// disable __longjmp_chk validation so that we can jump between stacks
// (which would normally be invalid to do with setjmp / longjmp)
#pragma push_macro("_FORTIFY_SOURCE")
#undef _FORTIFY_SOURCE
#include <setjmp.h>
#pragma pop_macro("_FORTIFY_SOURCE")
#endif

#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_)
volatile int jl_in_stackwalk = 0;
#else
#include <sys/mman.h> // mmap
#ifdef JL_HAVE_UCONTEXT
#include <ucontext.h>
#endif
#endif

// empirically, jl_finish_task needs about 64k stack space to infer/run
// and additionally, gc-stack reserves 64k for the guard pages
#if defined(MINSIGSTKSZ) && MINSIGSTKSZ > 131072
#define MINSTKSZ MINSIGSTKSZ
#else
#define MINSTKSZ 131072
#endif

#define ROOT_TASK_STACK_ADJUSTMENT 3000000

jl_sym_t *done_sym;
jl_sym_t *failed_sym;
jl_sym_t *runnable_sym;

extern size_t jl_page_size;
static char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner) JL_NOTSAFEPOINT;
static void jl_set_fiber(jl_ucontext_t *t);
static void jl_start_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t);
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t);

#ifdef JL_HAVE_UNW_CONTEXT
static JL_THREAD unw_cursor_t jl_basecursor;
#endif

#ifdef ALWAYS_COPY_STACKS
# ifndef COPY_STACKS
# error "ALWAYS_COPY_STACKS requires COPY_STACKS"
# endif
static int always_copy_stacks = 1;
#else
static int always_copy_stacks = 0;
#endif

#ifdef COPY_STACKS

static void memcpy_a16(uint64_t *to, uint64_t *from, size_t nb)
{
    memcpy((char*)jl_assume_aligned(to, 16), (char*)jl_assume_aligned(from, 16), nb);
    //uint64_t *end = (uint64_t*)((char*)from + nb);
    //while (from < end)
    //    *(to++) = *(from++);
}

static void NOINLINE save_stack(jl_ptls_t ptls, jl_task_t *lastt, jl_task_t **pt)
{
    char *frame_addr = (char*)((uintptr_t)jl_get_frame_addr() & ~15);
    char *stackbase = (char*)ptls->stackbase;
    assert(stackbase > frame_addr);
    size_t nb = stackbase - frame_addr;
    void *buf;
    if (lastt->bufsz < nb) {
        buf = (void*)jl_gc_alloc_buf(ptls, nb);
        lastt->stkbuf = buf;
        lastt->bufsz = nb;
    }
    else {
        buf = lastt->stkbuf;
    }
    *pt = lastt; // clear the gc-root for the target task before copying the stack for saving
    lastt->copy_stack = nb;
    lastt->sticky = 1;
    memcpy_a16((uint64_t*)buf, (uint64_t*)frame_addr, nb);
    // this task's stack could have been modified after
    // it was marked by an incremental collection
    // move the barrier back instead of walking it again here
    jl_gc_wb_back(lastt);
}

static void NOINLINE JL_NORETURN restore_stack(jl_task_t *t, jl_ptls_t ptls, char *p)
{
    size_t nb = t->copy_stack;
    char *_x = (char*)ptls->stackbase - nb;
    if (!p) {
        // switch to a stackframe that's beyond the bounds of the last switch
        p = _x;
        if ((char*)&_x > _x) {
            p = (char*)alloca((char*)&_x - _x);
        }
        restore_stack(t, ptls, p); // pass p to ensure the compiler can't tailcall this or avoid the alloca
    }
    assert(t->stkbuf != NULL);
    memcpy_a16((uint64_t*)_x, (uint64_t*)t->stkbuf, nb); // destroys all but the current stackframe
    jl_set_fiber(&t->ctx);
    abort(); // unreachable
}
static void restore_stack2(jl_task_t *t, jl_ptls_t ptls, jl_task_t *lastt)
{
    size_t nb = t->copy_stack;
    char *_x = (char*)ptls->stackbase - nb;
    assert(t->stkbuf != NULL);
    memcpy_a16((uint64_t*)_x, (uint64_t*)t->stkbuf, nb); // destroys all but the current stackframe
    jl_swap_fiber(&lastt->ctx, &t->ctx);
}
#endif

/* Rooted by the base module */
static jl_function_t *task_done_hook_func JL_GLOBALLY_ROOTED = NULL;

void JL_NORETURN jl_finish_task(jl_task_t *t, jl_value_t *resultval JL_MAYBE_UNROOTED)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_SIGATOMIC_BEGIN();
    if (t->exception != jl_nothing)
        t->state = failed_sym;
    else
        t->state = done_sym;
    if (t->copy_stack) // early free of stkbuf
        t->stkbuf = NULL;
    t->result = resultval;
    jl_gc_wb(t, t->result);
    // ensure that state is cleared
    ptls->in_finalizer = 0;
    ptls->in_pure_callback = 0;
    jl_get_ptls_states()->world_age = jl_world_counter;
    // let the runtime know this task is dead and find a new task to run
    if (task_done_hook_func == NULL) {
        task_done_hook_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                            jl_symbol("task_done_hook"));
    }
    if (task_done_hook_func != NULL) {
        jl_value_t *args[2] = {task_done_hook_func, (jl_value_t*)t};
        JL_TRY {
            jl_apply(args, 2);
        }
        JL_CATCH {
            jl_no_exc_handler(jl_current_exception());
        }
    }
    gc_debug_critical_error();
    abort();
}

JL_DLLEXPORT void *jl_task_stack_buffer(jl_task_t *task, size_t *size, int *tid)
{
    size_t off = 0;
#ifndef _OS_WINDOWS_
    if (jl_all_tls_states[0]->root_task == task) {
        // See jl_init_root_task(). The root task of the main thread
        // has its buffer enlarged by an artificial 3000000 bytes, but
        // that means that the start of the buffer usually points to
        // inaccessible memory. We need to correct for this.
        off = ROOT_TASK_STACK_ADJUSTMENT;
    }
#endif
    *tid = -1;
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls = jl_all_tls_states[i];
        if (ptls->current_task == task) {
            *tid = i;
#ifdef COPY_STACKS
            if (task->copy_stack) {
                *size = ptls->stacksize;
                return (char *)ptls->stackbase - *size;
            }
#endif
            break; // continue with normal return
        }
    }
    *size = task->bufsz - off;
    return (void *)((char *)task->stkbuf + off);
}

static void record_backtrace(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    // storing bt_size in ptls ensures roots in bt_data will be found
    ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE);
}

JL_DLLEXPORT void julia_init(JL_IMAGE_SEARCH rel)
{
    _julia_init(rel);
}

void jl_release_task_stack(jl_ptls_t ptls, jl_task_t *task);

static void ctx_switch(jl_ptls_t ptls, jl_task_t **pt)
{
    jl_task_t *t = *pt;
    assert(t != ptls->current_task);
    jl_task_t *lastt = ptls->current_task;
#ifdef JULIA_ENABLE_THREADING
    // If the current task is not holding any locks, free the locks list
    // so that it can be GC'd without leaking memory
    arraylist_t *locks = &lastt->locks;
    if (locks->len == 0 && locks->items != locks->_space) {
        arraylist_free(locks);
        arraylist_new(locks, 0);
    }
#endif

    int started = t->started;
    int killed = (lastt->state == done_sym || lastt->state == failed_sym);
    if (!started && !t->copy_stack) {
        // may need to allocate the stack
        if (t->stkbuf == NULL) {
            t->stkbuf = jl_alloc_fiber(&t->ctx, &t->bufsz, t);
            if (t->stkbuf == NULL) {
#ifdef COPY_STACKS
                // fall back to stack copying if mmap fails
                t->copy_stack = 1;
                t->sticky = 1;
                t->bufsz = 0;
                memcpy(&t->ctx, &ptls->base_ctx, sizeof(t->ctx));
#else
                jl_throw(jl_memory_exception);
#endif
            }
        }
    }

    if (killed) {
        *pt = lastt; // can't fail after here: clear the gc-root for the target task now
        lastt->gcstack = NULL;
        if (!lastt->copy_stack && lastt->stkbuf) {
            // early free of stkbuf back to the pool
            jl_release_task_stack(ptls, lastt);
        }
    }
    else {
#ifdef COPY_STACKS
        if (lastt->copy_stack) { // save the old copy-stack
            save_stack(ptls, lastt, pt); // allocates (gc-safepoint, and can also fail)
            if (jl_setjmp(lastt->ctx.uc_mcontext, 0)) {
                // TODO: mutex unlock the thread we just switched from
                return;
            }
        }
        else
#endif
        *pt = lastt; // can't fail after here: clear the gc-root for the target task now
        lastt->gcstack = ptls->pgcstack;
    }

    // set up global state for new task
    lastt->world_age = ptls->world_age;
    ptls->pgcstack = t->gcstack;
    ptls->world_age = t->world_age;
    t->gcstack = NULL;
#ifdef MIGRATE_TASKS
    ptls->previous_task = lastt;
#endif
    ptls->current_task = t;

    jl_ucontext_t *lastt_ctx = (killed ? NULL : &lastt->ctx);
#ifdef COPY_STACKS
    if (lastt->copy_stack)
        // if we are switching between copy-stacks,
        // don't save the old copy-stack
        // instead resume at jl_setjmp of the other task,
        // after restoring the stack
        lastt_ctx = NULL;
#endif
    if (started) {
#ifdef COPY_STACKS
        if (t->copy_stack) {
            if (lastt_ctx)
                restore_stack2(t, ptls, lastt);
            else if (lastt->copy_stack)
                restore_stack(t, ptls, NULL);     // (doesn't return)
            else
                restore_stack(t, ptls, (char*)1); // (doesn't return)
        }
        else
#endif
        if (!lastt_ctx)
            jl_set_fiber(&t->ctx);
        else
            jl_swap_fiber(lastt_ctx, &t->ctx);
    }
    else {
        if (always_copy_stacks)
            jl_longjmp(ptls->base_ctx.uc_mcontext, 1);
        else
            jl_start_fiber(lastt_ctx, &t->ctx);
    }
}

static jl_ptls_t NOINLINE refetch_ptls(void)
{
    return jl_get_ptls_states();
}

JL_DLLEXPORT void jl_switchto(jl_task_t **pt)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *t = *pt;
    jl_task_t *ct = ptls->current_task;
    if (t == ct) {
        return;
    }
    if (t->state == done_sym || t->state == failed_sym ||
            (t->started && t->stkbuf == NULL)) {
        ct->exception = t->exception;
        ct->result = t->result;
        return;
    }
    if (ptls->in_finalizer)
        jl_error("task switch not allowed from inside gc finalizer");
    if (ptls->in_pure_callback)
        jl_error("task switch not allowed from inside staged nor pure functions");
    if (t->sticky && jl_atomic_load_acquire(&t->tid) == -1) {
        // manually yielding to a task
        if (jl_atomic_compare_exchange(&t->tid, -1, ptls->tid) != -1)
            jl_error("cannot switch to task running on another thread");
    }
    else if (t->tid != ptls->tid) {
        jl_error("cannot switch to task running on another thread");
    }
    sig_atomic_t defer_signal = ptls->defer_signal;
    int8_t gc_state = jl_gc_unsafe_enter(ptls);

#ifdef ENABLE_TIMINGS
    jl_timing_block_t *blk = ct->timing_stack;
    if (blk)
        jl_timing_block_stop(blk);
#endif

    ctx_switch(ptls, pt);

#ifdef MIGRATE_TASKS
    ptls = refetch_ptls();
    t = ptls->previous_task;
    assert(t->tid == ptls->tid);
    if (!t->sticky && !t->copy_stack)
        t->tid = -1;
#else
    assert(ptls == refetch_ptls());
#endif

    ct = ptls->current_task;

#ifdef ENABLE_TIMINGS
    assert(blk == ct->timing_stack);
    if (blk)
        jl_timing_block_start(blk);
#endif

    jl_gc_unsafe_leave(ptls, gc_state);
    sig_atomic_t other_defer_signal = ptls->defer_signal;
    ptls->defer_signal = defer_signal;
    if (other_defer_signal && !defer_signal)
        jl_sigint_safepoint(ptls);
}

JL_DLLEXPORT JL_NORETURN void jl_no_exc_handler(jl_value_t *e)
{
    jl_printf(JL_STDERR, "fatal: error thrown and no exception handler available.\n");
    jl_static_show(JL_STDERR, e);
    jl_printf(JL_STDERR, "\n");
    jlbacktrace();
    jl_exit(1);
}

jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block);

// yield to exception handler
void JL_NORETURN throw_internal(jl_value_t *exception JL_MAYBE_UNROOTED)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ptls->io_wait = 0;
    if (ptls->safe_restore)
        jl_longjmp(*ptls->safe_restore, 1);
    JL_GC_PUSH1(&exception);
    jl_gc_unsafe_enter(ptls);
    if (exception) {
        // The temporary ptls->bt_data is rooted by special purpose code in the
        // GC. This exists only for the purpose of preserving bt_data until we
        // set ptls->bt_size=0 below.
        assert(ptls->current_task);
        jl_push_excstack(&ptls->current_task->excstack, exception,
                          ptls->bt_data, ptls->bt_size);
        ptls->bt_size = 0;
    }
    assert(ptls->current_task->excstack && ptls->current_task->excstack->top);
    jl_handler_t *eh = ptls->current_task->eh;
    if (eh != NULL) {
#ifdef ENABLE_TIMINGS
        jl_timing_block_t *cur_block = ptls->current_task->timing_stack;
        while (cur_block && eh->timing_stack != cur_block) {
            cur_block = jl_pop_timing_block(cur_block);
        }
        assert(cur_block == eh->timing_stack);
#endif
        jl_longjmp(eh->eh_ctx, 1);
    }
    else {
        jl_no_exc_handler(exception);
    }
    assert(0);
}

// record backtrace and raise an error
JL_DLLEXPORT void jl_throw(jl_value_t *e JL_MAYBE_UNROOTED)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(e != NULL);
    if (ptls->safe_restore)
        throw_internal(NULL);
    record_backtrace(ptls);
    throw_internal(e);
}

// rethrow with current excstack state
JL_DLLEXPORT void jl_rethrow(void)
{
    jl_excstack_t *excstack = jl_get_ptls_states()->current_task->excstack;
    if (!excstack || excstack->top == 0)
        jl_error("rethrow() not allowed outside a catch block");
    throw_internal(NULL);
}

// Special case throw for errors detected inside signal handlers.  This is not
// (cannot be) called directly in the signal handler itself, but is returned to
// after the signal handler exits.
JL_DLLEXPORT void jl_sig_throw(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *e = ptls->sig_exception;
    assert(e && ptls->bt_size != 0);
    ptls->sig_exception = NULL;
    throw_internal(e);
}

JL_DLLEXPORT void jl_rethrow_other(jl_value_t *e JL_MAYBE_UNROOTED)
{
    // TODO: Should uses of `rethrow(exc)` be replaced with a normal throw, now
    // that exception stacks allow root cause analysis?
    jl_excstack_t *excstack = jl_get_ptls_states()->current_task->excstack;
    if (!excstack || excstack->top == 0)
        jl_error("rethrow(exc) not allowed outside a catch block");
    // overwrite exception on top of stack. see jl_excstack_exception
    jl_excstack_raw(excstack)[excstack->top-1] = (uintptr_t)e;
    JL_GC_PROMISE_ROOTED(e);
    throw_internal(NULL);
}

JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t *start, jl_value_t *completion_future, size_t ssize)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *t = (jl_task_t*)jl_gc_alloc(ptls, sizeof(jl_task_t), jl_task_type);
    t->copy_stack = 0;
    if (ssize == 0) {
        // stack size unspecified; use default
        if (always_copy_stacks) {
            t->copy_stack = 1;
            t->bufsz = 0;
        }
        else {
            t->bufsz = JL_STACK_SIZE;
        }
    }
    else {
        // user requested dedicated stack of a certain size
        if (ssize < MINSTKSZ)
            ssize = MINSTKSZ;
        t->bufsz = ssize;
        t->stkbuf = jl_alloc_fiber(&t->ctx, &t->bufsz, t);
        if (t->stkbuf == NULL)
            jl_throw(jl_memory_exception);
    }
    t->next = jl_nothing;
    t->queue = jl_nothing;
    t->tls = jl_nothing;
    t->state = runnable_sym;
    t->start = start;
    t->result = jl_nothing;
    t->donenotify = completion_future;
    t->exception = jl_nothing;
    t->backtrace = jl_nothing;
    // Inherit logger state from parent task
    t->logstate = ptls->current_task->logstate;
    // there is no active exception handler available on this stack yet
    t->eh = NULL;
    t->sticky = 1;
    t->gcstack = NULL;
    t->excstack = NULL;
    t->stkbuf = NULL;
    t->started = 0;
    t->prio = -1;
    t->tid = -1;
#ifdef ENABLE_TIMINGS
    t->timing_stack = NULL;
#endif
#ifdef JULIA_ENABLE_THREADING
    arraylist_new(&t->locks, 0);
#endif

#if defined(JL_DEBUG_BUILD)
    if (!t->copy_stack)
        memset(&t->ctx, 0, sizeof(t->ctx));
#endif
#ifdef COPY_STACKS
    if (t->copy_stack)
        memcpy(&t->ctx, &ptls->base_ctx, sizeof(t->ctx));
#endif
    return t;
}

JL_DLLEXPORT jl_value_t *jl_get_current_task(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return (jl_value_t*)ptls->current_task;
}

// Do one-time initializations for task system
void jl_init_tasks(void) JL_GC_DISABLED
{
    done_sym = jl_symbol("done");
    failed_sym = jl_symbol("failed");
    runnable_sym = jl_symbol("runnable");
}

static void NOINLINE JL_NORETURN start_task(void)
{
    // this runs the first time we switch to a task
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *t = ptls->current_task;
    jl_value_t *res;

#ifdef MIGRATE_TASKS
    jl_task_t *pt = ptls->previous_task;
    if (!pt->sticky && !pt->copy_stack)
        pt->tid = -1;
#endif

    t->started = 1;
    if (t->exception != jl_nothing) {
        record_backtrace(ptls);
        jl_push_excstack(&t->excstack, t->exception,
                          ptls->bt_data, ptls->bt_size);
        res = t->exception;
    }
    else {
        JL_TRY {
            if (ptls->defer_signal) {
                ptls->defer_signal = 0;
                jl_sigint_safepoint(ptls);
            }
            JL_TIMING(ROOT);
            ptls->world_age = jl_world_counter;
            res = jl_apply(&t->start, 1);
        }
        JL_CATCH {
            res = jl_current_exception();
            t->exception = res;
            jl_gc_wb(t, res);
            goto skip_pop_exception;
        }
skip_pop_exception:;
    }
    jl_finish_task(t, res);
    gc_debug_critical_error();
    abort();
}


#if defined(JL_HAVE_UCONTEXT)
#ifdef _OS_WINDOWS_
#define setcontext jl_setcontext
#define getcontext jl_getcontext
#define swapcontext jl_swapcontext
#define makecontext jl_makecontext
#endif
static char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner) JL_NOTSAFEPOINT
{
#ifndef _OS_WINDOWS_
    int r = getcontext(t);
    if (r != 0)
        jl_error("getcontext failed");
#endif
    void *stk = jl_malloc_stack(ssize, owner);
    if (stk == NULL)
        return NULL;
    t->uc_stack.ss_sp = stk;
    t->uc_stack.ss_size = *ssize;
#ifdef _OS_WINDOWS_
    makecontext(t, &start_task);
#else
    t->uc_link = NULL;
    makecontext(t, &start_task, 0);
#endif
    return (char*)stk;
}
static void jl_start_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (lastt)
        swapcontext(lastt, t);
    else
        setcontext(t);
}
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    swapcontext(lastt, t);
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    setcontext(t);
}
static void jl_init_basefiber(size_t ssize)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    char *stkbuf = jl_alloc_fiber(&ptls->base_ctx, &ssize, NULL);
    ptls->stackbase = stkbuf + ssize;
    ptls->stacksize = ssize;
}
#endif

#if defined(JL_HAVE_UNW_CONTEXT)
static void start_basefiber(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_setjmp(ptls->base_ctx.uc_mcontext, 0))
        start_task();
    jl_longjmp(jl_root_task->ctx.uc_mcontext, 1);
}
#if defined(_CPU_X86_) || defined(_CPU_X86_64_)
#define PUSH_RET(ctx, stk) \
    do { \
        stk -= sizeof(uintptr_t); \
        *(uintptr_t*)stk = 0; /* push null RIP/EIP onto the stack */ \
    } while (0)
#elif defined(_CPU_ARM_)
#define PUSH_RET(ctx, stk) \
    unw_set_reg(ctx, UNW_ARM_R14, 0) /* put NULL into the LR */
#else
#error please define how to simulate a CALL on this platform
#endif
static char *jl_alloc_fiber(unw_context_t *t, size_t *ssize, jl_task_t *owner)
{
    char *stkbuf = (char*)jl_malloc_stack(ssize, owner);
    if (stkbuf == NULL)
        return NULL;
    char *stk = stkbuf;
    stk += *ssize;
    PUSH_RET(&jl_basecursor, stk);
    if (unw_set_reg(&jl_basecursor, UNW_REG_SP, (uintptr_t)stk) != 0) {
        jl_free_stack((void*)stkbuf, *ssize);
        jl_error("unw_set_reg UNW_REG_SP failed");
    }
    uintptr_t fn;
    if (t == &ptls->base_ctx)
        fn = (uintptr_t)&start_basefiber;
    else
        fn = (uintptr_t)&start_task;
    if (unw_set_reg(&jl_basecursor, UNW_REG_IP, fn) != 0) {
        jl_free_stack((void*)stkbuf, *ssize);
        jl_error("unw_set_reg UNW_REG_IP failed");
    }
    return stkbuf;
}
static void jl_start_fiber(unw_context_t *lastt, unw_context_t *t)
{
    if (lastt && jl_setjmp(lastt->uc_mcontext, 0))
        return;
    unw_resume(&jl_basecursor); // (doesn't return)
}
static void jl_swap_fiber(unw_context_t *lastt, unw_context_t *t)
{
    if (jl_setjmp(lastt->uc_mcontext, 0))
        return;
    jl_longjmp(t->uc_mcontext, 1); // (doesn't return)
}
static void jl_set_fiber(unw_context_t *t)
{
    jl_longjmp(t->uc_mcontext, 1);
}
static void jl_init_basefiber(size_t ssize)
{
    int r = unw_getcontext(&ptls->base_ctx);
    if (r != 0)
        jl_error("unw_getcontext failed");
    r = unw_init_local(&jl_basecursor, &ptls->base_ctx);
    if (r != 0)
        jl_error("unw_init_local failed");
#ifdef COPY_STACKS
    jl_ptls_t ptls = jl_get_ptls_states();
    char *stkbuf = jl_alloc_fiber(&ptls->base_ctx, &ssize, NULL);
    ptls->stackbase = stkbuf + ssize;
    ptls->stacksize = ssize;
    jl_start_fiber(jl_root_task, &ptls->base_ctx); // finishes initializing jl_basectx
#endif
}
#endif

#if defined(JL_HAVE_ASM)
static char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner)
{
    char *stkbuf = (char*)jl_malloc_stack(ssize, owner);
    if (stkbuf == NULL)
        return NULL;
#ifndef __clang_analyzer__
    ((char**)t)[0] = stkbuf; // stash the stack pointer somewhere for start_fiber
    ((size_t*)t)[1] = *ssize; // stash the stack size somewhere for start_fiber
#endif
    return stkbuf;
}
static void jl_start_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (lastt && jl_setjmp(lastt->uc_mcontext, 0))
        return;
    char *stk = ((char**)t)[0];
    size_t ssize = ((size_t*)t)[1];
    uintptr_t fn = (uintptr_t)&start_task;
    stk += ssize;
#ifdef _CPU_X86_64_
    asm volatile (
        " movq %0, %%rsp;\n"
        " movq %1, %%rax;\n"
        " xorq %%rbp, %%rbp;\n"
        " push %%rbp;\n" // instead of RSP
        " jmpq *%%rax;\n" // call `fn` with fake stack frame
        " ud2"
        : : "r"(stk), "r"(fn) : "memory" );
#elif defined(_CPU_X86_)
    asm volatile (
        " movl %0, %%esp;\n"
        " movl %1, %%eax;\n"
        " xorl %%ebp, %%ebp;\n"
        " push %%ebp;\n" // instead of ESP
        " jmpl *%%eax;\n" // call `fn` with fake stack frame
        " ud2"
        : : "r"(stk), "r"(fn) : "memory" );
#elif defined(_CPU_AARCH64_)
    asm volatile(
        " mov sp, %0;\n"
        " mov x29, xzr;\n" // Clear link register (x29) and frame pointer
        " mov x30, xzr;\n" // (x30) to terminate unwinder.
        " br %1;\n" // call `fn` with fake stack frame
        " brk #0x1" // abort
        : : "r" (stk), "r"(fn) : "memory" );
#elif defined(_CPU_ARM_)
    // A "i" constraint on `&start_task` works only on clang and not on GCC.
    asm(" mov sp, %0;\n"
        " mov lr, #0;\n" // Clear link register (lr) and frame pointer
        " mov fp, #0;\n" // (fp) to terminate unwinder.
        " bx %1;\n" // call `fn` with fake stack frame.  While `bx` can change
                    // the processor mode to thumb, this will never happen
                    // because all our addresses are word-aligned.
        " udf #0" // abort
        : : "r" (stk), "r"(fn) : "memory" );
#elif defined(_CPU_PPC64_)
    // N.B.: There is two iterations of the PPC64 ABI.
    // v2 is current and used here. Make sure you have the
    // correct version of the ABI reference when working on this code.
    asm volatile(
        // Move stack (-0x30 for initial stack frame) to stack pointer
        " addi 1, %0, -0x30;\n"
        // Build stack frame
        // Skip local variable save area
        " std 2, 0x28(1);\n" // Save TOC
        // Clear link editor/compiler words
        " std 0, 0x20(1);\n"
        " std 0, 0x18(1);\n"
        // Clear LR/CR save area
        " std 0, 0x10(1);\n"
        " std 0, 0x8(1);\n"
        " std 0, 0x0(1); \n" // Clear back link to terminate unwinder
        " mtlr 0; \n"        // Clear link register
        " mr 12, %1; \n"     // Set up target global entry point
        " mtctr 12; \n"      // Move jump target to counter register
        " bctr; \n"          // branch to counter (lr update disabled)
        " trap; \n"
        : : "r"(stk), "r"(fn) : "memory");
#else
#error JL_HAVE_ASM defined but not implemented for this CPU type
#endif
    __builtin_unreachable();
}
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (jl_setjmp(lastt->uc_mcontext, 0))
        return;
    jl_longjmp(t->uc_mcontext, 1); // (doesn't return)
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    jl_longjmp(t->uc_mcontext, 1);
}
static void jl_init_basefiber(size_t ssize)
{
#ifdef COPY_STACKS
    jl_ptls_t ptls = jl_get_ptls_states();
    char *stkbuf = jl_alloc_fiber(&ptls->base_ctx, &ssize, NULL);
    ptls->stackbase = stkbuf + ssize;
    ptls->stacksize = ssize;
#endif
}
#endif

#if defined(JL_HAVE_SIGALTSTACK)
static void start_basefiber(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_setjmp(ptls->base_ctx.uc_mcontext, 0))
        start_task();
}
static char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner)
{
    stack_t uc_stack, osigstk;
    struct sigaction sa, osa;
    sigset_t set, oset;
    void *stk = jl_malloc_stack(ssize, owner);
    if (stk == NULL)
        return NULL;
    // setup
    jl_ucontext_t base_ctx;
    memcpy(&base_ctx, &ptls->base_ctx, sizeof(ptls->base_ctx));
    sigfillset(&set);
    if (sigprocmask(SIG_BLOCK, &set, &oset) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigprocmask failed");
    }
    uc_stack.ss_sp = stk;
    uc_stack.ss_size = *ssize;
    uc_stack.ss_flags = 0;
    if (sigaltstack(&uc_stack, &osigstk) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaltstack failed");
    }
    memset(&sa, 0, sizeof(sa));
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = start_basefiber;
    sa.sa_flags = SA_ONSTACK;
    if (sigaction(SIGUSR2, &sa, &osa) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaction failed");
    }
    // emit signal
    pthread_kill(pthread_self(), SIGUSR2); // initializes jl_basectx
    sigdelset(&set, SIGUSR2);
    sigsuspend(&set);
    // cleanup
    if (sigaction(SIGUSR2, &osa, NULL) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaction failed");
    }
    if (osigstk.ss_size < MINSTKSZ && (osigstk.ss_flags | SS_DISABLE))
       osigstk.ss_size = MINSTKSZ;
    if (sigaltstack(&osigstk, NULL) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaltstack failed");
    }
    if (sigprocmask(SIG_SETMASK, &oset, NULL) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigprocmask failed");
    }
    memcpy(&t, &ptls->base_ctx, sizeof(ptls->base_ctx));
    memcpy(&ptls->base_ctx, &base_ctx, sizeof(ptls->base_ctx));
    return (char*)stk;
}
static void jl_start_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (lastt && jl_setjmp(lastt->uc_mcontext, 0))
        return;
    jl_longjmp(t->uc_mcontext, 1); // (doesn't return)
}
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (jl_setjmp(lastt->uc_mcontext, 0))
        return;
    jl_longjmp(t->uc_mcontext, 1); // (doesn't return)
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    jl_longjmp(t->uc_mcontext, 1);
}
static void jl_init_basefiber(size_t ssize)
{
#ifdef COPY_STACKS
    jl_ptls_t ptls = jl_get_ptls_states();
    char *stkbuf = jl_alloc_fiber(jl_root_task, &ssize, NULL);
    ptls->stackbase = stkbuf + ssize;
    ptls->stacksize = ssize;
    memcpy(&ptls->base_ctx, &jl_root_task->ctx, sizeof(ptls->base_ctx));
#endif
}
#endif

// Initialize a root task using the given stack.
void jl_init_root_task(void *stack_lo, void *stack_hi)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->root_task == NULL) {
        ptls->root_task = (jl_task_t*)jl_gc_alloc(ptls, sizeof(jl_task_t), jl_task_type);
        memset(ptls->root_task, 0, sizeof(jl_task_t));
        ptls->root_task->tls = jl_nothing;
    }
    ptls->current_task = ptls->root_task;
    void *stack = stack_lo;
    size_t ssize = (char*)stack_hi - (char*)stack_lo;
#ifndef _OS_WINDOWS_
    if (ptls->tid == 0) {
        stack = (void*)((char*)stack - ROOT_TASK_STACK_ADJUSTMENT); // offset our guess of the address of the bottom of stack to cover the guard pages too
        ssize += ROOT_TASK_STACK_ADJUSTMENT; // sizeof stack is known exactly, but not where we are in that stack
    }
#endif
    if (always_copy_stacks) {
        ptls->current_task->copy_stack = 1;
        ptls->current_task->stkbuf = NULL;
        ptls->current_task->bufsz = 0;
    }
    else {
        ptls->current_task->copy_stack = 0;
        ptls->current_task->stkbuf = stack;
        ptls->current_task->bufsz = ssize;
    }
    ptls->current_task->started = 1;
    ptls->current_task->next = jl_nothing;
    ptls->current_task->queue = jl_nothing;
    ptls->current_task->state = runnable_sym;
    ptls->current_task->start = NULL;
    ptls->current_task->result = jl_nothing;
    ptls->current_task->donenotify = jl_nothing;
    ptls->current_task->exception = jl_nothing;
    ptls->current_task->backtrace = jl_nothing;
    ptls->current_task->logstate = jl_nothing;
    ptls->current_task->eh = NULL;
    ptls->current_task->gcstack = NULL;
    ptls->current_task->excstack = NULL;
    ptls->current_task->tid = ptls->tid;
    ptls->current_task->sticky = 1;
#ifdef JULIA_ENABLE_THREADING
    arraylist_new(&ptls->current_task->locks, 0);
#endif

    if (always_copy_stacks) {
        ptls->stackbase = stack_hi;
        ptls->stacksize = ssize;
        if (jl_setjmp(ptls->base_ctx.uc_mcontext, 0))
            start_task();
    }
    else {
        jl_init_basefiber(JL_STACK_SIZE);
    }
}

JL_DLLEXPORT int jl_is_task_started(jl_task_t *t)
{
    return t->started;
}

JL_DLLEXPORT int16_t jl_get_task_tid(jl_task_t *t)
{
    return t->tid;
}


#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_)
extern DWORD32 __readgsdword(int);
extern DWORD32 __readgs(void);
#endif
JL_DLLEXPORT void jl_gdb_dump_threadinfo(void)
{
#if defined(_CPU_X86_64_)
    DWORD64 gs0 = __readgsqword(0x0);
    DWORD64 gs8 = __readgsqword(0x8);
    DWORD64 gs16 = __readgsqword(0x10);
    jl_safe_printf("ThreadId: %u, Stack: %p -- %p to %p, SEH: %p\n",
                   (unsigned)GetCurrentThreadId(),
                   jl_get_frame_addr(),
                   (void*)gs8, (void*)gs16, (void*)gs0);
#elif defined(_CPU_X86_)
    DWORD32 fs0 = __readfsdword(0x0);
    DWORD32 fs4 = __readfsdword(0x4);
    DWORD32 fs8 = __readfsdword(0x8);
    jl_safe_printf("ThreadId: %u, Stack: %p -- %p to %p, SEH: %p\n",
                   (unsigned)GetCurrentThreadId(),
                   jl_get_frame_addr(),
                   (void*)fs4, (void*)fs8, (void*)fs0);
    if (__readgs()) { // WoW64 if GS is non-zero
        DWORD32 gs0 = __readgsdword(0x0);
        DWORD32 gs4 = __readgsdword(0x4);
        DWORD32 gs8 = __readgsdword(0x8);
        DWORD32 gs12 = __readgsdword(0xc);
        DWORD32 gs16 = __readgsdword(0x10);
        DWORD32 gs20 = __readgsdword(0x14);
        jl_safe_printf("Stack64: %p%p to %p%p, SEH64: %p%p\n",
                       (void*)gs12, (void*)gs8,
                       (void*)gs20, (void*)gs16,
                       (void*)gs4, (void*)gs0);
    }
#else
    jl_safe_printf("ThreadId: %u, Stack: %p\n",
                   (unsigned)GetCurrentThreadId(),
                   jl_get_frame_addr());
#endif
}
#endif

#ifdef __cplusplus
}
#endif
