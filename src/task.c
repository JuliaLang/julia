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

// empirically, finish_task needs about 64k stack space to infer/run
// and additionally, gc-stack reserves 64k for the guard pages
#if defined(MINSIGSTKSZ) && MINSIGSTKSZ > 131072
#define MINSTKSZ MINSIGSTKSZ
#else
#define MINSTKSZ 131072
#endif

static jl_sym_t *done_sym;
static jl_sym_t *failed_sym;
static jl_sym_t *runnable_sym;

extern size_t jl_page_size;
jl_datatype_t *jl_task_type;
static char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner);
static void jl_set_fiber(jl_ucontext_t *t);
static void jl_start_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t);
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t);

#ifdef JL_HAVE_UNW_CONTEXT
static JL_THREAD unw_cursor_t jl_basecursor;
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
    memcpy_a16((uint64_t*)buf, (uint64_t*)frame_addr, nb);
    // this task's stack could have been modified after
    // it was marked by an incremental collection
    // move the barrier back instead of walking it again here
    jl_gc_wb_back(lastt);
}

static void NOINLINE JL_NORETURN restore_stack(jl_ptls_t ptls, char *p)
{
    jl_task_t *t = ptls->current_task;
    size_t nb = t->copy_stack;
    char *_x = (char*)ptls->stackbase - nb;
    if (!p) {
        // switch to a stackframe that's beyond the bounds of the last switch
        p = _x;
        if ((char*)&_x > _x) {
            p = (char*)alloca((char*)&_x - _x);
        }
        restore_stack(ptls, p); // pass p to ensure the compiler can't tailcall this or avoid the alloca
    }
    assert(t->stkbuf != NULL);
    memcpy_a16((uint64_t*)_x, (uint64_t*)t->stkbuf, nb); // destroys all but the current stackframe
    jl_set_fiber(&t->ctx);
    abort(); // unreachable
}
static void restore_stack2(jl_ptls_t ptls, jl_task_t *lastt)
{
    jl_task_t *t = ptls->current_task;
    size_t nb = t->copy_stack;
    char *_x = (char*)ptls->stackbase - nb;
    assert(t->stkbuf != NULL);
    memcpy_a16((uint64_t*)_x, (uint64_t*)t->stkbuf, nb); // destroys all but the current stackframe
    jl_swap_fiber(&lastt->ctx, &t->ctx);
}
#endif

static jl_function_t *task_done_hook_func = NULL;

static void JL_NORETURN finish_task(jl_task_t *t, jl_value_t *resultval JL_MAYBE_UNROOTED)
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
    if (ptls->tid != 0) {
        // For now, only thread 0 runs the task scheduler.
        // The others return to the thread loop
        ptls->root_task->result = jl_nothing;
        jl_task_t *task = ptls->root_task;
        jl_switchto(&task);
        gc_debug_critical_error();
        abort();
    }
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
            jl_no_exc_handler(jl_exception_in_transit);
        }
    }
    gc_debug_critical_error();
    abort();
}

static void record_backtrace(void) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_get_ptls_states();
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
#ifdef ENABLE_TIMINGS
    jl_timing_block_t *blk = lastt->timing_stack;
    if (blk)
        jl_timing_block_stop(blk);
#endif
    // backtraces don't survive task switches, see e.g. issue #12485
    ptls->bt_size = 0;
#ifdef JULIA_ENABLE_THREADING
    // If the current task is not holding any locks, free the locks list
    // so that it can be GC'd without leaking memory
    arraylist_t *locks = &lastt->locks;
    if (locks->len == 0 && locks->items != locks->_space) {
        arraylist_free(locks);
        arraylist_new(locks, 0);
    }
#endif

    int started = (t->stkbuf != NULL);
    int killed = (lastt->state == done_sym || lastt->state == failed_sym);
    if (!started && !t->copy_stack) {
        // may need to allocate the stack
        t->stkbuf = jl_alloc_fiber(&t->ctx, &t->bufsz, t);
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
#ifdef ENABLE_TIMINGS
                assert(blk == ptls->current_task->timing_stack);
                if (blk)
                    jl_timing_block_start(blk);
#endif
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

    // DEPRECATED:
    // restore task's current module, looking at parent tasks
    // if it hasn't set one.
    jl_task_t *last = t;
    while (last->current_module == NULL && last != last->parent)
        last = last->parent;
    if (last->current_module != NULL)
        jl_current_module = last->current_module;

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
                restore_stack2(ptls, lastt);
            else
                restore_stack(ptls, NULL); // (doesn't return)
        }
        else
#endif
        if (!lastt_ctx)
            jl_set_fiber(&t->ctx);
        else
            jl_swap_fiber(lastt_ctx, &t->ctx);
    }
    else {
        jl_start_fiber(lastt_ctx, &t->ctx);
    }
#ifdef ENABLE_TIMINGS
    assert(blk == ptls->current_task->timing_stack);
    if (blk)
        jl_timing_block_start(blk);
#endif
}

JL_DLLEXPORT void jl_switchto(jl_task_t **pt)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *t = *pt;
    if (t == ptls->current_task) {
        return;
    }
    if (t->state == done_sym || t->state == failed_sym ||
            (t->started && t->stkbuf == NULL)) {
        ptls->current_task->exception = t->exception;
        ptls->current_task->result = t->result;
        return;
    }
    if (ptls->in_finalizer)
        jl_error("task switch not allowed from inside gc finalizer");
    if (ptls->in_pure_callback)
        jl_error("task switch not allowed from inside staged nor pure functions");
    sig_atomic_t defer_signal = ptls->defer_signal;
    int8_t gc_state = jl_gc_unsafe_enter(ptls);
    ctx_switch(ptls, pt);
    jl_gc_unsafe_leave(ptls, gc_state);
    sig_atomic_t other_defer_signal = ptls->defer_signal;
    ptls->defer_signal = defer_signal;
    if (other_defer_signal && !defer_signal)
        jl_sigint_safepoint(ptls);
}

JL_DLLEXPORT JL_NORETURN void jl_no_exc_handler(jl_value_t *e) JL_NOTSAFEPOINT
{
    jl_printf(JL_STDERR, "fatal: error thrown and no exception handler available.\n");
    jl_static_show(JL_STDERR, e);
    jl_printf(JL_STDERR, "\n");
    jlbacktrace();
    jl_exit(1);
}

jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block);

// yield to exception handler
void JL_NORETURN throw_internal(jl_value_t *e JL_MAYBE_UNROOTED)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ptls->io_wait = 0;
    if (ptls->safe_restore)
        jl_longjmp(*ptls->safe_restore, 1);
    assert(e != NULL);
    ptls->exception_in_transit = e;
    jl_gc_unsafe_enter(ptls);
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
        jl_no_exc_handler(e);
    }
    assert(0);
}

// record backtrace and raise an error
JL_DLLEXPORT void jl_throw(jl_value_t *e)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(e != NULL);
    if (!ptls->safe_restore)
        record_backtrace();
    throw_internal(e);
}

JL_DLLEXPORT void jl_rethrow(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    throw_internal(ptls->exception_in_transit);
}

JL_DLLEXPORT void jl_rethrow_other(jl_value_t *e)
{
    throw_internal(e);
}

JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t *start, size_t ssize)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *t = (jl_task_t*)jl_gc_alloc(ptls, sizeof(jl_task_t), jl_task_type);
    t->copy_stack = 0;
    if (ssize == 0) {
#ifdef COPY_STACKS
        t->copy_stack = 1;
        t->bufsz = 0;
#else
        t->bufsz = JL_STACK_SIZE; // unspecified -- use the default size
#endif
    }
    else {
        if (ssize < MINSTKSZ)
            ssize = MINSTKSZ;
        t->bufsz = ssize;
    }
    t->current_module = NULL;
    t->parent = ptls->current_task;
    t->tls = jl_nothing;
    t->state = runnable_sym;
    t->start = start;
    t->result = jl_nothing;
    t->donenotify = jl_nothing;
    t->exception = jl_nothing;
    t->backtrace = jl_nothing;
    // Inherit logger state from parent task
    t->logstate = ptls->current_task->logstate;
    // there is no active exception handler available on this stack yet
    t->eh = NULL;
    t->tid = 0;
    t->gcstack = NULL;
    t->stkbuf = NULL;
    t->tid = 0;
    t->started = 0;
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
    jl_task_type = (jl_datatype_t*)
        jl_new_datatype(jl_symbol("Task"),
                        NULL,
                        jl_any_type,
                        jl_emptysvec,
                        jl_perm_symsvec(9,
                                        "parent",
                                        "storage",
                                        "state",
                                        "donenotify",
                                        "result",
                                        "exception",
                                        "backtrace",
                                        "logstate",
                                        "code"),
                        jl_svec(9,
                                jl_any_type,
                                jl_any_type,
                                jl_sym_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type,
                                jl_any_type),
                        0, 1, 8);
    jl_svecset(jl_task_type->types, 0, (jl_value_t*)jl_task_type);

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
    t->started = 1;
    if (t->exception != jl_nothing) {
        record_backtrace();
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
            res = jl_exception_in_transit;
            t->exception = res;
            jl_gc_wb(t, res);
        }
    }
    finish_task(t, res);
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
static char *jl_alloc_fiber(jl_ucontext_t *t, size_t *ssize, jl_task_t *owner)
{
#ifndef _OS_WINDOWS_
    int r = getcontext(t);
    if (r != 0)
        jl_error("getcontext failed");
#endif
    void *stk = jl_malloc_stack(ssize, owner);
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
    ((char**)t)[0] = stkbuf; // stash the stack pointer somewhere for start_fiber
    ((size_t*)t)[1] = *ssize; // stash the stack size somewhere for start_fiber
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
        " br %1;\n" // call `fn` with fake stack frame
        " udf #0" // abort
        : : "r" (stk), "r"(fn) : "memory" );
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
    ptls->current_task = (jl_task_t*)jl_gc_alloc(ptls, sizeof(jl_task_t),
                                                 jl_task_type);
    ptls->current_task->copy_stack = 0;
    void *stack = stack_lo;
    size_t ssize = (char*)stack_hi - (char*)stack_lo;
#ifndef _OS_WINDOWS_
    if (ptls->tid == 0) {
        stack = (void*)((char*)stack - 3000000); // offset our guess of the address of the bottom of stack to cover the guard pages too
        ssize += 3000000; // sizeof stack is known exactly, but not where we are in that stack
    }
#endif
    ptls->current_task->stkbuf = stack;
    ptls->current_task->bufsz = ssize;
    ptls->current_task->started = 1;
    ptls->current_task->parent = ptls->current_task;
    ptls->current_task->current_module = ptls->current_module;
    ptls->current_task->tls = jl_nothing;
    ptls->current_task->state = runnable_sym;
    ptls->current_task->start = NULL;
    ptls->current_task->result = jl_nothing;
    ptls->current_task->donenotify = jl_nothing;
    ptls->current_task->exception = jl_nothing;
    ptls->current_task->backtrace = jl_nothing;
    ptls->current_task->logstate = jl_nothing;
    ptls->current_task->eh = NULL;
    ptls->current_task->gcstack = NULL;
    ptls->current_task->tid = ptls->tid;
#ifdef JULIA_ENABLE_THREADING
    arraylist_new(&ptls->current_task->locks, 0);
#endif

    ptls->exception_in_transit = (jl_value_t*)jl_nothing;
    ptls->root_task = ptls->current_task;

    jl_init_basefiber(JL_STACK_SIZE);
}

JL_DLLEXPORT int jl_is_task_started(jl_task_t *t)
{
    return t->started;
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
