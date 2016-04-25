// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  task.c
  lightweight processes (symmetric coroutines)
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <errno.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_)
#include <winbase.h>
#include <malloc.h>
volatile int jl_in_stackwalk = 0;
#else
#include <unistd.h>
#include <sys/mman.h> // for mprotect
#include <dlfcn.h>   // for dladdr
#endif

/* This probing code is derived from Douglas Jones' user thread library */

/* true if stack grows up, false if down */
static int _stack_grows_up;

/* the offset of the beginning of the stack frame in a function */
static size_t _frame_offset;

struct _probe_data {
    intptr_t low_bound;         /* below probe on stack */
    intptr_t probe_local;       /* local to probe on stack */
    intptr_t high_bound;        /* above probe on stack */
    intptr_t prior_local;       /* value of probe_local from earlier call */

    jl_jmp_buf probe_env;       /* saved environment of probe */
    jl_jmp_buf probe_sameAR;    /* second environment saved by same call */
    jl_jmp_buf probe_samePC;    /* environment saved on previous call */

    jl_jmp_buf * ref_probe;     /* switches between probes */
};

static void boundhigh(struct _probe_data *p)
{
    int c;
    p->high_bound = (intptr_t)&c;
}

static void probe(struct _probe_data *p)
{
    p->prior_local = p->probe_local;
    p->probe_local = (intptr_t)&p;
    jl_setjmp( *(p->ref_probe), 0 );
    p->ref_probe = &p->probe_env;
    jl_setjmp( p->probe_sameAR, 0 );
    boundhigh(p);
}

static void boundlow(struct _probe_data *p)
{
    p->low_bound = (intptr_t)&p;
    probe(p);
}

// we need this function to exist so we can measure its stack frame!
static void NOINLINE_DECL(fill(struct _probe_data *p));

static void fill(struct _probe_data *p)
{
    boundlow(p);
}

static void _infer_direction_from(int *first_addr)
{
    int second;
    _stack_grows_up = (first_addr < &second);
}

static void _infer_stack_direction(void)
{
    int first;
    _infer_direction_from(&first);
}

static int mangle_pointers;

static void _probe_arch(void)
{
    struct _probe_data p;
    memset(p.probe_env, 0, sizeof(jl_jmp_buf));
    memset(p.probe_sameAR, 0, sizeof(jl_jmp_buf));
    memset(p.probe_samePC, 0, sizeof(jl_jmp_buf));
    p.ref_probe = &p.probe_samePC;

    _infer_stack_direction();

    /* do a probe with filler on stack */
    fill(&p);
    /* do a probe without filler */
    boundlow(&p);

#if defined(__linux__) && defined(__i386__)
    char **s = (char**)p.ref_probe;
    mangle_pointers = !(s[4] > jl_stack_lo &&
                        s[4] < jl_stack_hi);
#elif defined(__linux__) && defined(__x86_64__)
    char **s = (char**)p.ref_probe;
    mangle_pointers = !(s[6] > jl_stack_lo &&
                        s[6] < jl_stack_hi);
#else
    mangle_pointers = 0;
#endif

    intptr_t prior_diff = p.probe_local - p.prior_local;
    _frame_offset = labs(prior_diff);
}

/* end probing code */

static jl_sym_t *done_sym;
static jl_sym_t *failed_sym;
static jl_sym_t *runnable_sym;

extern size_t jl_page_size;
jl_datatype_t *jl_task_type;
#define jl_root_task (jl_get_ptls_states()->root_task)

#ifdef COPY_STACKS
#define jl_jmp_target (jl_get_ptls_states()->jmp_target)

#if (defined(_CPU_X86_64_) || defined(_CPU_X86_)) && !defined(_COMPILER_MICROSOFT_)
#define ASM_COPY_STACKS
#endif

#ifndef ASM_COPY_STACKS
#define jl_base_ctx (jl_get_ptls_states()->base_ctx)
#endif

static void NOINLINE save_stack(jl_task_t *t)
{
    if (t->state == done_sym || t->state == failed_sym)
        return;
    volatile char *_x;
    size_t nb = (char*)jl_stackbase - (char*)&_x;
    char *buf;
    if (t->stkbuf == NULL || t->bufsz < nb) {
        buf = (char*)allocb(nb);
        t->stkbuf = buf;
        t->bufsz = nb;
    }
    else {
        buf = (char*)t->stkbuf;
    }
    t->ssize = nb;
    memcpy(buf, (char*)&_x, nb);
    // this task's stack could have been modified after
    // it was marked by an incremental collection
    // move the barrier back instead of walking it again here
    jl_gc_wb_back(t);
}

static void NOINLINE restore_stack(jl_task_t *t, jl_jmp_buf *where, char *p)
{
    char *_x = (char*)jl_stackbase - t->ssize;
    if (!p) {
        p = _x;
        if ((char*)&_x > _x) {
            p = (char*)alloca((char*)&_x - _x);
        }
        restore_stack(t, where, p);
    }
    jl_jmp_target = where;
    assert(t->stkbuf != NULL);
    memcpy(_x, t->stkbuf, t->ssize);
    jl_longjmp(*jl_jmp_target, 1);
}
#endif

static jl_function_t *task_done_hook_func=NULL;

static void JL_NORETURN finish_task(jl_task_t *t, jl_value_t *resultval)
{
    if (t->exception != jl_nothing)
        t->state = failed_sym;
    else
        t->state = done_sym;
    t->result = resultval;
    jl_gc_wb(t, t->result);
    // TODO: early free of t->stkbuf
#ifdef COPY_STACKS
    t->stkbuf = (void*)(intptr_t)-1;
#endif
    if (ti_tid != 0) {
        // For now, only thread 0 runs the task scheduler.
        // The others return to the thread loop
        jl_switchto(jl_root_task, jl_nothing);
        gc_debug_critical_error();
        abort();
    }
    if (task_done_hook_func == NULL) {
        task_done_hook_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                            jl_symbol("task_done_hook"));
    }
    if (task_done_hook_func != NULL) {
        jl_value_t *args[2] = {task_done_hook_func, (jl_value_t*)t};
        jl_apply(args, 2);
    }
    gc_debug_critical_error();
    abort();
}

static void throw_if_exception_set(jl_task_t *t)
{
    if (t->exception != NULL && t->exception != jl_nothing) {
        jl_value_t *exc = t->exception;
        t->exception = jl_nothing;
        jl_throw(exc);
    }
}

static void record_backtrace(void)
{
    jl_bt_size = rec_backtrace(jl_bt_data, JL_MAX_BT_SIZE);
}

static void NOINLINE JL_NORETURN start_task(void)
{
    // this runs the first time we switch to a task
    jl_task_t *t = jl_current_task;
    jl_value_t *res;
    t->started = 1;
    if (t->exception != NULL && t->exception != jl_nothing) {
        record_backtrace();
        res = t->exception;
    }
    else {
        JL_TRY {
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

#ifdef COPY_STACKS
void NOINLINE jl_set_base_ctx(char *__stk)
{
    jl_stackbase = (char*)(((uintptr_t)__stk + sizeof(*__stk))&-16); // also ensures stackbase is 16-byte aligned
#ifndef ASM_COPY_STACKS
    if (jl_setjmp(jl_base_ctx, 1)) {
        start_task();
    }
#endif
}
#endif

JL_DLLEXPORT void julia_init(JL_IMAGE_SEARCH rel)
{
    // keep this function small, since we want to keep the stack frame
    // leading up to this also quite small
    _julia_init(rel);
#ifdef COPY_STACKS
    char __stk;
    jl_set_base_ctx(&__stk); // separate function, to record the size of a stack frame
#endif
}

static void ctx_switch(jl_task_t *t, jl_jmp_buf *where)
{
    if (t == jl_current_task)
        return;
    /*
      making task switching interrupt-safe is going to be challenging.
      we need JL_SIGATOMIC_BEGIN in jl_enter_handler, and then
      JL_SIGATOMIC_END after every JL_TRY sigsetjmp that returns zero.
      also protect jl_eh_restore_state.
      then we need JL_SIGATOMIC_BEGIN at the top of this function (ctx_switch).
      the JL_SIGATOMIC_END at the end of this function handles the case
      of task switching with yieldto().
      then we need to handle the case of task switching via raise().
      to do that, the top of every catch block must do JL_SIGATOMIC_END
      *IF AND ONLY IF* throwing the exception involved a task switch.
    */
    //JL_SIGATOMIC_BEGIN();
    if (!jl_setjmp(jl_current_task->ctx, 0)) {
        jl_bt_size = 0;  // backtraces don't survive task switches, see e.g. issue #12485
#ifdef COPY_STACKS
        jl_task_t *lastt = jl_current_task;
        save_stack(lastt);
#endif

        // set up global state for new task
        jl_current_task->gcstack = jl_pgcstack;
        jl_pgcstack = t->gcstack;
#ifdef JULIA_ENABLE_THREADING
        // If the current task is not holding any locks, free the locks list
        // so that it can be GC'd without leaking memory
        arraylist_t *locks = &jl_current_task->locks;
        if (locks->len == 0 && locks->items != locks->_space) {
            arraylist_free(locks);
            arraylist_new(locks, 0);
        }
#endif

        // restore task's current module, looking at parent tasks
        // if it hasn't set one.
        jl_task_t *last = t;
        while (last->current_module == NULL && last != jl_root_task) {
            last = last->parent;
        }
        if (last->current_module != NULL) {
            jl_current_module = last->current_module;
        }

        jl_current_task = t;

#ifdef COPY_STACKS
        if (t->stkbuf) {
            restore_stack(t, where, NULL);
        }
        else {
#ifdef ASM_COPY_STACKS
            void *stackbase = jl_stackbase;
#ifdef _CPU_X86_64_
#ifdef _OS_WINDOWS_
            stackbase = (char*)stackbase - 0x20;
#endif
            asm(" movq %0, %%rsp;\n"
                " xorq %%rbp, %%rbp;\n"
                " push %%rbp;\n" // instead of RSP
                " jmp %P1;\n" // call stack_task with fake stack frame
                " ud2"
                : : "r"(stackbase), "i"(&start_task) : "memory" );
#elif defined(_CPU_X86_)
            asm(" movl %0, %%esp;\n"
                " xorl %%ebp, %%ebp;\n"
                " push %%ebp;\n" // instead of ESP
                " jmp %P1;\n" // call stack_task with fake stack frame
                " ud2"
                : : "r" (stackbase), ""(&start_task) : "memory" );
#else
#error ASM_COPY_STACKS not supported on this cpu architecture
#endif
#else // ASM_COPY_STACKS
            jl_longjmp(jl_base_ctx, 1);
#endif
        }
#else
        jl_longjmp(*where, 1);
#endif
    }
    //JL_SIGATOMIC_END();
}

JL_DLLEXPORT jl_value_t *jl_switchto(jl_task_t *t, jl_value_t *arg)
{
    if (t == jl_current_task) {
        throw_if_exception_set(t);
        return arg;
    }
    if (t->state == done_sym || t->state == failed_sym ||
        (t->stkbuf == (void*)(intptr_t)-1)) {
        if (t->exception != jl_nothing)
            jl_throw(t->exception);
        return t->result;
    }
    if (jl_in_finalizer)
        jl_error("task switch not allowed from inside gc finalizer");
    if (in_pure_callback)
        jl_error("task switch not allowed from inside staged function");
    int8_t gc_state = jl_gc_unsafe_enter();
    jl_task_arg_in_transit = arg;
    ctx_switch(t, &t->ctx);
    jl_value_t *val = jl_task_arg_in_transit;
    jl_task_arg_in_transit = jl_nothing;
    throw_if_exception_set(jl_current_task);
    jl_gc_unsafe_leave(gc_state);
    return val;
}

#ifndef COPY_STACKS

#ifdef __linux__
#if defined(__i386__)
static intptr_t ptr_mangle(intptr_t p)
{
    intptr_t ret;
    asm(" movl %1, %%eax;\n"
        " xorl %%gs:0x18, %%eax;"
        " roll $9, %%eax;"
        " movl %%eax, %0;"
        : "=r"(ret) : "r"(p) : "%eax");
    return ret;
}
static intptr_t ptr_demangle(intptr_t p)
{
    intptr_t ret;
    asm(" movl %1, %%eax;\n"
        " rorl $9, %%eax;"
        " xorl %%gs:0x18, %%eax;"
        " movl %%eax, %0;"
        : "=r"(ret) : "r"(p) : "%eax" );
    return ret;
}
#elif defined(__x86_64__)
static intptr_t ptr_mangle(intptr_t p)
{
    intptr_t ret;
    asm(" movq %1, %%rax;\n"
        " xorq %%fs:0x30, %%rax;"
        " rolq $17, %%rax;"
        " movq %%rax, %0;"
        : "=r"(ret) : "r"(p) : "%rax");
    return ret;
}
static intptr_t ptr_demangle(intptr_t p)
{
    intptr_t ret;
    asm(" movq %1, %%rax;\n"
        " rorq $17, %%rax;"
        " xorq %%fs:0x30, %%rax;"
        " movq %%rax, %0;"
        : "=r"(ret) : "r"(p) : "%rax" );
    return ret;
}
#endif
#endif //__linux__

/* rebase any values in saved state to the new stack */
static void rebase_state(jl_jmp_buf *ctx, intptr_t local_sp, intptr_t new_sp)
{
    intptr_t *s = (intptr_t*)ctx;
    intptr_t diff = new_sp - local_sp; /* subtract old base, and add new base */
#if defined(__linux__) && defined(__i386__)
    s[3] += diff;
    if (mangle_pointers)
        s[4] = ptr_mangle(ptr_demangle(s[4])+diff);
    else
        s[4] += diff;
#elif defined(__linux__) && defined(__x86_64__)
    if (mangle_pointers) {
        s[1] = ptr_mangle(ptr_demangle(s[1])+diff);
        s[6] = ptr_mangle(ptr_demangle(s[6])+diff);
    }
    else {
        s[1] += diff;
        s[6] += diff;
    }
#elif defined(__APPLE__) && defined(__i386__)
    s[8] += diff;
    s[9] += diff;
#elif defined(__APPLE__) && defined(__x86_64__)
    s[1] += diff;
    s[2] += diff;
#else
#error "COPY_STACKS must be defined on this platform."
#endif
}
static void init_task(jl_task_t *t, char *stack)
{
    if (jl_setjmp(t->ctx, 0)) {
        start_task();
    }
    // this runs when the task is created
    intptr_t local_sp = (intptr_t)&t;
    intptr_t new_sp = (intptr_t)stack + t->ssize - _frame_offset;
#ifdef _P64
    // SP must be 16-byte aligned
    new_sp = new_sp&-16;
    local_sp = local_sp&-16;
#endif
    memcpy((void*)new_sp, (void*)local_sp, _frame_offset);
    rebase_state(&t->ctx, local_sp, new_sp);
}

#endif /* !COPY_STACKS */


// yield to exception handler
void JL_NORETURN throw_internal(jl_value_t *e)
{
    if (jl_safe_restore)
        jl_longjmp(*jl_safe_restore, 1);
    jl_gc_unsafe_enter();
    assert(e != NULL);
    jl_exception_in_transit = e;
    if (jl_current_task->eh != NULL) {
        jl_longjmp(jl_current_task->eh->eh_ctx, 1);
    }
    else {
        jl_printf(JL_STDERR, "fatal: error thrown and no exception handler available.\n");
        jl_static_show(JL_STDERR, e);
        jl_printf(JL_STDERR, "\n");
        jlbacktrace();
        jl_exit(1);
    }
    assert(0);
}

// record backtrace and raise an error
JL_DLLEXPORT void jl_throw(jl_value_t *e)
{
    assert(e != NULL);
    if (!jl_safe_restore)
        record_backtrace();
    throw_internal(e);
}

JL_DLLEXPORT void jl_rethrow(void)
{
    throw_internal(jl_exception_in_transit);
}

JL_DLLEXPORT void jl_rethrow_other(jl_value_t *e)
{
    throw_internal(e);
}

JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t *start, size_t ssize)
{
    size_t pagesz = jl_page_size;
    jl_task_t *t = (jl_task_t*)jl_gc_allocobj(sizeof(jl_task_t));
    jl_set_typeof(t, jl_task_type);
#ifndef COPY_STACKS
    if (ssize == 0) // unspecified -- pick some default size
        ssize = 1*1024*1024; // 1M (for now)
#endif
    ssize = LLT_ALIGN(ssize, pagesz);
    t->ssize = ssize;
    t->current_module = NULL;
    t->parent = jl_current_task;
    t->tls = jl_nothing;
    t->consumers = jl_nothing;
    t->state = runnable_sym;
    t->start = start;
    t->result = jl_nothing;
    t->donenotify = jl_nothing;
    t->exception = jl_nothing;
    t->backtrace = jl_nothing;
    // there is no active exception handler available on this stack yet
    t->eh = NULL;
    t->gcstack = NULL;
    t->stkbuf = NULL;
    t->tid = 0;
    t->started = 0;

#ifdef COPY_STACKS
    t->bufsz = 0;
#else
    JL_GC_PUSH1(&t);

    char *stk = allocb(ssize+pagesz+(pagesz-1));
    t->stkbuf = stk;
    jl_gc_wb_buf(t, t->stkbuf);
    stk = (char*)LLT_ALIGN((uintptr_t)stk, pagesz);
    // add a guard page to detect stack overflow
    if (mprotect(stk, pagesz-1, PROT_NONE) == -1)
        jl_errorf("mprotect: %s", strerror(errno));
    stk += pagesz;

    init_task(t, stk);
    //jl_gc_add_finalizer((jl_value_t*)t, jl_unprotect_stack_func);
    JL_GC_POP();
#endif

#ifdef JULIA_ENABLE_THREADING
    arraylist_new(&t->locks, 0);
#endif
    return t;
}

JL_CALLABLE(jl_unprotect_stack)
{
#ifndef COPY_STACKS
    jl_task_t *t = (jl_task_t*)args[0];
    size_t pagesz = jl_page_size;
    char *stk = (char*)LLT_ALIGN((uintptr_t)t->stkbuf, pagesz);
    // unprotect stack so it can be reallocated for something else
    mprotect(stk, pagesz - 1, PROT_READ|PROT_WRITE);
#endif
    return jl_nothing;
}

JL_DLLEXPORT jl_value_t *jl_get_current_task(void)
{
    return (jl_value_t*)jl_current_task;
}

jl_function_t *jl_unprotect_stack_func;

// Do one-time initializations for task system
void jl_init_tasks(void)
{
    _probe_arch();
    jl_task_type = jl_new_datatype(jl_symbol("Task"),
                                   jl_any_type,
                                   jl_emptysvec,
                                   jl_svec(9,
                                            jl_symbol("parent"),
                                            jl_symbol("storage"),
                                            jl_symbol("state"),
                                            jl_symbol("consumers"),
                                            jl_symbol("donenotify"),
                                            jl_symbol("result"),
                                            jl_symbol("exception"),
                                            jl_symbol("backtrace"),
                                            jl_symbol("code")),
                                   jl_svec(9,
                                            jl_any_type,
                                            jl_any_type, jl_sym_type,
                                            jl_any_type, jl_any_type,
                                            jl_any_type, jl_any_type,
                                            jl_any_type, jl_any_type),
                                   0, 1, 8);
    jl_svecset(jl_task_type->types, 0, (jl_value_t*)jl_task_type);

    done_sym = jl_symbol("done");
    failed_sym = jl_symbol("failed");
    runnable_sym = jl_symbol("runnable");

    //jl_unprotect_stack_func = jl_new_closure(jl_unprotect_stack, (jl_value_t*)jl_emptysvec, NULL);
}

// Initialize a root task using the given stack.
void jl_init_root_task(void *stack, size_t ssize)
{
    jl_current_task = (jl_task_t*)jl_gc_allocobj(sizeof(jl_task_t));
    jl_set_typeof(jl_current_task, jl_task_type);
#ifdef COPY_STACKS
    jl_current_task->ssize = 0;  // size of saved piece
    jl_current_task->bufsz = 0;
    jl_current_task->stkbuf = NULL;
#else
    jl_current_task->ssize = ssize;
    jl_current_task->stkbuf = stack;
#endif
    jl_current_task->started = 1;
    jl_current_task->parent = jl_current_task;
    jl_current_task->current_module = jl_current_module;
    jl_current_task->tls = jl_nothing;
    jl_current_task->consumers = jl_nothing;
    jl_current_task->state = runnable_sym;
    jl_current_task->start = NULL;
    jl_current_task->result = jl_nothing;
    jl_current_task->donenotify = jl_nothing;
    jl_current_task->exception = jl_nothing;
    jl_current_task->backtrace = jl_nothing;
    jl_current_task->eh = NULL;
    jl_current_task->gcstack = NULL;
    jl_current_task->tid = ti_tid;
#ifdef JULIA_ENABLE_THREADING
    arraylist_new(&jl_current_task->locks, 0);
#endif

    jl_root_task = jl_current_task;

    jl_exception_in_transit = (jl_value_t*)jl_nothing;
    jl_task_arg_in_transit = (jl_value_t*)jl_nothing;
}

JL_DLLEXPORT int jl_is_task_started(jl_task_t *t)
{
    return t->started;
}

#ifdef __cplusplus
}
#endif
