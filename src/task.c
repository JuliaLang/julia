/*
  task.c
  lightweight processes (symmetric coroutines)
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>
#include <signal.h>
#include <libgen.h>
#include <unistd.h>
#include <errno.h>
#include "julia.h"
#include "builtin_proto.h"
#if defined(__APPLE__)
#include <execinfo.h>
#elif defined(__WIN32__)
#include <Winbase.h>
#include <malloc.h>
#else
// This gives unwind only local unwinding options ==> faster code
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#endif

/* This probing code is derived from Douglas Jones' user thread library */

/* true if stack grows up, false if down */
static int _stack_grows_up;

/* the offset of the beginning of the stack frame in a function */
static size_t _frame_offset;

struct _probe_data {
    intptr_t low_bound;		/* below probe on stack */
    intptr_t probe_local;	/* local to probe on stack */
    intptr_t high_bound;	/* above probe on stack */
    intptr_t prior_local;	/* value of probe_local from earlier call */

    jl_jmp_buf probe_env;	/* saved environment of probe */
    jl_jmp_buf probe_sameAR;	/* second environment saved by same call */
    jl_jmp_buf probe_samePC;	/* environment saved on previous call */

    jl_jmp_buf * ref_probe;	/* switches between probes */
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
static void fill(struct _probe_data *p) __attribute__ ((noinline));
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
extern char *jl_stack_lo;
extern char *jl_stack_hi;

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

/*
  TODO:
  - per-task storage (scheme-like parameters)
  - stack growth
*/

extern size_t jl_page_size;
jl_struct_type_t *jl_task_type;
DLLEXPORT jl_task_t * volatile jl_current_task;
jl_task_t *jl_root_task;
jl_value_t * volatile jl_task_arg_in_transit;
static volatile int n_args_in_transit;
jl_value_t *jl_exception_in_transit;
#ifdef JL_GC_MARKSWEEP
jl_gcframe_t *jl_pgcstack = NULL;
#endif

static void start_task(jl_task_t *t);

#ifdef COPY_STACKS
jl_jmp_buf * volatile jl_jmp_target;

static void save_stack(jl_task_t *t)
{
    volatile int _x;
    size_t nb = (char*)t->stackbase - (char*)&_x;
    char *buf;
    if (t->stkbuf == NULL || t->bufsz < nb) {
        buf = allocb(nb);
        t->stkbuf = buf;
        t->bufsz = nb;
    }
    else {
        buf = t->stkbuf;
    }
    t->ssize = nb;
    memcpy(buf, (char*)&_x, nb);
}

void __attribute__((noinline)) restore_stack(jl_task_t *t, jl_jmp_buf *where, char *p)
{
    char* _x = (char*)(t->stackbase-t->ssize);
    if (!p) {
        p = _x;
        if ((char*)&_x > _x) {
            p = alloca((char*)&_x - _x);
    	}
        restore_stack(t, where, p);
    }
    jl_jmp_target = where;

    if (t->stkbuf != NULL) {
        memcpy(_x, t->stkbuf, t->ssize);
    }
    jl_longjmp(*jl_jmp_target, 1);
}

static void switch_stack(jl_task_t *t, jl_jmp_buf *where)
{
    assert(t == jl_current_task);
    if (t->stkbuf == NULL) {
        start_task(t);
        // doesn't return
    }
    else {
        restore_stack(t, where, NULL);
    }
}

void jl_switch_stack(jl_task_t *t, jl_jmp_buf *where)
{
    switch_stack(t, where);
}
#endif

static void ctx_switch(jl_task_t *t, jl_jmp_buf *where)
{
    if (t == jl_current_task)
        return;
    /*
      making task switching interrupt-safe is going to be challenging.
      we need JL_SIGATOMIC_BEGIN in jl_enter_handler, and then
      JL_SIGATOMIC_END after every JL_TRY setjmp that returns zero.
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
#ifdef COPY_STACKS
        jl_task_t *lastt = jl_current_task;
        save_stack(lastt);
#endif

        // set up global state for new task
#ifdef JL_GC_MARKSWEEP
        jl_current_task->state.gcstack = jl_pgcstack;
        jl_pgcstack = t->state.gcstack;
#endif
        t->last = jl_current_task;
        // by default, exit to first task to switch to this one
        if (t->on_exit == NULL)
            t->on_exit = jl_current_task;
        jl_current_task = t;

#ifdef COPY_STACKS
        jl_jmp_target = where;
        jl_longjmp(lastt->base_ctx, 1);
#else
        jl_longjmp(*where, 1);
#endif
    }
    //JL_SIGATOMIC_END();
}

static jl_value_t *switchto(jl_task_t *t)
{
    if (t->done) {
        jl_task_arg_in_transit = (jl_value_t*)jl_null;
        return t->result;
    }
    ctx_switch(t, &t->ctx);
    jl_value_t *val = jl_task_arg_in_transit;
    jl_task_arg_in_transit = (jl_value_t*)jl_null;
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
    ptrint_t *s = (ptrint_t*)ctx;
    ptrint_t diff = new_sp - local_sp; /* subtract old base, and add new base */
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

#endif /* !COPY_STACKS */

jl_value_t *jl_switchto(jl_task_t *t, jl_value_t *arg)
{
    jl_task_arg_in_transit = arg;
    n_args_in_transit = 1;
    return switchto(t);
}

static void finish_task(jl_task_t *t, jl_value_t *resultval)
{
    assert(t->done==0);
    t->done = 1;
    t->result = resultval;
    // TODO: early free of t->stkbuf
#ifdef COPY_STACKS
    t->stkbuf = NULL;
#endif
}

static void start_task(jl_task_t *t)
{
    // this runs the first time we switch to t
    jl_value_t *arg = jl_task_arg_in_transit;
    jl_value_t *res;
    JL_GC_PUSH(&arg);

#ifdef COPY_STACKS
    ptrint_t local_sp = (ptrint_t)jl_pgcstack;
    // here we attempt to figure out how big our stack frame is, since we
    // might need to copy all of it later. this is a bit of a fuzzy guess.
    local_sp += sizeof(jl_gcframe_t);
    local_sp += 12*sizeof(void*);
    t->stackbase = (void*)(local_sp + _frame_offset);
    if (jl_setjmp(t->base_ctx, 0)) {
        // we get here to remove our data from the process stack
        switch_stack(jl_current_task, jl_jmp_target);
    }
#endif
    if (n_args_in_transit == 0) {
        res = jl_apply(t->start, NULL, 0);
    }
    else if (n_args_in_transit == 1) {
        res = jl_apply(t->start, &arg, 1);
    }
    else {
        assert(jl_is_tuple(jl_task_arg_in_transit));
        res = jl_apply(t->start, &jl_tupleref(jl_task_arg_in_transit,0),
                       n_args_in_transit);
    }
    JL_GC_POP();
    finish_task(t, res);
    jl_task_t *cont = t->on_exit;
    // if parent task has exited, try its parent, and so on
    while (cont->done)
        cont = cont->on_exit;
    jl_switchto(cont, t->result);
    assert(0);
}

#ifndef COPY_STACKS
static void init_task(jl_task_t *t)
{
    if (jl_setjmp(t->ctx, 0)) {
        start_task(t);
    }
    // this runs when the task is created
    ptrint_t local_sp = (ptrint_t)&t;
    ptrint_t new_sp = (ptrint_t)t->stack + t->ssize - _frame_offset;
#ifdef __LP64__
    // SP must be 16-byte aligned
    new_sp = new_sp&-16;
    local_sp = local_sp&-16;
#endif
    memcpy((void*)new_sp, (void*)local_sp, _frame_offset);
    rebase_state(&t->ctx, local_sp, new_sp);
}
#endif

void getFunctionInfo(char **name, int *line, const char **filename, size_t pointer);

static void push_frame_info_from_ip(jl_array_t *a, size_t ip)
{
    char *func_name;
    int line_num;
    const char *file_name;
    int i = jl_array_len(a);
    getFunctionInfo(&func_name, &line_num, &file_name, ip);
    if (func_name != NULL) {
        jl_array_grow_end(a, 3);
        jl_arrayset(a, (jl_value_t*)jl_symbol(func_name), i); i++;
        jl_arrayset(a, (jl_value_t*)jl_symbol(file_name), i); i++;
        jl_arrayset(a, jl_box_long(line_num), i);
    }
}

#if defined(__APPLE__)
// stacktrace using execinfo
static jl_value_t *build_backtrace(void)
{
    void *array[1024];
    size_t ip;
    size_t *p;
    jl_array_t *a;
    a = jl_alloc_cell_1d(0);
    JL_GC_PUSH(&a);
    
    backtrace(array, 1023);
    p = (size_t*)array;
    while ((ip = *(p++)) != 0) {
        push_frame_info_from_ip(a, ip);
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}
#elif defined(__WIN32__)
static jl_value_t *build_backtrace(void)
{
    void *array[1024];
    size_t ip;
    size_t *p;
    jl_array_t *a;
    unsigned short num;
    a = jl_alloc_cell_1d(0);
    JL_GC_PUSH(&a);

    /** MINGW does not have the necessary declarations for linking CaptureStackBackTrace*/
#if defined(__MINGW_H)
    HINSTANCE kernel32 = LoadLibrary("Kernel32.dll");

    if (kernel32 != NULL) {
        typedef USHORT (*CaptureStackBackTraceType)(ULONG FramesToSkip, ULONG FramesToCapture, void* BackTrace, ULONG* BackTraceHash);
        CaptureStackBackTraceType func = (CaptureStackBackTraceType)GetProcAddress(kernel32, "RtlCaptureStackBackTrace");

        if (func == NULL) {
            FreeLibrary(kernel32);
            kernel32 = NULL;
            func = NULL;
            return (jl_value_t*)a;
        }
        else {
            num = func(0, 1023, array, NULL);
        }
    }
    else {
        JL_PUTS("Failed to load kernel32.dll", JL_STDERR);
        jl_exit(1);
    }
    FreeLibrary(kernel32);
#else
    num = RtlCaptureStackBackTrace(0, 1023, array, NULL);
#endif

    p = (size_t*)array;
    while ((ip = *(p++)) != 0 && (num--)>0) {
        push_frame_info_from_ip(a, ip);
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}
#else
// stacktrace using libunwind
static jl_value_t *build_backtrace(void)
{
    unw_cursor_t cursor; unw_context_t uc;
    unw_word_t ip;
    jl_array_t *a;
    size_t n=0;
    a = jl_alloc_cell_1d(0);
    JL_GC_PUSH(&a);
    
    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    while (unw_step(&cursor) && n < 10000) { 
        unw_get_reg(&cursor, UNW_REG_IP, &ip);
        push_frame_info_from_ip(a, ip);
        n++;
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}
#endif

DLLEXPORT void jl_register_toplevel_eh(void)
{
    jl_current_task->state.eh_task->state.bt = 1;
}

// yield to exception handler
void jl_raise(jl_value_t *e)
{
    jl_task_t *eh = jl_current_task->state.eh_task;
    eh->state.err = 1;
    jl_exception_in_transit = e;
    if (eh->state.bt) {
        jl_value_t *tracedata, *bt;
        tracedata = build_backtrace();
        JL_GC_PUSH(&tracedata);
        bt = jl_new_struct(jl_backtrace_type,
                           jl_exception_in_transit, tracedata);
        jl_exception_in_transit = bt;
        JL_GC_POP();
    }
    if (jl_current_task == eh && eh->state.eh_ctx!=0) {
        jl_longjmp(*eh->state.eh_ctx, 1);
    }
    else {
        if (eh->done || eh->state.eh_ctx==NULL) {
            // our handler is not available, use root task
            JL_PRINTF(JL_STDERR, "warning: exception handler exited\n");
            eh = jl_root_task;
        }
        // for now, exit the task
        finish_task(jl_current_task, e);
        ctx_switch(eh, eh->state.eh_ctx);
        // TODO: continued exception
    }
    jl_exit(1);
}

jl_task_t *jl_new_task(jl_function_t *start, size_t ssize)
{
    size_t pagesz = jl_page_size;
    jl_task_t *t = (jl_task_t*)allocobj(sizeof(jl_task_t));
    t->type = (jl_type_t*)jl_task_type;
    ssize = LLT_ALIGN(ssize, pagesz);
    t->ssize = ssize;
    t->on_exit = NULL;
    t->last = jl_current_task;
    t->tls = jl_current_task->tls;
    t->consumers = jl_nothing;
    t->done = 0;
    t->runnable = 1;
    t->start = start;
    t->result = NULL;
    t->state.err = 0;
    t->state.bt = 0;
    t->state.eh_task = jl_current_task->state.eh_task;
    // there is no active exception handler available on this stack yet
    t->state.eh_ctx = NULL;
    t->state.prev = NULL;
#ifdef JL_GC_MARKSWEEP
    t->state.gcstack = NULL;
#endif
    t->stkbuf = NULL;

#ifdef COPY_STACKS
    t->bufsz = 0;
#else
    JL_GC_PUSH(&t);

    char *stk = allocb(ssize+pagesz+(pagesz-1));
    t->stkbuf = stk;
    stk = (char*)LLT_ALIGN((uptrint_t)stk, pagesz);
    // add a guard page to detect stack overflow
    // the GC might read this area, which is ok, just prevent writes
    if (mprotect(stk, pagesz-1, PROT_READ) == -1)
        jl_errorf("mprotect: %s", strerror(errno));
    t->stack = stk+pagesz;

    init_task(t);
    JL_GC_POP();
    jl_gc_add_finalizer((jl_value_t*)t, jl_unprotect_stack_func);
#endif

    return t;
}

JL_CALLABLE(jl_unprotect_stack)
{
#ifndef COPY_STACKS
    jl_task_t *t = (jl_task_t*)args[0];
    char *stk = t->stack-jl_page_size;
    // unprotect stack so it can be reallocated for something else
    mprotect(stk, jl_page_size-1, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif
    return (jl_value_t*)jl_null;
}

#define JL_MIN_STACK     (4096*sizeof(void*))
#define JL_DEFAULT_STACK (2*12288*sizeof(void*))

JL_CALLABLE(jl_f_task)
{
    JL_NARGS(Task, 1, 2);
    JL_TYPECHK(Task, function, args[0]);
    /*
      we need a somewhat large stack, because execution can trigger
      compilation, which uses perhaps too much stack space.
    */
    size_t ssize = JL_DEFAULT_STACK;
    if (nargs == 2) {
        JL_TYPECHK(Task, long, args[1]);
        ssize = jl_unbox_long(args[1]);
        if (ssize < JL_MIN_STACK)
            jl_error("Task: stack size too small");
    }
    return (jl_value_t*)jl_new_task((jl_function_t*)args[0], ssize);
}

JL_CALLABLE(jl_f_yieldto)
{
    JL_NARGSV(yieldto, 1);
    JL_TYPECHK(yieldto, task, args[0]);
    n_args_in_transit = nargs-1;
    if (nargs == 2) {
        jl_task_arg_in_transit = args[1];
    }
    else if (nargs > 2) {
        jl_task_arg_in_transit = jl_f_tuple(NULL, &args[1], n_args_in_transit);
    }
    else {
        jl_task_arg_in_transit = (jl_value_t*)jl_null;
    }
    return switchto((jl_task_t*)args[0]);
}

DLLEXPORT jl_value_t *jl_get_current_task(void)
{
    return (jl_value_t*)jl_current_task;
}

jl_function_t *jl_unprotect_stack_func;

void jl_init_tasks(void *stack, size_t ssize)
{
    _probe_arch();
    jl_task_type = jl_new_struct_type(jl_symbol("Task"), jl_any_type,
                                      jl_null,
                                      jl_tuple(6, jl_symbol("parent"),
                                               jl_symbol("last"),
                                               jl_symbol("tls"),
                                               jl_symbol("consumers"),
                                               jl_symbol("done"),
                                               jl_symbol("runnable")),
                                      jl_tuple(6, jl_any_type, jl_any_type,
                                               jl_any_type, jl_any_type,
                                               jl_bool_type, jl_bool_type));
    jl_tupleset(jl_task_type->types, 0, (jl_value_t*)jl_task_type);
    jl_task_type->fptr = jl_f_task;

    jl_current_task = (jl_task_t*)allocobj(sizeof(jl_task_t));
    jl_current_task->type = (jl_type_t*)jl_task_type;
#ifdef COPY_STACKS
    jl_current_task->stackbase = stack+ssize;
    jl_current_task->ssize = 0;  // size of saved piece
    jl_current_task->bufsz = 0;
#else
    jl_current_task->stack = stack;
    jl_current_task->ssize = ssize;
#endif
    jl_current_task->stkbuf = NULL;
    jl_current_task->on_exit = jl_current_task;
    jl_current_task->last = jl_current_task;
    jl_current_task->tls = NULL;
    jl_current_task->consumers = NULL;
    jl_current_task->done = 0;
    jl_current_task->runnable = 1;
    jl_current_task->start = NULL;
    jl_current_task->result = NULL;
    jl_current_task->state.err = 0;
    jl_current_task->state.bt = 0;
    jl_current_task->state.eh_task = jl_current_task;
    jl_current_task->state.eh_ctx = NULL;
    jl_current_task->state.prev = NULL;
#ifdef JL_GC_MARKSWEEP
    jl_current_task->state.gcstack = NULL;
#endif

    jl_root_task = jl_current_task;

    jl_exception_in_transit = (jl_value_t*)jl_null;
    jl_task_arg_in_transit = (jl_value_t*)jl_null;
    jl_unprotect_stack_func = jl_new_closure(jl_unprotect_stack, (jl_value_t*)jl_null, NULL);
}
