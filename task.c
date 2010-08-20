/*
  task.c
  lightweight processes (or asymmetric coroutines)
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <libgen.h>
#include <unistd.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

/* This probing code is derived from Douglas Jones' user thread library */

/* the list of offsets in jmp_buf to be adjusted */
/* # of offsets cannot be greater than jmp_buf */
static int _offsets[sizeof(jmp_buf) / sizeof(int)];
static int _offsets_len;

/* true if stack grows up, false if down */
static int _stack_grows_up;

/* the offset of the beginning of the stack frame in a function */
static size_t _frame_offset;

struct _probe_data {
    intptr_t low_bound;		/* below probe on stack */
    intptr_t probe_local;	/* local to probe on stack */
    intptr_t high_bound;	/* above probe on stack */
    intptr_t prior_local;	/* value of probe_local from earlier call */

    jmp_buf probe_env;	/* saved environment of probe */
    jmp_buf probe_sameAR;	/* second environment saved by same call */
    jmp_buf probe_samePC;	/* environment saved on previous call */

    jmp_buf * ref_probe;	/* switches between probes */
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
    setjmp( *(p->ref_probe) );
    p->ref_probe = &p->probe_env;
    setjmp( p->probe_sameAR );
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

static void _infer_jmpbuf_offsets(struct _probe_data *pb)
{
    /* following line views jump buffer as array of long intptr_t */
    unsigned i;
    intptr_t * p = (intptr_t *)pb->probe_env;
    intptr_t * sameAR = (intptr_t *)pb->probe_sameAR;
    intptr_t * samePC = (intptr_t *)pb->probe_samePC;
    intptr_t prior_diff = pb->probe_local - pb->prior_local;
    intptr_t min_frame = pb->probe_local;

    for (i = 0; i < sizeof(jmp_buf) / sizeof(intptr_t); ++i) {
        intptr_t pi = p[i], samePCi = samePC[i];
        if (pi != samePCi) {
            if (pi != sameAR[i]) {
                ios_printf(ios_stderr, "could not initialize task support\n");
                exit(1);
            }
            if ((pi - samePCi) == prior_diff) {
                /* the i'th pointer field in jmp_buf needs to be save/restored */
                _offsets[_offsets_len++] = i;
                if ((_stack_grows_up && min_frame > pi) || (!_stack_grows_up && min_frame < pi)) {
                    min_frame = pi;
                }
            }
        }
    }

    /*
    _frame_offset = (_stack_grows_up
                     ? pb->probe_local - min_frame
                     : min_frame - pb->probe_local);
    */
    _frame_offset = labs(prior_diff);
}

static void _infer_direction_from(int *first_addr)
{
    int second;
    _stack_grows_up = (first_addr < &second);
}

static void _infer_stack_direction()
{
    int first;
    _infer_direction_from(&first);
}

static void _probe_arch()
{
    struct _probe_data p;
    memset(p.probe_env, 0, sizeof(jmp_buf));
    memset(p.probe_sameAR, 0, sizeof(jmp_buf));
    memset(p.probe_samePC, 0, sizeof(jmp_buf));
    p.ref_probe = &p.probe_samePC;

    _infer_stack_direction();

    /* do a probe with filler on stack */
    fill(&p);
    /* do a probe without filler */
    boundlow(&p);
    _infer_jmpbuf_offsets(&p);
}

/* end probing code */

/*
  TODO:
  - per-task storage (scheme-like parameters)
  - stack growth
*/

extern size_t jl_page_size;
jl_struct_type_t *jl_task_type;
jl_task_t * volatile jl_current_task;
jl_task_t *jl_root_task;
static jl_value_t * volatile task_arg_in_transit;

jl_value_t *jl_switchto(jl_task_t *t, jl_value_t *arg)
{
    if (t->done)
        return t->result;
    task_arg_in_transit = arg;
    if (!setjmp(jl_current_task->ctx)) {
        jl_current_task = t;
        longjmp(t->ctx, 1);
    }
    return task_arg_in_transit;
}

// yield to exception handler
void jl_raise()
{
    jl_task_t *eh = jl_current_task->state.eh_task;
    eh->state.err = 1;
    if (jl_current_task == eh) {
        longjmp(*eh->state.eh_ctx, 1);
    }
    else {
        if (eh->done || eh->state.eh_ctx==NULL) {
            // our handler is not available, use root task
            ios_printf(ios_stderr, "warning: exception handler exited\n");
            eh = jl_root_task;
        }
        if (!setjmp(jl_current_task->ctx)) {
            jl_current_task = eh;
            longjmp(*eh->state.eh_ctx, 1);
        }
        else {
            // TODO: continued exception
        }
    }
}

#ifdef LINUX
#if defined(ARCH_X86)
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
#elif defined(ARCH_X86_64)
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
#endif //LINUX

/* rebase any values in saved state to the new stack */
static void rebase_state(jmp_buf *ctx, intptr_t local_sp, intptr_t new_sp)
{
    ptrint_t *s = (ptrint_t*)ctx;
    ptrint_t diff = new_sp - local_sp; /* subtract old base, and add new base */
#if defined(LINUX) && defined(ARCH_X86)
    s[3] += diff;
    s[4] = ptr_mangle(ptr_demangle(s[4])+diff);
#elif defined(LINUX) && defined(ARCH_X86_64)
    s[1] = ptr_mangle(ptr_demangle(s[1])+diff);
    s[6] = ptr_mangle(ptr_demangle(s[6])+diff);
#elif defined(MACOSX) && defined(ARCH_X86)
    s[8] += diff;
    s[9] += diff;
#elif defined(MACOSX) && defined(ARCH_X86_64)
    s[1] += diff;
    s[2] += diff;
#else
    // use automated guess and hope for the best
    int i;
    for (i=0; i < _offsets_len; i++) {
        s[_offsets[i]] += diff;
    }
#endif
}

static void init_task(jl_task_t *t)
{
    if (setjmp(t->ctx)) {
        // this runs the first time we switch to t
        t->result = jl_apply(t->start, NULL, 0);
        t->done = 1;
        jl_task_t *cont = t->on_exit;
        // if parent task has exited, try its parent, and so on
        while (cont->done)
            cont = cont->on_exit;
        jl_switchto(cont, t->result);
    }
    // this runs when the task is created
    ptrint_t local_sp = (ptrint_t)&t;
    ptrint_t new_sp = (ptrint_t)t->stack + t->ssize - _frame_offset;
    memcpy((void*)new_sp, (void*)local_sp, _frame_offset);
    rebase_state(&t->ctx, local_sp, new_sp);
}

jl_task_t *jl_new_task(jl_function_t *start, size_t ssize)
{
    size_t pagesz = jl_page_size;
    jl_task_t *t = (jl_task_t*)allocb(sizeof(jl_task_t));
    t->type = (jl_type_t*)jl_task_type;
    ssize = LLT_ALIGN(ssize, pagesz);
    t->ssize = ssize;
    char *stk = allocb(ssize+pagesz+(pagesz-1));
    stk = (char*)LLT_ALIGN((uptrint_t)stk, pagesz);
    // add a guard page to detect stack overflow
    // the GC might read this area, which is ok, just prevent writes
    if (mprotect(stk, pagesz-1, PROT_READ) == -1)
        jl_errorf("mprotect: %s", strerror(errno));
    t->stack = stk+pagesz;
    t->on_exit = jl_current_task;
    t->done = 0;
    t->start = start;
    t->state.err = 0;
    t->state.eh_task = jl_current_task->state.eh_task;
    // there is no active exception handler available on this stack yet
    t->state.eh_ctx = NULL;
    t->state.current_output_stream = jl_current_task->state.current_output_stream;
    init_task(t);

    return t;
}

JL_CALLABLE(jl_f_task)
{
    JL_NARGS(Task, 1, 2);
    JL_TYPECHK(Task, function, args[0]);
    /*
      we need a somewhat large stack, because execution can trigger
      compilation, which uses perhaps too much stack space.
    */
    size_t ssize = 24576;
#ifdef BITS64
    ssize *= 2;
#endif
    if (nargs == 2) {
        JL_TYPECHK(Task, int32, args[1]);
        ssize = jl_unbox_int32(args[1]);
        if (ssize < 4096)
            jl_error("Task: stack size too small");
    }
    return (jl_value_t*)jl_new_task((jl_function_t*)args[0], ssize);
}

JL_CALLABLE(jl_f_yieldto)
{
    JL_NARGS(yieldto, 1, 2);
    jl_value_t *arg = (nargs==1 ? (jl_value_t*)jl_null : args[1]);
    JL_TYPECHK(yieldto, task, args[0]);
    return jl_switchto((jl_task_t*)args[0], arg);
}

JL_CALLABLE(jl_f_current_task)
{
    JL_NARGS(current_task, 0, 0);
    return (jl_value_t*)jl_current_task;
}

JL_CALLABLE(jl_f_taskdone)
{
    JL_NARGS(task_done, 1, 1);
    JL_TYPECHK(task_done, task, args[0]);
    return ((jl_task_t*)args[0])->done ? jl_true : jl_false;
}

void jl_init_tasks(void *stack, size_t ssize)
{
    _probe_arch();
    jl_task_type = jl_new_struct_type(jl_symbol("Task"), jl_any_type,
                                      jl_null,
                                      jl_tuple(1, jl_symbol("parent")),
                                      jl_tuple(1, jl_any_type));
    jl_tupleset(jl_task_type->types, 0, (jl_value_t*)jl_task_type);
    jl_task_type->fptr = jl_f_task;

    jl_current_task = (jl_task_t*)allocb(sizeof(jl_task_t));
    jl_current_task->type = (jl_type_t*)jl_task_type;
    jl_current_task->ssize = ssize;
    jl_current_task->stack = stack;
    jl_current_task->on_exit = jl_current_task;
    jl_current_task->done = 0;
    jl_current_task->start = jl_bottom_func;
    jl_current_task->state.err = 0;
    jl_current_task->state.eh_task = jl_current_task;
    jl_current_task->state.eh_ctx = NULL;
    jl_current_task->state.current_output_stream = ios_stdout;

    jl_root_task = jl_current_task;

    jl_add_builtin("Task", (jl_value_t*)jl_task_type);
    jl_add_builtin_func("yieldto", jl_f_yieldto);
    jl_add_builtin_func("current_task", jl_f_current_task);
    jl_add_builtin_func("task_done", jl_f_taskdone);
}
