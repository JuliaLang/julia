/*
  task.c
  lightweight processes (symmetric coroutines)
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
//#include <sys/mman.h>
#include <signal.h>
#include <errno.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_)
#include <winbase.h>
#include <malloc.h>
#include <dbghelp.h>
volatile int jl_in_stackwalk = 0;
#else
#include <unistd.h>
// This gives unwind only local unwinding options ==> faster code
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#include <dlfcn.h>   // for dladdr
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
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
static void __declspec(noinline) fill(struct _probe_data *p);
#else
static void fill(struct _probe_data *p) __attribute__ ((noinline));
#endif

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

static jl_sym_t *done_sym;
static jl_sym_t *failed_sym;
static jl_sym_t *runnable_sym;

extern size_t jl_page_size;
jl_datatype_t *jl_task_type;
DLLEXPORT jl_task_t * volatile jl_current_task;
jl_task_t *jl_root_task;
jl_value_t * volatile jl_task_arg_in_transit;
jl_value_t *jl_exception_in_transit;
#ifdef JL_GC_MARKSWEEP
jl_gcframe_t *jl_pgcstack = NULL;
#endif

static void start_task(jl_task_t *t);

#ifdef COPY_STACKS
jl_jmp_buf * volatile jl_jmp_target;

static void save_stack(jl_task_t *t)
{
    if (t->state == done_sym || t->state == failed_sym)
        return;
    volatile int _x;
    size_t nb = (char*)t->stackbase - (char*)&_x;
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
}

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
void __declspec(noinline) restore_stack(jl_task_t *t, jl_jmp_buf *where, char *p)
#else
void __attribute__((noinline)) restore_stack(jl_task_t *t, jl_jmp_buf *where, char *p)
#endif
{
    char* _x = (char*)t->stackbase - t->ssize;
    if (!p) {
        p = _x;
        if ((char*)&_x > _x) {
            p = (char*)alloca((char*)&_x - _x);
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
#ifdef COPY_STACKS
        jl_task_t *lastt = jl_current_task;
        save_stack(lastt);
#endif

        // set up global state for new task
#ifdef JL_GC_MARKSWEEP
        jl_current_task->gcstack = jl_pgcstack;
        jl_pgcstack = t->gcstack;
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

        t->last = jl_current_task;
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

extern int jl_in_gc;
static jl_value_t *switchto(jl_task_t *t)
{
    if (t->state == done_sym || t->state == failed_sym) {
        jl_task_arg_in_transit = (jl_value_t*)jl_null;
        if (t->exception != jl_nothing)
            jl_throw(t->exception);
        return t->result;
    }
    if (jl_in_gc) {
        jl_error("task switch not allowed from inside gc finalizer");
    }
    ctx_switch(t, &t->ctx);
    jl_value_t *val = jl_task_arg_in_transit;
    jl_task_arg_in_transit = (jl_value_t*)jl_null;
    if (jl_current_task->exception != NULL &&
        jl_current_task->exception != jl_nothing) {
        jl_value_t *exc = jl_current_task->exception;
        jl_current_task->exception = jl_nothing;
        jl_throw(exc);
    }
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
    return switchto(t);
}

static jl_function_t *task_done_hook_func=NULL;

static void finish_task(jl_task_t *t, jl_value_t *resultval)
{
    if (t->exception != jl_nothing)
        t->state = failed_sym;
    else
        t->state = done_sym;
    t->result = resultval;
    // TODO: early free of t->stkbuf
#ifdef COPY_STACKS
    t->stkbuf = NULL;
#endif
    if (task_done_hook_func == NULL) {
        task_done_hook_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                            jl_symbol("task_done_hook"));
    }
    if (task_done_hook_func != NULL) {
        jl_apply(task_done_hook_func, (jl_value_t**)&t, 1);
    }
    assert(0);
}

static void start_task(jl_task_t *t)
{
    // this runs the first time we switch to t
    jl_value_t *arg = jl_task_arg_in_transit;
    jl_value_t *res;
    JL_GC_PUSH1(&arg);

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
    res = jl_apply(t->start, NULL, 0);
    JL_GC_POP();
    finish_task(t, res);
    assert(0);
}

DLLEXPORT void jl_handle_stack_switch()
{
    jl_switch_stack(jl_current_task, jl_jmp_target);
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
#ifdef _P64
    // SP must be 16-byte aligned
    new_sp = new_sp&-16;
    local_sp = local_sp&-16;
#endif
    memcpy((void*)new_sp, (void*)local_sp, _frame_offset);
    rebase_state(&t->ctx, local_sp, new_sp);
}
#endif

ptrint_t bt_data[MAX_BT_SIZE+1];
size_t bt_size = 0;

void jl_getFunctionInfo(const char **name, int *line, const char **filename, size_t pointer, int skipC);

static const char* name_unknown = "???";
static int frame_info_from_ip(const char **func_name, int *line_num, const char **file_name, size_t ip, int skipC)
{
    int fromC = 0;

    jl_getFunctionInfo(func_name, line_num, file_name, ip, skipC);
    if (*func_name == NULL) {
        fromC = 1;
#if defined(_OS_WINDOWS_)
        if (jl_in_stackwalk) {
            *func_name = name_unknown;
            *file_name = name_unknown;
            *line_num = ip;
        }
        else {
            jl_in_stackwalk = 1;
            DWORD64 dwDisplacement64 = 0;
            DWORD64 dwAddress = ip;

            char buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)];
            PSYMBOL_INFO pSymbol = (PSYMBOL_INFO)buffer;
            pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
            pSymbol->MaxNameLen = MAX_SYM_NAME;

            if (SymFromAddr(GetCurrentProcess(), dwAddress, &dwDisplacement64, pSymbol)) {
                // SymFromAddr returned success
                *func_name = strdup(pSymbol->Name);
            }
            else {
                *func_name = name_unknown;
                // SymFromAddr failed
                //DWORD error = GetLastError();
                //printf("SymFromAddr returned error : %d\n", error);
            }

            IMAGEHLP_LINE64 line;
            DWORD dwDisplacement = 0;
            line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);

            if (SymGetLineFromAddr64(GetCurrentProcess(), dwAddress, &dwDisplacement, &line)) {
                // SymGetLineFromAddr64 returned success
                *file_name = strdup(line.FileName);
                *line_num = line.LineNumber;
            }
            else {
                *file_name = name_unknown;
                *line_num = ip;
                // SymGetLineFromAddr64 failed
                //DWORD error = GetLastError();
                //printf("SymGetLineFromAddr64 returned error : %d\n", error);
            }
            jl_in_stackwalk = 0;
        }
#else
        *func_name = name_unknown;
        *file_name = name_unknown;
        *line_num = ip;
#endif
    }
    return fromC;
}

#if defined(_OS_WINDOWS_)
int needsSymRefreshModuleList;
BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
DLLEXPORT size_t rec_backtrace(ptrint_t *data, size_t maxsize)
{
    CONTEXT Context;
    memset(&Context, 0, sizeof(Context));
    jl_in_stackwalk = 1;
    RtlCaptureContext(&Context);
    jl_in_stackwalk = 0;
    return rec_backtrace_ctx(data, maxsize, &Context);
}
DLLEXPORT size_t rec_backtrace_ctx(ptrint_t *data, size_t maxsize, CONTEXT *Context)
{
    if (jl_in_stackwalk) {
        return 0;
    }
    STACKFRAME64 stk;
    memset(&stk, 0, sizeof(stk));

    if (needsSymRefreshModuleList && hSymRefreshModuleList != 0) {
        jl_in_stackwalk = 1;
        hSymRefreshModuleList(GetCurrentProcess());
        jl_in_stackwalk = 0;
        needsSymRefreshModuleList = 0;
    }
#if defined(_CPU_X86_64_) 
    DWORD MachineType = IMAGE_FILE_MACHINE_AMD64;
    stk.AddrPC.Offset = Context->Rip;
    stk.AddrStack.Offset = Context->Rsp;
    stk.AddrFrame.Offset = Context->Rbp;
#elif defined(_CPU_X86_)
    DWORD MachineType = IMAGE_FILE_MACHINE_I386;
    stk.AddrPC.Offset = Context->Eip;
    stk.AddrStack.Offset = Context->Esp;
    stk.AddrFrame.Offset = Context->Ebp;
#else
#error WIN16 not supported :P
#endif
    stk.AddrPC.Mode = AddrModeFlat;
    stk.AddrStack.Mode = AddrModeFlat;
    stk.AddrFrame.Mode = AddrModeFlat;
    
    size_t n = 0;
    intptr_t lastsp = stk.AddrStack.Offset;
    while (n < maxsize) {
        jl_in_stackwalk = 1;
        BOOL result = StackWalk64(MachineType, GetCurrentProcess(), hMainThread,
            &stk, Context, NULL, SymFunctionTableAccess64, SymGetModuleBase64, NULL);
        jl_in_stackwalk = 0;
        data[n++] = (intptr_t)stk.AddrPC.Offset;
        intptr_t sp = (intptr_t)stk.AddrStack.Offset;
        if (!result || sp == 0 || 
            (_stack_grows_up ? sp < lastsp : sp > lastsp) ||
            stk.AddrReturn.Offset == 0)
            break;
        lastsp = sp;
    }
    return n;
}
#else
// stacktrace using libunwind
DLLEXPORT size_t rec_backtrace(ptrint_t *data, size_t maxsize)
{
    unw_context_t uc;
    unw_getcontext(&uc);
    return rec_backtrace_ctx(data, maxsize, &uc);
}
DLLEXPORT size_t rec_backtrace_ctx(ptrint_t *data, size_t maxsize, unw_context_t *uc)
{
#ifndef __arm__
    unw_cursor_t cursor;
    unw_word_t ip;
    size_t n=0;
    
    unw_init_local(&cursor, uc);
    do {
        if (n >= maxsize)
            break;
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0) {
            break;
        }
        data[n++] = ip;
    } while (unw_step(&cursor) > 0);
    return n;
#else
    return 0;
#endif
}
#ifdef LIBOSXUNWIND
size_t rec_backtrace_ctx_dwarf(ptrint_t *data, size_t maxsize, unw_context_t *uc)
{
    unw_cursor_t cursor;
    unw_word_t ip;
    size_t n=0;
    
    unw_init_local_dwarf(&cursor, uc);
    do {
        if (n >= maxsize)
            break;
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0) {
            break;
        }
        data[n++] = ip;
    } while (unw_step(&cursor) > 0);
    return n;
}
#endif
#endif

static void record_backtrace(void)
{
    bt_size = rec_backtrace(bt_data, MAX_BT_SIZE);
}

static jl_value_t *array_ptr_void_type = NULL;
DLLEXPORT jl_value_t *jl_backtrace_from_here(void)
{
    if (array_ptr_void_type == NULL)
        array_ptr_void_type = jl_apply_type((jl_value_t*)jl_array_type,
                                            jl_tuple2(jl_voidpointer_type,
                                                      jl_box_long(1)));
    jl_array_t *bt = jl_alloc_array_1d(array_ptr_void_type, MAX_BT_SIZE);
    size_t n = rec_backtrace((ptrint_t*)jl_array_data(bt), MAX_BT_SIZE);
    if (n < MAX_BT_SIZE)
        jl_array_del_end(bt, MAX_BT_SIZE-n);
    return (jl_value_t*)bt;
}

DLLEXPORT jl_value_t *jl_lookup_code_address(void *ip, int skipC)
{
    const char *func_name;
    int line_num;
    const char *file_name;
    int fromC = frame_info_from_ip(&func_name, &line_num, &file_name, (size_t)ip, skipC);
    if (func_name != NULL) {
        jl_value_t *r = (jl_value_t*)jl_alloc_tuple(4);
        JL_GC_PUSH1(&r);
        jl_tupleset(r, 0, jl_symbol(func_name));
        jl_tupleset(r, 1, jl_symbol(file_name));
        jl_tupleset(r, 2, jl_box_long(line_num));
        jl_tupleset(r, 3, jl_box_bool(fromC));
#if defined(_OS_WINDOWS_) && !defined(LLVM34)
        if (fromC && func_name != name_unknown) free((void*)func_name);
        if (fromC && file_name != name_unknown) free((void*)file_name);
#endif
        JL_GC_POP();
        return r;
    }
    return (jl_value_t*)jl_null;
}

DLLEXPORT jl_value_t *jl_get_backtrace(void)
{
    if (array_ptr_void_type == NULL)
        array_ptr_void_type = jl_apply_type((jl_value_t*)jl_array_type,
                                            jl_tuple2(jl_voidpointer_type,
                                                      jl_box_long(1)));
    jl_array_t *bt = jl_alloc_array_1d(array_ptr_void_type, bt_size);
    memcpy(bt->data, bt_data, bt_size*sizeof(void*));
    return (jl_value_t*)bt;
}

//for looking up functions from gdb:
DLLEXPORT void gdblookup(ptrint_t ip)
{
    const char *func_name;
    int line_num;
    const char *file_name;
#ifdef _OS_WINDOWS_
    int fromC = frame_info_from_ip(&func_name, &line_num, &file_name, ip, 0);
#else
    frame_info_from_ip(&func_name, &line_num, &file_name, ip, 0);
#endif
    if (func_name != NULL) {
        ios_printf(ios_stderr, "%s at %s:%d\n", func_name, file_name, line_num);
#ifdef _OS_WINDOWS_
        if (fromC && func_name != name_unknown) free((void*)func_name);
        if (fromC && file_name != name_unknown) free((void*)file_name);
#endif
    }
}

DLLEXPORT void jlbacktrace()
{
    for(size_t i=0; i < bt_size; i++)
        gdblookup(bt_data[i]);
}

DLLEXPORT void gdbbacktrace()
{
    record_backtrace();
    jlbacktrace();
}


// yield to exception handler
void NORETURN throw_internal(jl_value_t *e)
{
    assert(e != NULL);
    jl_exception_in_transit = e;
    if (jl_current_task->eh != NULL) {
        jl_longjmp(jl_current_task->eh->eh_ctx, 1);
    }
    else {
        if (jl_current_task == jl_root_task) {
            JL_PRINTF(JL_STDERR, "fatal: error thrown and no exception handler available.\n");
            jl_static_show(JL_STDERR, e);
            JL_PRINTF(JL_STDERR, "\n");
            exit(1);
        }
        jl_current_task->exception = e;
        finish_task(jl_current_task, e);
        assert(0);
    }
    jl_exit(1);
}

// record backtrace and raise an error
DLLEXPORT void jl_throw(jl_value_t *e)
{
    assert(e != NULL);
    record_backtrace();
    throw_internal(e);
}

DLLEXPORT void jl_rethrow(void)
{
    throw_internal(jl_exception_in_transit);
}

DLLEXPORT void jl_rethrow_other(jl_value_t *e)
{
    throw_internal(e);
}

DLLEXPORT void jl_throw_with_superfluous_argument(jl_value_t *e, int line)
{
    jl_throw(e);
}

jl_task_t *jl_new_task(jl_function_t *start, size_t ssize)
{
    size_t pagesz = jl_page_size;
    jl_task_t *t = (jl_task_t*)allocobj(sizeof(jl_task_t));
    t->type = (jl_value_t*)jl_task_type;
    ssize = LLT_ALIGN(ssize, pagesz);
    t->ssize = ssize;
    t->current_module = NULL;
    t->parent = jl_current_task;
    t->last = NULL;
    t->tls = jl_nothing;
    t->consumers = jl_nothing;
    t->state = runnable_sym;
    t->start = start;
    t->result = jl_nothing;
    t->donenotify = jl_nothing;
    t->exception = jl_nothing;
    // there is no active exception handler available on this stack yet
    t->eh = NULL;
#ifdef JL_GC_MARKSWEEP
    t->gcstack = NULL;
#endif
    t->stkbuf = NULL;

#ifdef COPY_STACKS
    t->bufsz = 0;
#else
    JL_GC_PUSH1(&t);

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
    if (nargs == 2) {
        jl_task_arg_in_transit = args[1];
    }
    else if (nargs > 2) {
        jl_task_arg_in_transit = jl_f_tuple(NULL, &args[1], nargs-1);
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
    jl_task_type = jl_new_datatype(jl_symbol("Task"),
                                   jl_any_type,
                                   jl_null,
                                   jl_tuple(9,
                                            jl_symbol("parent"),
                                            jl_symbol("last"),
                                            jl_symbol("storage"),
                                            jl_symbol("state"),
                                            jl_symbol("consumers"),
                                            jl_symbol("donenotify"),
                                            jl_symbol("result"),
                                            jl_symbol("exception"),
                                            jl_symbol("code")),
                                   jl_tuple(9,
                                            jl_any_type, jl_any_type,
                                            jl_any_type, jl_sym_type,
                                            jl_any_type, jl_any_type,
                                            jl_any_type, jl_any_type, jl_function_type),
                                   0, 1);
    jl_tupleset(jl_task_type->types, 0, (jl_value_t*)jl_task_type);
    jl_task_type->fptr = jl_f_task;

    done_sym = jl_symbol("done");
    failed_sym = jl_symbol("failed");
    runnable_sym = jl_symbol("runnable");

    jl_current_task = (jl_task_t*)allocobj(sizeof(jl_task_t));
    jl_current_task->type = (jl_value_t*)jl_task_type;
#ifdef COPY_STACKS
    jl_current_task->stackbase = NULL;
    jl_current_task->ssize = 0;  // size of saved piece
    jl_current_task->bufsz = 0;
#else
    jl_current_task->stack = stack;
    jl_current_task->ssize = ssize;
#endif
    jl_current_task->stkbuf = NULL;
    jl_current_task->parent = jl_current_task;
    jl_current_task->current_module = jl_current_module;
    jl_current_task->last = jl_current_task;
    jl_current_task->tls = NULL;
    jl_current_task->consumers = NULL;
    jl_current_task->state = runnable_sym;
    jl_current_task->start = NULL;
    jl_current_task->result = NULL;
    jl_current_task->donenotify = NULL;
    jl_current_task->exception = NULL;
    jl_current_task->eh = NULL;
#ifdef JL_GC_MARKSWEEP
    jl_current_task->gcstack = NULL;
#endif

    jl_root_task = jl_current_task;

    jl_exception_in_transit = (jl_value_t*)jl_null;
    jl_task_arg_in_transit = (jl_value_t*)jl_null;
    jl_unprotect_stack_func = jl_new_closure(jl_unprotect_stack, (jl_value_t*)jl_null, NULL);
}

#ifdef __cplusplus
}
#endif
