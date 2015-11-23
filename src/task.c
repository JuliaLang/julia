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
#include <dbghelp.h>
volatile int jl_in_stackwalk = 0;
#else
#include <unistd.h>
#include <sys/mman.h> // for mprotect
#include <dlfcn.h>   // for dladdr
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

void NOINLINE restore_stack(jl_task_t *t, jl_jmp_buf *where, char *p)
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

static void NORETURN finish_task(jl_task_t *t, jl_value_t *resultval)
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
        abort();
    }
    if (task_done_hook_func == NULL) {
        task_done_hook_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                            jl_symbol("task_done_hook"));
    }
    if (task_done_hook_func != NULL) {
        jl_apply(task_done_hook_func, (jl_value_t**)&t, 1);
    }
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

static void record_backtrace(void);
static void NOINLINE NORETURN start_task(void)
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
            res = jl_apply(t->start, NULL, 0);
        }
        JL_CATCH {
            res = jl_exception_in_transit;
            t->exception = res;
            jl_gc_wb(t, res);
        }
    }
    finish_task(t, res);
    abort();
}

#ifdef COPY_STACKS
void NOINLINE jl_set_base_ctx(char *__stk)
{
    jl_stackbase = (char*)(((uptrint_t)__stk + sizeof(*__stk))&-16); // also ensures stackbase is 16-byte aligned
#ifndef ASM_COPY_STACKS
    if (jl_setjmp(jl_base_ctx, 1)) {
        start_task();
    }
#endif
}
#endif

DLLEXPORT void julia_init(JL_IMAGE_SEARCH rel)
{ // keep this function small, since we want to keep the stack frame
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
        bt_size = 0;  // backtraces don't survive task switches, see e.g. issue #12485
#ifdef COPY_STACKS
        jl_task_t *lastt = jl_current_task;
        save_stack(lastt);
#endif

        // set up global state for new task
        jl_current_task->gcstack = jl_pgcstack;
        jl_pgcstack = t->gcstack;

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

extern int jl_in_gc;
DLLEXPORT jl_value_t *jl_switchto(jl_task_t *t, jl_value_t *arg)
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
    if (jl_in_gc)
        jl_error("task switch not allowed from inside gc finalizer");
    jl_task_arg_in_transit = arg;
    ctx_switch(t, &t->ctx);
    jl_value_t *val = jl_task_arg_in_transit;
    jl_task_arg_in_transit = jl_nothing;
    throw_if_exception_set(jl_current_task);
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
static void init_task(jl_task_t *t, char* stack)
{
    if (jl_setjmp(t->ctx, 0)) {
        start_task();
    }
    // this runs when the task is created
    ptrint_t local_sp = (ptrint_t)&t;
    ptrint_t new_sp = (ptrint_t)stack + t->ssize - _frame_offset;
#ifdef _P64
    // SP must be 16-byte aligned
    new_sp = new_sp&-16;
    local_sp = local_sp&-16;
#endif
    memcpy((void*)new_sp, (void*)local_sp, _frame_offset);
    rebase_state(&t->ctx, local_sp, new_sp);
}

#endif /* !COPY_STACKS */

ptrint_t bt_data[MAX_BT_SIZE+1];
size_t bt_size = 0;

// Always Set *func_name and *file_name to malloc'd pointers (non-NULL)
static int frame_info_from_ip(char **func_name,
                              char **file_name, size_t *line_num,
                              char **inlinedat_file, size_t *inlinedat_line,
                              size_t ip, int skipC, int skipInline)
{
    static const char *name_unknown = "???";
    int fromC = 0;

    jl_getFunctionInfo(func_name, file_name, line_num, inlinedat_file, inlinedat_line, ip, &fromC,
                       skipC, skipInline);
    if (!*func_name) {
        *func_name = strdup(name_unknown);
        *line_num = ip;
    }
    if (!*file_name) {
        *file_name = strdup(name_unknown);
    }
    return fromC;
}

#if defined(_OS_WINDOWS_)
#ifdef _CPU_X86_64_
static UNWIND_HISTORY_TABLE HistoryTable;
#else
static struct {
    DWORD64 dwAddr;
    DWORD64 ImageBase;
} HistoryTable;
#endif
static PVOID CALLBACK JuliaFunctionTableAccess64(
        _In_  HANDLE hProcess,
        _In_  DWORD64 AddrBase)
{
    //jl_printf(JL_STDOUT, "lookup %d\n", AddrBase);
#ifdef _CPU_X86_64_
    DWORD64 ImageBase;
    PRUNTIME_FUNCTION fn = RtlLookupFunctionEntry(AddrBase, &ImageBase, &HistoryTable);
    if (fn) return fn;
    if (jl_in_stackwalk) {
        return 0;
    }
    jl_in_stackwalk = 1;
    PVOID ftable = SymFunctionTableAccess64(hProcess, AddrBase);
    jl_in_stackwalk = 0;
    return ftable;
#else
    return SymFunctionTableAccess64(hProcess, AddrBase);
#endif
}
static DWORD64 WINAPI JuliaGetModuleBase64(
        _In_  HANDLE hProcess,
        _In_  DWORD64 dwAddr)
{
    //jl_printf(JL_STDOUT, "lookup base %d\n", dwAddr);
#ifdef _CPU_X86_64_
    DWORD64 ImageBase;
    PRUNTIME_FUNCTION fn = RtlLookupFunctionEntry(dwAddr, &ImageBase, &HistoryTable);
    if (fn) return ImageBase;
    if (jl_in_stackwalk) {
        return 0;
    }
    jl_in_stackwalk = 1;
    DWORD64 fbase = SymGetModuleBase64(hProcess, dwAddr);
    jl_in_stackwalk = 0;
    return fbase;
#else
    if (dwAddr == HistoryTable.dwAddr) return HistoryTable.ImageBase;
    DWORD64 ImageBase = jl_getUnwindInfo(dwAddr);
    if (ImageBase) {
        HistoryTable.dwAddr = dwAddr;
        HistoryTable.ImageBase = ImageBase;
        return ImageBase;
    }
    return SymGetModuleBase64(hProcess, dwAddr);
#endif
}

int needsSymRefreshModuleList;
BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
DLLEXPORT size_t rec_backtrace(ptrint_t *data, size_t maxsize)
{
    CONTEXT Context;
    memset(&Context, 0, sizeof(Context));
    RtlCaptureContext(&Context);
    return rec_backtrace_ctx(data, maxsize, &Context);
}
DLLEXPORT size_t rec_backtrace_ctx(ptrint_t *data, size_t maxsize, CONTEXT *Context)
{
    if (needsSymRefreshModuleList && hSymRefreshModuleList != 0 && !jl_in_stackwalk) {
        jl_in_stackwalk = 1;
        hSymRefreshModuleList(GetCurrentProcess());
        jl_in_stackwalk = 0;
        needsSymRefreshModuleList = 0;
    }
#if !defined(_CPU_X86_64_)
    if (jl_in_stackwalk) {
        return 0;
    }
    DWORD MachineType = IMAGE_FILE_MACHINE_I386;
    STACKFRAME64 stk;
    memset(&stk, 0, sizeof(stk));
    stk.AddrPC.Offset = Context->Eip;
    stk.AddrStack.Offset = Context->Esp;
    stk.AddrFrame.Offset = Context->Ebp;
    stk.AddrPC.Mode = AddrModeFlat;
    stk.AddrStack.Mode = AddrModeFlat;
    stk.AddrFrame.Mode = AddrModeFlat;
    jl_in_stackwalk = 1;
#endif

    size_t n = 0;
    while (n < maxsize) {
#ifndef _CPU_X86_64_
        data[n++] = (intptr_t)stk.AddrPC.Offset;
        BOOL result = StackWalk64(MachineType, GetCurrentProcess(), hMainThread,
            &stk, Context, NULL, JuliaFunctionTableAccess64, JuliaGetModuleBase64, NULL);
        if (!result)
            break;
#else
        data[n++] = (intptr_t)Context->Rip;
        DWORD64 ImageBase = JuliaGetModuleBase64(GetCurrentProcess(), Context->Rip);
        if (!ImageBase)
            break;

        MEMORY_BASIC_INFORMATION mInfo;

        PRUNTIME_FUNCTION FunctionEntry = (PRUNTIME_FUNCTION)JuliaFunctionTableAccess64(GetCurrentProcess(), Context->Rip);
        if (!FunctionEntry) { // assume this is a NO_FPO RBP-based function
            Context->Rsp = Context->Rbp;                 // MOV RSP, RBP

            // Check whether the pointer is valid and executable before dereferencing
            // to avoid segfault while recording. See #10638.
            if (VirtualQuery((LPCVOID)Context->Rsp, &mInfo, sizeof(MEMORY_BASIC_INFORMATION)) == 0)
                break;
            DWORD X = mInfo.AllocationProtect;
            if (!((X&PAGE_READONLY) || (X&PAGE_READWRITE) || (X&PAGE_WRITECOPY) || (X&PAGE_EXECUTE_READ)) ||
                  (X&PAGE_GUARD) || (X&PAGE_NOACCESS))
                break;

            Context->Rbp = *(DWORD64*)Context->Rsp;      // POP RBP
            Context->Rsp = Context->Rsp + sizeof(void*);
            Context->Rip = *(DWORD64*)Context->Rsp;      // POP RIP (aka RET)
            Context->Rsp = Context->Rsp + sizeof(void*);
        }
        else {
            PVOID HandlerData;
            DWORD64 EstablisherFrame;
            (void)RtlVirtualUnwind(
                    0 /*UNW_FLAG_NHANDLER*/,
                    ImageBase,
                    Context->Rip,
                    FunctionEntry,
                    Context,
                    &HandlerData,
                    &EstablisherFrame,
                    NULL);
        }
        if (!Context->Rip)
            break;
#endif
    }
#if !defined(_CPU_X86_64_)
    jl_in_stackwalk = 0;
#endif
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
#if !defined(_CPU_ARM_) && !defined(_CPU_PPC64_)
    unw_cursor_t cursor;
    unw_word_t ip;
    size_t n=0;

    unw_init_local(&cursor, uc);
    do {
        if (n >= maxsize)
            break;
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0)
            break;
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
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0)
            break;
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
    jl_svec_t *tp = NULL;
    jl_array_t *bt = NULL;
    JL_GC_PUSH2(&tp, &bt);
    if (array_ptr_void_type == NULL) {
        tp = jl_svec2(jl_voidpointer_type, jl_box_long(1));
        array_ptr_void_type = jl_apply_type((jl_value_t*)jl_array_type, tp);
    }
    bt = jl_alloc_array_1d(array_ptr_void_type, MAX_BT_SIZE);
    size_t n = rec_backtrace((ptrint_t*)jl_array_data(bt), MAX_BT_SIZE);
    if (n < MAX_BT_SIZE)
        jl_array_del_end(bt, MAX_BT_SIZE-n);
    JL_GC_POP();
    return (jl_value_t*)bt;
}

DLLEXPORT jl_value_t *jl_lookup_code_address(void *ip, int skipC)
{
    char *func_name;
    size_t line_num;
    char *file_name;
    size_t inlinedat_line;
    char *inlinedat_file;
    int fromC = frame_info_from_ip(&func_name, &file_name, &line_num,
                                   &inlinedat_file, &inlinedat_line, (size_t)ip, skipC, 0);
    jl_value_t *r = (jl_value_t*)jl_alloc_svec(7);
    JL_GC_PUSH1(&r);
    jl_svecset(r, 0, jl_symbol(func_name));
    jl_svecset(r, 1, jl_symbol(file_name));
    jl_svecset(r, 2, jl_box_long(line_num));
    jl_svecset(r, 3, jl_symbol(inlinedat_file ? inlinedat_file : ""));
    jl_svecset(r, 4, jl_box_long(inlinedat_file ? inlinedat_line : -1));
    jl_svecset(r, 5, jl_box_bool(fromC));
    jl_svecset(r, 6, jl_box_long((intptr_t)ip));
    free(func_name);
    free(file_name);
    free(inlinedat_file);
    JL_GC_POP();
    return r;
}

DLLEXPORT jl_value_t *jl_get_backtrace(void)
{
    jl_svec_t *tp = NULL;
    jl_array_t *bt = NULL;
    JL_GC_PUSH2(&tp, &bt);
    if (array_ptr_void_type == NULL) {
        tp = jl_svec2(jl_voidpointer_type, jl_box_long(1));
        array_ptr_void_type = jl_apply_type((jl_value_t*)jl_array_type, tp);
    }
    bt = jl_alloc_array_1d(array_ptr_void_type, bt_size);
    memcpy(bt->data, bt_data, bt_size*sizeof(void*));
    JL_GC_POP();
    return (jl_value_t*)bt;
}

//for looking up functions from gdb:
DLLEXPORT void gdblookup(ptrint_t ip)
{
    char *func_name;
    size_t line_num;
    char *file_name;
    size_t inlinedat_line;
    char *inlinedat_file;
    frame_info_from_ip(&func_name, &file_name, &line_num, &inlinedat_file, &inlinedat_line, ip,
                      /* skipC */ 0, /* skipInline */ 1);
    if (line_num == ip) {
        jl_safe_printf("unknown function (ip: %p)\n", (void*)ip);
    }
    else if (line_num == -1) {
        jl_safe_printf("%s at %s (unknown line)\n", func_name, file_name);
    }
    else {
        jl_safe_printf("%s at %s:%" PRIuPTR "\n", func_name, file_name,
                       (uintptr_t)line_num);
    }
    free(func_name);
    free(file_name);
    free(inlinedat_file);
}

DLLEXPORT void jlbacktrace(void)
{
    size_t n = bt_size; //bt_size > 40 ? 40 : bt_size;
    for(size_t i=0; i < n; i++)
        gdblookup(bt_data[i]);
}

DLLEXPORT void gdbbacktrace(void)
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
        jl_printf(JL_STDERR, "fatal: error thrown and no exception handler available.\n");
        jl_static_show(JL_STDERR, e);
        jl_printf(JL_STDERR, "\n");
        jlbacktrace();
        jl_exit(1);
    }
    assert(0);
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

DLLEXPORT jl_task_t *jl_new_task(jl_function_t *start, size_t ssize)
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
    stk = (char*)LLT_ALIGN((uptrint_t)stk, pagesz);
    // add a guard page to detect stack overflow
    if (mprotect(stk, pagesz-1, PROT_NONE) == -1)
        jl_errorf("mprotect: %s", strerror(errno));
    stk += pagesz;

    init_task(t, stk);
    JL_GC_POP();
    jl_gc_add_finalizer((jl_value_t*)t, jl_unprotect_stack_func);
#endif

    return t;
}

JL_CALLABLE(jl_unprotect_stack)
{
#ifndef COPY_STACKS
    jl_task_t *t = (jl_task_t*)args[0];
    size_t pagesz = jl_page_size;
    char *stk = (char*)LLT_ALIGN((uptrint_t)t->stkbuf, pagesz);
    // unprotect stack so it can be reallocated for something else
    mprotect(stk, pagesz - 1, PROT_READ|PROT_WRITE);
#endif
    return jl_nothing;
}

DLLEXPORT jl_value_t *jl_get_current_task(void)
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
                                            jl_any_type, jl_function_type),
                                   0, 1, 8);
    jl_svecset(jl_task_type->types, 0, (jl_value_t*)jl_task_type);

    done_sym = jl_symbol("done");
    failed_sym = jl_symbol("failed");
    runnable_sym = jl_symbol("runnable");

    jl_unprotect_stack_func = jl_new_closure(jl_unprotect_stack, (jl_value_t*)jl_emptysvec, NULL);
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
    // TODO update for threads
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

    jl_root_task = jl_current_task;

    jl_exception_in_transit = (jl_value_t*)jl_nothing;
    jl_task_arg_in_transit = (jl_value_t*)jl_nothing;
}

DLLEXPORT int jl_is_task_started(jl_task_t *t)
{
    return t->started;
}

#ifdef __cplusplus
}
#endif
