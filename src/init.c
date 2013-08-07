/*
  init.c
  system initialization and global state
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <assert.h>

#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

#include <errno.h>
#include <signal.h>

#if !defined(_OS_WINDOWS_) || defined(_COMPILER_MINGW_)
#include <getopt.h>
#endif

#include "julia.h"
#include <stdio.h>

#ifdef _OS_WINDOWS_
#define WIN32_LEAN_AND_MEAN
// Copied from MINGW_FLOAT_H which may not be found due to a colision with the builtin gcc float.h
// eventually we can probably integrate this into OpenLibm.
#if defined(_COMPILER_MINGW_)
void __cdecl __MINGW_NOTHROW _fpreset (void);
void __cdecl __MINGW_NOTHROW fpreset (void);
#else
void __cdecl _fpreset (void);
void __cdecl fpreset (void);
#endif
#define _FPE_INVALID        0x81
#define _FPE_DENORMAL       0x82
#define _FPE_ZERODIVIDE     0x83
#define _FPE_OVERFLOW       0x84
#define _FPE_UNDERFLOW      0x85
#define _FPE_INEXACT        0x86
#define _FPE_UNEMULATED     0x87
#define _FPE_SQRTNEG        0x88
#define _FPE_STACKOVERFLOW  0x8a
#define _FPE_STACKUNDERFLOW 0x8b
#define _FPE_EXPLICITGEN    0x8c    /* raise( SIGFPE ); */
#include <windows.h>
#if defined(_CPU_X86_64_)
#include <dbghelp.h>
extern int needsSymRefreshModuleList;
#endif
#endif
#if defined(__linux__)
//#define _GNU_SOURCE
#include <sched.h>   // for setting CPU affinity
#endif

int jl_boot_file_loaded = 0;

char *jl_stack_lo;
char *jl_stack_hi;
size_t jl_page_size;

static void jl_find_stack_bottom(void)
{
    size_t stack_size;
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__)
    struct rlimit rl;
    getrlimit(RLIMIT_STACK, &rl);
    stack_size = rl.rlim_cur;
#else
    stack_size = 262144;  // guess
#endif
    jl_stack_hi = (char*)&stack_size;
    jl_stack_lo = jl_stack_hi - stack_size;
}

#ifdef _OS_WINDOWS_
void __cdecl fpe_handler(int arg,int num)
#else
void fpe_handler(int arg)
#endif
{
    (void)arg;
#ifndef _OS_WINDOWS_
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGFPE);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);
#else
    fpreset();
    signal(SIGFPE, (void (__cdecl *)(int))fpe_handler);
    switch(num) {
    case _FPE_INVALID:
    case _FPE_OVERFLOW:
    case _FPE_UNDERFLOW:
    default:
        jl_errorf("Unexpected FPE Error 0x%X", num);
        break;
    case _FPE_ZERODIVIDE:
#endif
        jl_throw(jl_diverror_exception);
#ifdef _OS_WINDOWS_
        break;
    }
#endif
}

#ifndef _OS_WINDOWS_
#if defined(_OS_LINUX_)
void segv_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;

    if (
#ifdef COPY_STACKS
        (char*)info->si_addr > (char*)jl_stack_lo-3000000 &&
        (char*)info->si_addr < (char*)jl_stack_hi
#else
        (char*)info->si_addr > (char*)jl_current_task->stack-8192 &&
        (char*)info->si_addr <
        (char*)jl_current_task->stack+jl_current_task->ssize
#endif
        ) {
        sigemptyset(&sset);
        sigaddset(&sset, SIGSEGV);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_stackovf_exception);
    }
    else {
        uv_tty_reset_mode();
        sigfillset(&sset);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        signal(sig, SIG_DFL);
        if (sig != SIGSEGV &&
            sig != SIGBUS &&
            sig != SIGILL)
            raise(sig);
    }
}
#endif

#endif

volatile sig_atomic_t jl_signal_pending = 0;
volatile sig_atomic_t jl_defer_signal = 0;

#ifdef _OS_WINDOWS_
void restore_signals()
{
    SetConsoleCtrlHandler(NULL,0); //turn on ctrl-c handler
}
static void __fastcall win_raise_exception(void* excpt)
{ //why __fastcall? because the first two arguments are passed in registers, making this easier
    jl_throw(excpt);
}
volatile HANDLE hMainThread = NULL;
DLLEXPORT void jlbacktrace();
DLLEXPORT void gdblookup(ptrint_t ip);
static BOOL WINAPI sigint_handler(DWORD wsig) //This needs winapi types to guarantee __stdcall
{
    int sig;
    //windows signals use different numbers from unix
    switch(wsig) {
        case CTRL_C_EVENT: sig = SIGINT; break;
        //case CTRL_BREAK_EVENT: sig = SIGTERM; break;
        // etc.
        default: sig = SIGTERM; break;
    }
    if (jl_defer_signal) {
        jl_signal_pending = sig;
    }
    else {
        jl_signal_pending = 0;
        if ((DWORD)-1 == SuspendThread(hMainThread)) {
            //error
            fputs("error: SuspendThread failed\n",stderr);
            return 0;
        }
        CONTEXT ctxThread;
        memset(&ctxThread,0,sizeof(CONTEXT));
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!GetThreadContext(hMainThread, &ctxThread)) {
            //error
            fputs("error: GetThreadContext failed\n",stderr);
            return 0;
        }
#if defined(_CPU_X86_64_)
        ctxThread.Rip = (DWORD64)&win_raise_exception;
        ctxThread.Rcx = (DWORD64)jl_interrupt_exception;
        ctxThread.Rsp &= (DWORD64)-16;
        ctxThread.Rsp -= 8; //fix up the stack pointer -- this seems to be correct by observation
#elif defined(_CPU_X86_)
        ctxThread.Eip = (DWORD)&win_raise_exception;
        ctxThread.Ecx = (DWORD)jl_interrupt_exception;
        ctxThread.Esp &= (DWORD)-16;
        ctxThread.Esp -= 4; //fix up the stack pointer
#else
#error WIN16 not supported :P
#endif
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!SetThreadContext(hMainThread,&ctxThread)) {
            fputs("error: SetThreadContext failed\n",stderr);
            //error
            return 0;
        }
        if ((DWORD)-1 == ResumeThread(hMainThread)) {
            fputs("error: ResumeThread failed\n",stderr);
            //error
            return 0;
        }
    }
    return 1;
}
static LONG WINAPI exception_handler(struct _EXCEPTION_POINTERS *ExceptionInfo) {
    if (ExceptionInfo->ExceptionRecord->ExceptionFlags == 0) {
        switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
        case EXCEPTION_STACK_OVERFLOW:
#if defined(_CPU_X86_64_)
            ExceptionInfo->ContextRecord->Rip = (DWORD64)&win_raise_exception;
            ExceptionInfo->ContextRecord->Rcx = (DWORD64)jl_stackovf_exception;
            ExceptionInfo->ContextRecord->Rsp &= (DWORD64)-16;
            ExceptionInfo->ContextRecord->Rsp -= 8; //fix up the stack pointer -- this seems to be correct by observation
#elif defined(_CPU_X86_)
            ExceptionInfo->ContextRecord->Eip = (DWORD)&win_raise_exception;
            ExceptionInfo->ContextRecord->Ecx = (DWORD)jl_stackovf_exception;
            ExceptionInfo->ContextRecord->Esp &= (DWORD)-16;
            ExceptionInfo->ContextRecord->Esp -= 4; //fix up the stack pointer
#else
#error WIN16 not supported :P
#endif
            return EXCEPTION_CONTINUE_EXECUTION;
        default:
            ios_puts("Please submit a bug report with steps to reproduce this fault, and any error messages that follow (in their entirety). Thanks.\nException: ", ios_stderr);
            switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
                case EXCEPTION_ACCESS_VIOLATION:
                    ios_puts("EXCEPTION_ACCESS_VIOLATION", ios_stderr); break;
                case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
                    ios_puts("EXCEPTION_ARRAY_BOUNDS_EXCEEDED", ios_stderr); break;
                case EXCEPTION_BREAKPOINT:
                    ios_puts("EXCEPTION_BREAKPOINT", ios_stderr); break;
                case EXCEPTION_DATATYPE_MISALIGNMENT:
                    ios_puts("EXCEPTION_DATATYPE_MISALIGNMENT", ios_stderr); break;
                case EXCEPTION_FLT_DENORMAL_OPERAND:
                    ios_puts("EXCEPTION_FLT_DENORMAL_OPERAND", ios_stderr); break;
                case EXCEPTION_FLT_DIVIDE_BY_ZERO:
                    ios_puts("EXCEPTION_FLT_DIVIDE_BY_ZERO", ios_stderr); break;
                case EXCEPTION_FLT_INEXACT_RESULT:
                    ios_puts("EXCEPTION_FLT_INEXACT_RESULT", ios_stderr); break;
                case EXCEPTION_FLT_INVALID_OPERATION:
                    ios_puts("EXCEPTION_FLT_INVALID_OPERATION", ios_stderr); break;
                case EXCEPTION_FLT_OVERFLOW:
                    ios_puts("EXCEPTION_FLT_OVERFLOW", ios_stderr); break;
                case EXCEPTION_FLT_STACK_CHECK:
                    ios_puts("EXCEPTION_FLT_STACK_CHECK", ios_stderr); break;
                case EXCEPTION_FLT_UNDERFLOW:
                    ios_puts("EXCEPTION_FLT_UNDERFLOW", ios_stderr); break;
                case EXCEPTION_ILLEGAL_INSTRUCTION:
                    ios_puts("EXCEPTION_ILLEGAL_INSTRUCTION", ios_stderr); break;
                case EXCEPTION_IN_PAGE_ERROR:
                    ios_puts("EXCEPTION_IN_PAGE_ERROR", ios_stderr); break;
                case EXCEPTION_INT_DIVIDE_BY_ZERO:
                    ios_puts("EXCEPTION_INT_DIVIDE_BY_ZERO", ios_stderr); break;
                case EXCEPTION_INT_OVERFLOW:
                    ios_puts("EXCEPTION_INT_OVERFLOW", ios_stderr); break;
                case EXCEPTION_INVALID_DISPOSITION:
                    ios_puts("EXCEPTION_INVALID_DISPOSITION", ios_stderr); break;
                case EXCEPTION_NONCONTINUABLE_EXCEPTION:
                    ios_puts("EXCEPTION_NONCONTINUABLE_EXCEPTION", ios_stderr); break;
                case EXCEPTION_PRIV_INSTRUCTION:
                    ios_puts("EXCEPTION_PRIV_INSTRUCTION", ios_stderr); break;
                case EXCEPTION_SINGLE_STEP:
                    ios_puts("EXCEPTION_SINGLE_STEP", ios_stderr); break;
                case EXCEPTION_STACK_OVERFLOW:
                    ios_puts("EXCEPTION_STACK_OVERFLOW", ios_stderr); break;
                default:
                    ios_puts("UNKNOWN", ios_stderr); break;
            }
            ios_printf(ios_stderr," at 0x%Ix -- ", (size_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);
            gdblookup((ptrint_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);
            bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, ExceptionInfo->ContextRecord);
            jlbacktrace();
            break;
        }
    }
    return EXCEPTION_CONTINUE_SEARCH;
}
#else
void restore_signals()
{
    sigset_t sset;
    sigemptyset(&sset);
    sigprocmask(SIG_SETMASK, &sset, 0);
}
void sigint_handler(int sig, siginfo_t *info, void *context)
{
    if (jl_defer_signal) {
        jl_signal_pending = sig;
    }
    else {
        jl_signal_pending = 0;
        sigset_t sset;
        sigemptyset(&sset);
        sigaddset(&sset, SIGINT);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_interrupt_exception);
    }
}
#endif

struct uv_shutdown_queue_item { uv_handle_t *h; struct uv_shutdown_queue_item *next; };
struct uv_shutdown_queue { struct uv_shutdown_queue_item *first; struct uv_shutdown_queue_item *last; };

static void jl_uv_exitcleanup_add(uv_handle_t* handle, struct uv_shutdown_queue *queue)
{
    struct uv_shutdown_queue_item *item = malloc(sizeof(struct uv_shutdown_queue_item));
    item->h = handle;
    item->next = NULL;
    if (queue->last) queue->last->next = item;
    if (!queue->first) queue->first = item;
    queue->last = item;
}
static void jl_uv_exitcleanup_walk(uv_handle_t* handle, void *arg) {
    if (handle != (uv_handle_t*)jl_uv_stdout && handle != (uv_handle_t*)jl_uv_stderr)
        jl_uv_exitcleanup_add(handle, arg);
}
DLLEXPORT void uv_atexit_hook()
{
#if defined(JL_GC_MARKSWEEP) && defined(GC_FINAL_STATS)
    jl_print_gc_stats(JL_STDERR);
#endif
    if (jl_base_module) {
        jl_value_t *f = jl_get_global(jl_base_module, jl_symbol("_atexit"));
        if (f!=NULL && jl_is_function(f)) {
            jl_apply((jl_function_t*)f, NULL, 0);
        }
    }

    jl_gc_run_all_finalizers();

    uv_loop_t* loop = jl_global_event_loop();
    struct uv_shutdown_queue queue = {NULL, NULL};
    uv_walk(loop, jl_uv_exitcleanup_walk, &queue);
    // close stdout and stderr last, since we like being
    // able to show stuff (incl. printf's)
    jl_uv_exitcleanup_add((uv_handle_t*)jl_uv_stdout, &queue);
    jl_uv_exitcleanup_add((uv_handle_t*)jl_uv_stderr, &queue);
    struct uv_shutdown_queue_item *item = queue.first;
    while (item) {
        uv_handle_t *handle = item->h;
        if (uv_is_closing(handle)) {
            item = item->next;
            continue;
        }
        switch(handle->type) {
        case UV_FILE:
            free(handle);
            break;
        case UV_TTY:
        case UV_UDP:
//#ifndef _OS_WINDOWS_ // unix only supports shutdown on TCP and NAMED_PIPE
// but uv_shutdown doesn't seem to be particularly reliable, so we'll avoid it in general
            jl_close_uv(handle);
            break;
//#endif
        case UV_TCP:
        case UV_NAMED_PIPE:
            // These will be shut down in jl_close_uv.
            jl_close_uv(handle);
            break;
        //Don't close these directly, but rather let the GC take care of it
        case UV_POLL:
            uv_poll_stop((uv_poll_t*)handle);
            handle->data = NULL;
            uv_unref(handle);
            break;
        case UV_TIMER:
            uv_timer_stop((uv_timer_t*)handle);
            handle->data = NULL;
            uv_unref(handle);
            break;
        case UV_IDLE:
            uv_idle_stop((uv_idle_t*)handle);
        case UV_ASYNC:
            handle->data = NULL;
            uv_unref(handle);
            break;
        case UV_FS_EVENT:
            handle->data = NULL;
            uv_unref(handle);
            break;
        case UV_FS_POLL:
            uv_fs_poll_stop((uv_fs_poll_t*)handle);
            handle->data = NULL;
            uv_unref(handle);
            break;
        case UV_PREPARE:
        case UV_CHECK:
        case UV_SIGNAL:
        case UV_PROCESS:
            jl_close_uv(handle);
            break;
        case UV_HANDLE:
        case UV_STREAM:
        case UV_UNKNOWN_HANDLE:
        case UV_HANDLE_TYPE_MAX:
        case UV_RAW_FD:
        case UV_RAW_HANDLE:
        default:
            assert(0);
        }
        item = item->next;
    }
    uv_run(loop,UV_RUN_DEFAULT); //let libuv spin until everything has finished closing
}

void jl_get_builtin_hooks(void);

uv_lib_t *jl_dl_handle;
#ifdef _OS_WINDOWS_
uv_lib_t _jl_ntdll_handle;
uv_lib_t _jl_exe_handle;
uv_lib_t _jl_kernel32_handle;
uv_lib_t _jl_crtdll_handle;
uv_lib_t _jl_winsock_handle;

uv_lib_t *jl_ntdll_handle=&_jl_ntdll_handle;
uv_lib_t *jl_exe_handle=&_jl_exe_handle;
uv_lib_t *jl_kernel32_handle=&_jl_kernel32_handle;
uv_lib_t *jl_crtdll_handle=&_jl_crtdll_handle;
uv_lib_t *jl_winsock_handle=&_jl_winsock_handle;
#endif
uv_loop_t *jl_io_loop;

#ifdef COPY_STACKS
void jl_switch_stack(jl_task_t *t, jl_jmp_buf *where);
extern jl_jmp_buf * volatile jl_jmp_target;
#endif

void *init_stdio_handle(uv_file fd,int readable)
{
    void *handle;
    uv_handle_type type = uv_guess_handle(fd);
    jl_uv_file_t *file;
#ifndef _OS_WINDOWS_    
    // Duplicate the file descritor so we can later dup it over if we want to redirect
    // STDIO without having to worry about closing the associated libuv object. 
    // On windows however, libuv objects remember streams by their HANDLE, so this is 
    // unnessecary. 
    fd = dup(fd);
#endif
    //printf("%d: %d -- %d\n", fd, type);
    switch(type)
    {
        case UV_TTY:
            handle = malloc(sizeof(uv_tty_t));
            if (uv_tty_init(jl_io_loop,(uv_tty_t*)handle,fd,readable)) {
                jl_errorf("Error initializing stdio in uv_tty_init (%d, %d)\n", fd, type);
                abort();
            }
            ((uv_tty_t*)handle)->data=0;
            uv_tty_set_mode((void*)handle,0); //cooked stdio
            break;
        case UV_FILE: 
            file = malloc(sizeof(jl_uv_file_t));
            file->loop = jl_io_loop;
            file->type = UV_FILE;
            file->file = fd;
            file->data = 0;
            handle = file;
            break;
        case UV_NAMED_PIPE:
            handle = malloc(sizeof(uv_pipe_t));
            if (uv_pipe_init(jl_io_loop, (uv_pipe_t*)handle, (readable?UV_PIPE_READABLE:UV_PIPE_WRITABLE))) {
                jl_errorf("Error initializing stdio in uv_pipe_init (%d, %d)\n", fd, type);
                abort();
            }
            if (uv_pipe_open((uv_pipe_t*)handle,fd)) {
                jl_errorf("Error initializing stdio in uv_pipe_open (%d, %d)\n", fd, type);
                abort();
            }
            ((uv_pipe_t*)handle)->data=0;
            break;
        case UV_TCP:
            handle = malloc(sizeof(uv_tcp_t));
            if (uv_tcp_init(jl_io_loop, (uv_tcp_t*)handle)) {
                jl_errorf("Error initializing stdio in uv_tcp_init (%d, %d)\n", fd, type);
                abort();
            }
            if (uv_tcp_open((uv_tcp_t*)handle,fd)) {
                jl_errorf("Error initializing stdio in uv_tcp_open (%d, %d)\n", fd, type);
                abort();
            }
            ((uv_tcp_t*)handle)->data=0;
            break;
        case UV_UDP:
        default:
            jl_errorf("This type of handle for stdio is not yet supported (%d, %d)!\n", fd, type);
            handle = NULL;
            break;
    }
    return handle;
}

void init_stdio()
{   //order must be 2,1,0
    JL_STDERR = init_stdio_handle(2,0);
    JL_STDOUT = init_stdio_handle(1,0);
    JL_STDIN = init_stdio_handle(0,1);
}

#ifndef _OS_WINDOWS_
static void *signal_stack;
#endif

#ifdef _OS_DARWIN_
#include <mach/mach_traps.h>
#include <mach/task.h>
#include <mach/mig_errors.h>
static mach_port_t segv_port = 0;

extern boolean_t exc_server(mach_msg_header_t *, mach_msg_header_t *);

void * mach_segv_listener(void *arg)
{
    (void) arg;
    while (1) {
        int ret = mach_msg_server(exc_server,2048,segv_port,MACH_MSG_TIMEOUT_NONE);
        printf("mach_msg_server: %s\n", mach_error_string(ret));
        jl_exit(1);
    }
}

void darwin_stack_overflow_handler(unw_context_t *uc)
{
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, uc);
    jl_exception_in_transit = jl_stackovf_exception;
    jl_rethrow();
}

#define HANDLE_MACH_ERROR(msg, retval) \
    if(retval!=KERN_SUCCESS) { mach_error(msg ":", (retval)); jl_exit(1); }

//exc_server uses dlsym to find symbol
DLLEXPORT kern_return_t catch_exception_raise
                (mach_port_t                          exception_port,
                 mach_port_t                                  thread,
                 mach_port_t                                    task,
                 exception_type_t                          exception,
                 exception_data_t                               code,
                 mach_msg_type_number_t                   code_count)
{
    unsigned int count = MACHINE_THREAD_STATE_COUNT;
    unsigned int exc_count = X86_EXCEPTION_STATE64_COUNT;
    x86_thread_state64_t state, old_state;
    x86_exception_state64_t exc_state;
    kern_return_t ret;
    //memset(&state,0,sizeof(x86_thread_state64_t));
    //memset(&exc_state,0,sizeof(x86_exception_state64_t));
    ret = thread_get_state(thread,x86_EXCEPTION_STATE64,(thread_state_t)&exc_state,&exc_count);
    uint64_t fault_addr = exc_state.__faultvaddr;
    if (
#ifdef COPY_STACKS
        (char*)fault_addr > (char*)jl_stack_lo-3000000 &&
        (char*)fault_addr < (char*)jl_stack_hi
#else
        (char*)fault_addr > (char*)jl_current_task->stack-8192 &&
        (char*)fault_addr <
        (char*)jl_current_task->stack+jl_current_task->ssize
#endif
        ) 
    {
        ret = thread_get_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,&count);
        HANDLE_MACH_ERROR("thread_get_state",ret);
        old_state = state;
        // memset(&state,0,sizeof(x86_thread_state64_t));
        // Setup libunwind information
        state.__rsp = (uint64_t)signal_stack + SIGSTKSZ;
        state.__rsp -= sizeof(unw_context_t);
        state.__rsp &= -16;
        unw_context_t *uc = (unw_context_t*)state.__rsp;
        state.__rsp -= 512;
        // This is for alignment. In particular note that the sizeof(void*) is necessary
        // since it would usually specify the return address (i.e. we are aligning the call
        // frame to a 16 byte boundary as required by the abi, but the stack pointer
        // to point to the byte beyond that. Not doing this leads to funny behavior on
        // the first access to an external function will fail due to stack misalignment
        state.__rsp &= -16;
        state.__rsp -= sizeof(void*);
        memset(uc,0,sizeof(unw_context_t));
        memcpy(uc,&old_state,sizeof(x86_thread_state64_t));
        state.__rdi = (uint64_t)uc;
        state.__rip = (uint64_t)darwin_stack_overflow_handler;

        state.__rbp = state.__rsp;
        ret = thread_set_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,count);
        HANDLE_MACH_ERROR("thread_set_state",ret);
        return KERN_SUCCESS;
    }
    else {
        return -309;
    }
}

#endif

void julia_init(char *imageFile)
{
    jl_page_size = jl_getpagesize();
    jl_find_stack_bottom();
    jl_dl_handle = jl_load_dynamic_library(NULL, JL_RTLD_DEFAULT);
#ifdef _OS_WINDOWS_
    uv_dlopen("ntdll.dll",jl_ntdll_handle); //bypass julia's pathchecking for system dlls
    uv_dlopen("Kernel32.dll",jl_kernel32_handle);
    uv_dlopen("msvcrt.dll",jl_crtdll_handle);
    uv_dlopen("Ws2_32.dll",jl_winsock_handle);
    _jl_exe_handle.handle = GetModuleHandleA(NULL);
    if (!DuplicateHandle( GetCurrentProcess(), GetCurrentThread(),
        GetCurrentProcess(), (PHANDLE)&hMainThread, 0,
        TRUE, DUPLICATE_SAME_ACCESS )) {
        JL_PRINTF(JL_STDERR, "Couldn't access handle to main thread\n");
    }
#if defined(_CPU_X86_64_)
    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS);
    SymInitialize(GetCurrentProcess(), NULL, 1);
    needsSymRefreshModuleList = 0;
#endif
#endif
    jl_io_loop = uv_default_loop(); //this loop will internal events (spawining process etc.)
    init_stdio();

#if defined(__linux__)
    int ncores = jl_cpu_cores();
    if (ncores > 1) {
        cpu_set_t cpumask;
        CPU_ZERO(&cpumask);
        for(int i=0; i < ncores; i++) {
            CPU_SET(i, &cpumask);
        }
        sched_setaffinity(0, sizeof(cpu_set_t), &cpumask);
    }
#endif

#ifdef JL_GC_MARKSWEEP
    jl_gc_init();
    jl_gc_disable();
#endif
    jl_init_frontend();
    jl_init_types();
    jl_init_tasks(jl_stack_lo, jl_stack_hi-jl_stack_lo);
    jl_init_codegen();
    jl_an_empty_cell = (jl_value_t*)jl_alloc_cell_1d(0);

    jl_init_serializer();

    if (!imageFile) {
        jl_main_module = jl_new_module(jl_symbol("Main"));
        jl_main_module->parent = jl_main_module;
        jl_core_module = jl_new_module(jl_symbol("Core"));
        jl_core_module->parent = jl_main_module;
        jl_set_const(jl_main_module, jl_symbol("Core"),
                     (jl_value_t*)jl_core_module);
        jl_module_using(jl_main_module, jl_core_module);
        jl_current_module = jl_core_module;
        jl_init_intrinsic_functions();
        jl_init_primitives();
        jl_load("boot.jl");
        jl_get_builtin_hooks();
        jl_boot_file_loaded = 1;
        jl_init_box_caches();
    }

    if (imageFile) {
        JL_TRY {
            jl_restore_system_image(imageFile);
        }
        JL_CATCH {
            JL_PRINTF(JL_STDERR, "error during init:\n");
            jl_show(jl_stderr_obj(), jl_exception_in_transit);
            JL_PRINTF(JL_STDERR, "\n");
            jl_exit(1);
        }
    }

    // set module field of primitive types
    int i;
    void **table = jl_core_module->bindings.table;
    for(i=1; i < jl_core_module->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->value && jl_is_datatype(b->value)) {
                jl_datatype_t *tt = (jl_datatype_t*)b->value;
                tt->name->module = jl_core_module;
            }
        }
    }

    // the Main module is the one which is always open, and set as the
    // current module for bare (non-module-wrapped) toplevel expressions.
    // it does "using Base" if Base is available.
    if (jl_base_module != NULL) {
        jl_add_standard_imports(jl_main_module);
    }
    // eval() uses Main by default, so Main.eval === Core.eval
    jl_module_import(jl_main_module, jl_core_module, jl_symbol("eval"));
    jl_current_module = jl_main_module;


#ifndef _OS_WINDOWS_
    signal_stack = malloc(SIGSTKSZ);
    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_handler = fpe_handler;
    actf.sa_flags = 0;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
#if defined(_OS_LINUX_)
    stack_t ss;
    ss.ss_flags = 0;
    ss.ss_size = SIGSTKSZ;
    ss.ss_sp = signal_stack;
    if (sigaltstack(&ss, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "sigaltstack: %s\n", strerror(errno));
        jl_exit(1);
    }

    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = segv_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    if (sigaction(SIGSEGV, &act, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }

    if (signal(SIGPIPE,SIG_IGN) == SIG_ERR) {
        JL_PRINTF(JL_STDERR, "Couldn't set SIGPIPE\n");
        jl_exit(1);
    }
#elif defined (_OS_DARWIN_)
    kern_return_t ret;
    mach_port_t self = mach_task_self();
    ret = mach_port_allocate(self,MACH_PORT_RIGHT_RECEIVE,&segv_port);
    HANDLE_MACH_ERROR("mach_port_allocate",ret);
    ret = mach_port_insert_right(self,segv_port,segv_port,MACH_MSG_TYPE_MAKE_SEND);
    HANDLE_MACH_ERROR("mach_port_insert_right",ret);

    // Alright, create a thread to serve as the listener for exceptions
    pthread_t thread;
    pthread_attr_t attr;
    if (pthread_attr_init(&attr) != 0)
    {
        JL_PRINTF(JL_STDERR, "pthread_attr_init failed");
        jl_exit(1);  
    }
    pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
    if (pthread_create(&thread,&attr,mach_segv_listener,NULL) != 0)
    {
        JL_PRINTF(JL_STDERR, "pthread_create failed");
        jl_exit(1);  
    }     
    pthread_attr_destroy(&attr);

    ret = task_set_exception_ports(self,EXC_MASK_BAD_ACCESS,segv_port,EXCEPTION_DEFAULT,MACHINE_THREAD_STATE);
    HANDLE_MACH_ERROR("task_set_exception_ports",ret);
#endif
#else
    if (signal(SIGFPE, (void (__cdecl *)(int))fpe_handler) == SIG_ERR) {
        JL_PRINTF(JL_STDERR, "Couldn't set SIGFPE\n");
        jl_exit(1);
    }
#endif


#ifdef JL_GC_MARKSWEEP
    jl_gc_enable();
#endif
}

DLLEXPORT void jl_install_sigint_handler()
{
#ifdef _OS_WINDOWS_
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)sigint_handler,1);
#else
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = sigint_handler;
    act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINT, &act, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
#endif
    //printf("sigint installed\n");
}


extern void * __stack_chk_guard;

DLLEXPORT int julia_trampoline(int argc, char **argv, int (*pmain)(int ac,char *av[]))
{
#if defined(_OS_WINDOWS_) //&& !defined(_WIN64)
    SetUnhandledExceptionFilter(exception_handler);
#endif
    unsigned char * p = (unsigned char *) &__stack_chk_guard;
    char a = p[sizeof(__stack_chk_guard)-1];
    char b = p[sizeof(__stack_chk_guard)-2];
    char c = p[0];
    /* If you have the ability to generate random numbers in your kernel then use them */
    p[sizeof(__stack_chk_guard)-1] = 255;
    p[sizeof(__stack_chk_guard)-2] = '\n';
    p[0] = 0;
#ifdef COPY_STACKS
    // initialize base context of root task
    jl_root_task->stackbase = (char*)&argc;
    if (jl_setjmp(jl_root_task->base_ctx, 0)) {
        jl_switch_stack(jl_current_task, jl_jmp_target);
    }
#endif
    int ret = pmain(argc, argv);
    p[sizeof(__stack_chk_guard)-1] = a;
    p[sizeof(__stack_chk_guard)-2] = b;
    p[0] = c;
    return ret;
}

jl_function_t *jl_typeinf_func=NULL;

DLLEXPORT void jl_enable_inference(void)
{
    if (jl_typeinf_func != NULL) return;
    jl_typeinf_func = (jl_function_t*)jl_get_global(jl_base_module,
                                                    jl_symbol("typeinf_ext"));
}

static jl_value_t *core(char *name)
{
    return jl_get_global(jl_core_module, jl_symbol(name));
}

static jl_value_t *basemod(char *name)
{
    return jl_get_global(jl_base_module, jl_symbol(name));
}

// fetch references to things defined in boot.jl
void jl_get_builtin_hooks(void)
{
    jl_nothing = core("nothing");
    jl_root_task->tls = jl_nothing;
    jl_root_task->consumers = jl_nothing;
    jl_root_task->donenotify = jl_nothing;
    jl_root_task->exception = jl_nothing;

    jl_char_type    = (jl_datatype_t*)core("Char");
    jl_int8_type    = (jl_datatype_t*)core("Int8");
    jl_uint8_type   = (jl_datatype_t*)core("Uint8");
    jl_int16_type   = (jl_datatype_t*)core("Int16");
    jl_uint16_type  = (jl_datatype_t*)core("Uint16");
    jl_uint32_type  = (jl_datatype_t*)core("Uint32");
    jl_uint64_type  = (jl_datatype_t*)core("Uint64");

    jl_float16_type = (jl_datatype_t*)core("Float16");
    jl_float32_type = (jl_datatype_t*)core("Float32");
    jl_float64_type = (jl_datatype_t*)core("Float64");
    jl_floatingpoint_type = (jl_datatype_t*)core("FloatingPoint");

    jl_stackovf_exception =
        jl_apply((jl_function_t*)core("StackOverflowError"), NULL, 0);
    jl_diverror_exception =
        jl_apply((jl_function_t*)core("DivideError"), NULL, 0);
    jl_domain_exception =
        jl_apply((jl_function_t*)core("DomainError"), NULL, 0);
    jl_overflow_exception =
        jl_apply((jl_function_t*)core("OverflowError"), NULL, 0);
    jl_inexact_exception =
        jl_apply((jl_function_t*)core("InexactError"), NULL, 0);
    jl_undefref_exception =
        jl_apply((jl_function_t*)core("UndefRefError"),NULL,0);
    jl_interrupt_exception =
        jl_apply((jl_function_t*)core("InterruptException"),NULL,0);
    jl_bounds_exception =
        jl_apply((jl_function_t*)core("BoundsError"),NULL,0);
    jl_memory_exception =
        jl_apply((jl_function_t*)core("MemoryError"),NULL,0);

    jl_ascii_string_type = (jl_datatype_t*)core("ASCIIString");
    jl_utf8_string_type = (jl_datatype_t*)core("UTF8String");
    jl_symbolnode_type = (jl_datatype_t*)core("SymbolNode");
    jl_getfieldnode_type = (jl_datatype_t*)core("GetfieldNode");

    jl_array_uint8_type = jl_apply_type((jl_value_t*)jl_array_type,
                                        jl_tuple2(jl_uint8_type,
                                                  jl_box_long(1)));
}

DLLEXPORT void jl_get_system_hooks(void)
{
    if (jl_errorexception_type) return; // only do this once

    jl_errorexception_type = (jl_datatype_t*)basemod("ErrorException");
    jl_typeerror_type = (jl_datatype_t*)basemod("TypeError");
    jl_methoderror_type = (jl_datatype_t*)basemod("MethodError");
    jl_loaderror_type = (jl_datatype_t*)basemod("LoadError");
    jl_weakref_type = (jl_datatype_t*)basemod("WeakRef");
}
