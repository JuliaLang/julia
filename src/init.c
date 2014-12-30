/*
  init.c
  system initialization and global state
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <fcntl.h>

#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

#if defined(__APPLE__)
#include <AvailabilityMacros.h>
#define __need_ucontext64_t
#ifdef MAC_OS_X_VERSION_10_10
#include <sys/_types/_ucontext64.h>
#else
#include <machine/_structs.h>
#endif
#endif

#include <errno.h>
#include <signal.h>

#if !defined(_OS_WINDOWS_) || defined(_COMPILER_MINGW_)
#include <getopt.h>
#endif

#include "julia.h"
#include "julia_internal.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
DLLEXPORT char * dirname(char *);
#else
#include <libgen.h>
#endif

#ifdef _OS_WINDOWS_
#define WIN32_LEAN_AND_MEAN
// Copied from MINGW_FLOAT_H which may not be found due to a collision with the builtin gcc float.h
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
#include <dbghelp.h>
#include <io.h>
extern int needsSymRefreshModuleList;
extern BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
#endif
#if defined(__linux__)
//#define _GNU_SOURCE
#include <sched.h>   // for setting CPU affinity
#endif

DLLEXPORT void jlbacktrace();
DLLEXPORT void gdbbacktrace();
DLLEXPORT void gdblookup(ptrint_t ip);

static const char system_image_path[256] = JL_SYSTEM_IMAGE_PATH;

jl_compileropts_t jl_compileropts = { NULL, // julia_home
                                      NULL, // julia_bin
                                      NULL, // build_path
                                      system_image_path, // image_file
                                      NULL, // cpu_target ("native", "core2", etc...)
                                      0,    // code_coverage
                                      0,    // malloc_log
                                      JL_COMPILEROPT_CHECK_BOUNDS_DEFAULT,
                                      JL_COMPILEROPT_DUMPBITCODE_OFF,
                                      0,    // int_literals
                                      JL_COMPILEROPT_COMPILE_DEFAULT,
                                      0,    // opt_level
                                      1,    // depwarn
                                      1     // can_inline
};

int jl_boot_file_loaded = 0;
int exit_on_sigint = 0;

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
void __cdecl fpe_handler(int arg, int num)
{
    (void)arg;
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
        jl_throw(jl_diverror_exception);
        break;
    }
}
#else
void fpe_handler(int arg)
{
    (void)arg;
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGFPE);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    jl_throw(jl_diverror_exception);
}
#endif

#ifndef _OS_WINDOWS_
static int is_addr_on_stack(void *addr)
{
#ifdef COPY_STACKS
    return ((char*)addr > (char*)jl_stack_lo-3000000 &&
            (char*)addr < (char*)jl_stack_hi);
#else
    return ((char*)addr > (char*)jl_current_task->stack-8192 &&
            (char*)addr < (char*)jl_current_task->stack+jl_current_task->ssize);
#endif
}

#ifndef SIGINFO
#define SIGINFO SIGUSR1
#endif

void sigdie_handler(int sig, siginfo_t *info, void *context)
{
    if (sig != SIGINFO) {
        sigset_t sset;
        uv_tty_reset_mode();
        sigfillset(&sset);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        signal(sig, SIG_DFL);
    }
    ios_printf(ios_stderr,"\nsignal (%d): %s\n", sig, strsignal(sig));
#ifdef __APPLE__
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, (bt_context_t)&((ucontext64_t*)context)->uc_mcontext64->__ss);
#else
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, (ucontext_t*)context);
#endif
    jlbacktrace();
    if (sig != SIGSEGV &&
        sig != SIGBUS &&
        sig != SIGILL &&
        sig != SIGINFO) {
        raise(sig);
    }
}
#endif

#if defined(__linux__) || defined(__FreeBSD__)
extern int in_jl_;
void segv_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;

    if (sig == SIGSEGV && (in_jl_ || is_addr_on_stack(info->si_addr))) { // stack overflow
        sigemptyset(&sset);
        sigaddset(&sset, SIGSEGV);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_stackovf_exception);
    }
    else if (info->si_code == SEGV_ACCERR) {  // writing to read-only memory (e.g., mmap)
        sigemptyset(&sset);
        sigaddset(&sset, SIGSEGV);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_memory_exception);
    }
    else {
        sigdie_handler(sig, info, context);
    }
}
#endif

volatile sig_atomic_t jl_signal_pending = 0;
volatile sig_atomic_t jl_defer_signal = 0;

#ifdef _OS_WINDOWS_
BOOL (*pSetThreadStackGuarantee)(PULONG);
void restore_signals(void)
{
    SetConsoleCtrlHandler(NULL, 0); //turn on ctrl-c handler
}

void jl_throw_in_ctx(jl_value_t *excpt, CONTEXT *ctxThread, int bt)
{
    assert(excpt != NULL);
#if defined(_CPU_X86_64_)
    DWORD64 Rsp = (ctxThread->Rsp&(DWORD64)-16) - 8;
#elif defined(_CPU_X86_)
    DWORD32 Esp = (ctxThread->Esp&(DWORD32)-16) - 4;
#else
#error WIN16 not supported :P
#endif
    bt_size = bt ? rec_backtrace_ctx(bt_data, MAX_BT_SIZE, ctxThread) : 0;
    jl_exception_in_transit = excpt;
#if defined(_CPU_X86_64_)
    *(DWORD64*)Rsp = 0;
    ctxThread->Rsp = Rsp;
    ctxThread->Rip = (DWORD64)&jl_rethrow;
#elif defined(_CPU_X86_)
    *(DWORD32*)Esp = 0;
    ctxThread->Esp = Esp;
    ctxThread->Eip = (DWORD)&jl_rethrow;
#endif
}

volatile HANDLE hMainThread = NULL;

static BOOL WINAPI sigint_handler(DWORD wsig) //This needs winapi types to guarantee __stdcall
{
    if (exit_on_sigint) jl_exit(0);
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
        jl_throw_in_ctx(jl_interrupt_exception, &ctxThread, 1);
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

static LONG WINAPI _exception_handler(struct _EXCEPTION_POINTERS *ExceptionInfo, int in_ctx)
{
    if (ExceptionInfo->ExceptionRecord->ExceptionFlags == 0) {
        switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
                fpreset();
                jl_throw_in_ctx(jl_diverror_exception, ExceptionInfo->ContextRecord,in_ctx);
                return EXCEPTION_CONTINUE_EXECUTION;
            case EXCEPTION_STACK_OVERFLOW:
                jl_throw_in_ctx(jl_stackovf_exception, ExceptionInfo->ContextRecord,in_ctx&&pSetThreadStackGuarantee);
                return EXCEPTION_CONTINUE_EXECUTION;
        }
        ios_puts("\nPlease submit a bug report with steps to reproduce this fault, and any error messages that follow (in their entirety). Thanks.\nException: ", ios_stderr);
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
        static int recursion = 0;
        if (recursion++)
            exit(1);
        else
            jl_exit(1);
    }
    return EXCEPTION_CONTINUE_SEARCH;
}

static LONG WINAPI exception_handler(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
    return _exception_handler(ExceptionInfo,1);
}

#if defined(_CPU_X86_64_)
EXCEPTION_DISPOSITION _seh_exception_handler(PEXCEPTION_RECORD ExceptionRecord, void *EstablisherFrame, PCONTEXT ContextRecord, void *DispatcherContext)
{
    EXCEPTION_POINTERS ExceptionInfo;
    ExceptionInfo.ExceptionRecord = ExceptionRecord;
    ExceptionInfo.ContextRecord = ContextRecord;

    EXCEPTION_DISPOSITION rval;
    switch (_exception_handler(&ExceptionInfo,1)) {
        case EXCEPTION_CONTINUE_EXECUTION:
            rval = ExceptionContinueExecution; break;
        case EXCEPTION_CONTINUE_SEARCH:
            rval = ExceptionContinueSearch; break;
#ifndef _MSC_VER
        case EXCEPTION_EXECUTE_HANDLER:
            rval = ExceptionExecuteHandler; break;
#endif
    }

    return rval;
}
#endif
#else // #ifdef _OS_WINDOWS_

void restore_signals(void)
{
    sigset_t sset;
    sigemptyset(&sset);
    sigprocmask(SIG_SETMASK, &sset, 0);
}

void sigint_handler(int sig, siginfo_t *info, void *context)
{
    if (exit_on_sigint) jl_exit(0);
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

static void jl_uv_exitcleanup_add(uv_handle_t *handle, struct uv_shutdown_queue *queue)
{
    struct uv_shutdown_queue_item *item = (struct uv_shutdown_queue_item*)malloc(sizeof(struct uv_shutdown_queue_item));
    item->h = handle;
    item->next = NULL;
    if (queue->last) queue->last->next = item;
    if (!queue->first) queue->first = item;
    queue->last = item;
}

static void jl_uv_exitcleanup_walk(uv_handle_t *handle, void *arg)
{
    if (handle != (uv_handle_t*)jl_uv_stdout && handle != (uv_handle_t*)jl_uv_stderr)
        jl_uv_exitcleanup_add(handle, (struct uv_shutdown_queue*)arg);
}

void jl_write_coverage_data(void);
void jl_write_malloc_log(void);

DLLEXPORT void jl_atexit_hook()
{
#if defined(JL_GC_MARKSWEEP) && defined(GC_FINAL_STATS)
    jl_print_gc_stats(JL_STDERR);
#endif
    if (jl_compileropts.code_coverage)
        jl_write_coverage_data();
    if (jl_compileropts.malloc_log)
        jl_write_malloc_log();
    if (jl_base_module) {
        jl_value_t *f = jl_get_global(jl_base_module, jl_symbol("_atexit"));
        if (f!=NULL && jl_is_function(f)) {
            JL_TRY {
                jl_apply((jl_function_t*)f, NULL, 0);
            }
            JL_CATCH {
                JL_PRINTF(JL_STDERR, "\natexit hook threw an error: ");
                jl_show(jl_stderr_obj(),jl_exception_in_transit);
            }
        }
    }

    jl_gc_run_all_finalizers();

    uv_loop_t *loop = jl_global_event_loop();
    struct uv_shutdown_queue queue = {NULL, NULL};
    uv_walk(loop, jl_uv_exitcleanup_walk, &queue);
    // close stdout and stderr last, since we like being
    // able to show stuff (incl. printf's)
    jl_uv_exitcleanup_add((uv_handle_t*)jl_uv_stdout, &queue);
    jl_uv_exitcleanup_add((uv_handle_t*)jl_uv_stderr, &queue);
    //uv_unref((uv_handle_t*)jl_uv_stdout);
    //uv_unref((uv_handle_t*)jl_uv_stderr);
    struct uv_shutdown_queue_item *item = queue.first;
    while (item) {
        JL_TRY {
            while (item) {
                uv_handle_t *handle = item->h;
                if (handle->type != UV_FILE && uv_is_closing(handle)) {
                    item = item->next;
                    continue;
                }
                switch(handle->type) {
                case UV_TTY:
                case UV_UDP:
                case UV_TCP:
                case UV_NAMED_PIPE:
                case UV_POLL:
                case UV_TIMER:
                case UV_ASYNC:
                case UV_FS_EVENT:
                case UV_FS_POLL:
                case UV_IDLE:
                case UV_PREPARE:
                case UV_CHECK:
                case UV_SIGNAL:
                case UV_PROCESS:
                case UV_FILE:
                    // These will be shutdown as appropriate by jl_close_uv
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
        }
        JL_CATCH {
            //error handling -- continue cleanup, as much as possible
            uv_unref(item->h);
            jl_printf(JL_STDERR, "error during exit cleanup: close: ");
            jl_static_show(JL_STDERR, jl_exception_in_transit);
            item = item->next;
        }
    }
    uv_run(loop,UV_RUN_DEFAULT); //let libuv spin until everything has finished closing
}

void jl_get_builtin_hooks(void);

uv_lib_t *jl_dl_handle;
uv_lib_t _jl_RTLD_DEFAULT_handle;
uv_lib_t *jl_RTLD_DEFAULT_handle=&_jl_RTLD_DEFAULT_handle;
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

void *init_stdio_handle(uv_file fd,int readable)
{
    void *handle;
    uv_handle_type type = uv_guess_handle(fd);
    jl_uv_file_t *file;
#ifndef _OS_WINDOWS_
    // Duplicate the file descriptor so we can later dup it over if we want to redirect
    // STDIO without having to worry about closing the associated libuv object.
    // On windows however, libuv objects remember streams by their HANDLE, so this is
    // unnecessary.
    fd = dup(fd);
#endif
    //printf("%d: %d -- %d\n", fd, type, 0);
    switch(type) {
        case UV_TTY:
            handle = malloc(sizeof(uv_tty_t));
            if (uv_tty_init(jl_io_loop,(uv_tty_t*)handle,fd,readable)) {
                jl_errorf("Error initializing stdio in uv_tty_init (%d, %d)\n", fd, type);
                abort();
            }
            ((uv_tty_t*)handle)->data=0;
            uv_tty_set_mode((uv_tty_t*)handle,0); //cooked stdio
            break;
        case UV_UNKNOWN_HANDLE:
            // dup the descriptor with a new one pointing at the bit bucket ...
#if defined(_OS_WINDOWS_)
            _dup2(_open("NUL", O_RDWR | O_BINARY, _S_IREAD | _S_IWRITE), fd);
#else
            dup2(open("/dev/null", O_RDWR, S_IRUSR | S_IWUSR /* 0600 */ | S_IRGRP | S_IROTH /* 0644 */), fd);
#endif
            // ...and continue on as in the UV_FILE case
        case UV_FILE:
            file = (jl_uv_file_t*)malloc(sizeof(jl_uv_file_t));
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
    JL_STDERR = (uv_stream_t*)init_stdio_handle(2,0);
    JL_STDOUT = (uv_stream_t*)init_stdio_handle(1,0);
    JL_STDIN = (uv_stream_t*)init_stdio_handle(0,1);
}

#ifdef JL_USE_INTEL_JITEVENTS
char jl_using_intel_jitevents; // Non-zero if running under Intel VTune Amplifier
#endif

#if defined(JL_USE_INTEL_JITEVENTS) && defined(__linux__)
unsigned sig_stack_size = SIGSTKSZ;
#elif defined(_OS_WINDOWS_)
#define sig_stack_size 131072 // 128k
#else
#define sig_stack_size SIGSTKSZ
#endif

#ifndef _OS_WINDOWS_
static void *signal_stack;
#endif

#ifdef _OS_DARWIN_
#include <mach/mach_traps.h>
#include <mach/task.h>
#include <mach/mig_errors.h>
static mach_port_t segv_port = 0;

extern boolean_t exc_server(mach_msg_header_t *, mach_msg_header_t *);

void *mach_segv_listener(void *arg)
{
    (void)arg;
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

void darwin_accerr_handler(unw_context_t *uc)
{
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, uc);
    jl_exception_in_transit = jl_memory_exception;
    jl_rethrow();
}

#define HANDLE_MACH_ERROR(msg, retval) \
    if (retval!=KERN_SUCCESS) { mach_error(msg ":", (retval)); jl_exit(1); }

#ifdef LIBOSXUNWIND
extern kern_return_t profiler_segv_handler(mach_port_t,mach_port_t,mach_port_t,exception_type_t,exception_data_t,mach_msg_type_number_t);
extern volatile mach_port_t mach_profiler_thread;
#endif

enum x86_trap_flags {
    USER_MODE = 0x4,
    WRITE_FAULT = 0x2,
    PAGE_PRESENT = 0x1
};

//exc_server uses dlsym to find symbol
DLLEXPORT
kern_return_t catch_exception_raise(mach_port_t            exception_port,
                                    mach_port_t            thread,
                                    mach_port_t            task,
                                    exception_type_t       exception,
                                    exception_data_t       code,
                                    mach_msg_type_number_t code_count)
{
    unsigned int count = MACHINE_THREAD_STATE_COUNT;
    unsigned int exc_count = X86_EXCEPTION_STATE64_COUNT;
    x86_thread_state64_t state, old_state;
    x86_exception_state64_t exc_state;
    kern_return_t ret;
    //memset(&state,0,sizeof(x86_thread_state64_t));
    //memset(&exc_state,0,sizeof(x86_exception_state64_t));
#ifdef LIBOSXUNWIND
    if (thread == mach_profiler_thread) {
        return profiler_segv_handler(exception_port,thread,task,exception,code,code_count);
    }
#endif
    ret = thread_get_state(thread,x86_EXCEPTION_STATE64,(thread_state_t)&exc_state,&exc_count);
    HANDLE_MACH_ERROR("thread_get_state(1)",ret);
    uint64_t fault_addr = exc_state.__faultvaddr;
    if (is_addr_on_stack((void*)fault_addr) ||
        ((exc_state.__err & PAGE_PRESENT) == PAGE_PRESENT)) {
        ret = thread_get_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,&count);
        HANDLE_MACH_ERROR("thread_get_state(2)",ret);
        old_state = state;
        // memset(&state,0,sizeof(x86_thread_state64_t));
        // Setup libunwind information
        state.__rsp = (uint64_t)signal_stack + sig_stack_size;
        state.__rsp -= sizeof(unw_context_t);
        state.__rsp &= -16;
        unw_context_t *uc = (unw_context_t*)state.__rsp;
        state.__rsp -= 512;
        // This is for alignment. In particular note that the sizeof(void*) is necessary
        // since it would usually specify the return address (i.e., we are aligning the call
        // frame to a 16 byte boundary as required by the abi, but the stack pointer
        // to point to the byte beyond that. Not doing this leads to funny behavior on
        // the first access to an external function will fail due to stack misalignment
        state.__rsp &= -16;
        state.__rsp -= sizeof(void*);
        memset(uc,0,sizeof(unw_context_t));
        memcpy(uc,&old_state,sizeof(x86_thread_state64_t));
        state.__rdi = (uint64_t)uc;
        if ((exc_state.__err & PAGE_PRESENT) == PAGE_PRESENT)
            state.__rip = (uint64_t)darwin_accerr_handler;
        else
            state.__rip = (uint64_t)darwin_stack_overflow_handler;

        state.__rbp = state.__rsp;
        ret = thread_set_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,count);
        HANDLE_MACH_ERROR("thread_set_state",ret);
        return KERN_SUCCESS;
    }
    else {
        ret = thread_get_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,&count);
        HANDLE_MACH_ERROR("thread_get_state(3)",ret);
        ios_printf(ios_stderr,"\nsignal (%d): %s\n", SIGSEGV, strsignal(SIGSEGV));
        bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, (unw_context_t*)&state);
        jlbacktrace();
        return KERN_INVALID_ARGUMENT;
    }
}

#endif

static int isabspath(const char *in)
{
#ifdef _OS_WINDOWS_
    char c0 = in[0];
    if (c0 == '/' || c0 == '\\') {
        return 1; // absolute path relative to %CD% (current drive), or UNC
    }
    else {
        int s = strlen(in);
        if (s > 2) {
            char c1 = in[1];
            char c2 = in[2];
            if (c1 == ':' && (c2 == '/' || c2 == '\\')) return 1; // absolute path
        }
    }
#else
    if (jl_compileropts.image_file[0] == '/') return 1; // absolute path
#endif
    return 0; // relative path
}

static char *abspath(const char *in)
{ // compute an absolute path location, so that chdir doesn't change the file reference
#ifndef _OS_WINDOWS_
    char *out = realpath(in, NULL);
    if (!out) {
        if (in[0] == PATHSEPSTRING[0]) {
            out = strdup(in);
        }
        else {
            size_t path_size = PATH_MAX;
            size_t len = strlen(in);
            char *path = (char*)malloc(PATH_MAX);
            if (uv_cwd(path, &path_size)) {
                ios_printf(ios_stderr, "fatal error: unexpected error while retrieving current working directory\n");
                exit(1);
            }
            if (path_size + len + 1 >= PATH_MAX) {
                ios_printf(ios_stderr, "fatal error: current working directory path too long\n");
                exit(1);
            }
            path[path_size-1] = PATHSEPSTRING[0];
            memcpy(path+path_size, in, len+1);
            out = strdup(path);
        }
    }
#else
    DWORD n = GetFullPathName(in, 0, NULL, NULL);
    if (n <= 0) {
        ios_printf(ios_stderr, "fatal error: jl_compileropts.image_file path too long or GetFullPathName failed\n");
        exit(1);
    }
    char *out = (char*)malloc(n);
    DWORD m = GetFullPathName(in, n, out, NULL);
    if (n != m + 1) {
        ios_printf(ios_stderr, "fatal error: jl_compileropts.image_file path too long or GetFullPathName failed\n");
        exit(1);
    }
#endif
    return out;
}

static void jl_resolve_sysimg_location(JL_IMAGE_SEARCH rel)
{ // this function resolves the paths in jl_compileropts to absolute file locations as needed
  // and it replaces the pointers to `julia_home`, `julia_bin`, `image_file`, and `build_path`
  // it may fail, print an error, and exit(1) if any of these paths are longer than PATH_MAX
  //
  // note: if you care about lost memory, you should call the appropriate `free()` function
  // on the original pointer for each `char*` you've inserted into `jl_compileropts`, after
  // calling `julia_init()`
    char *free_path = (char*)malloc(PATH_MAX);
    size_t path_size = PATH_MAX;
    if (uv_exepath(free_path, &path_size)) {
        ios_printf(ios_stderr, "fatal error: unexpected error while retrieving exepath\n");
        exit(1);
    }
    if (path_size >= PATH_MAX) {
        ios_printf(ios_stderr, "fatal error: jl_compileropts.julia_bin path too long\n");
        exit(1);
    }
    jl_compileropts.julia_bin = strdup(free_path);
    if (!jl_compileropts.julia_home) {
        jl_compileropts.julia_home = getenv("JULIA_HOME");
        if (!jl_compileropts.julia_home) {
            jl_compileropts.julia_home = dirname(free_path);
        }
    }
    if (jl_compileropts.julia_home)
        jl_compileropts.julia_home = abspath(jl_compileropts.julia_home);
    free(free_path);
    free_path = NULL;
    if (jl_compileropts.image_file) {
        if (rel == JL_IMAGE_JULIA_HOME && !isabspath(jl_compileropts.image_file)) {
            // build time path, relative to JULIA_HOME
            free_path = (char*)malloc(PATH_MAX);
            int n = snprintf(free_path, PATH_MAX, "%s" PATHSEPSTRING "%s",
                     jl_compileropts.julia_home, jl_compileropts.image_file);
            if (n >= PATH_MAX || n < 0) {
                ios_printf(ios_stderr, "fatal error: jl_compileropts.image_file path too long\n");
                exit(1);
            }
            jl_compileropts.image_file = free_path;
        }
        if (jl_compileropts.image_file)
            jl_compileropts.image_file = abspath(jl_compileropts.image_file);
        if (free_path) {
            free(free_path);
            free_path = NULL;
        }
    }
    if (jl_compileropts.build_path)
        jl_compileropts.build_path = abspath(jl_compileropts.build_path);
}

void _julia_init(JL_IMAGE_SEARCH rel)
{
    libsupport_init();
    jl_io_loop = uv_default_loop(); // this loop will internal events (spawning process etc.),
                                    // best to call this first, since it also initializes libuv
    restore_signals();
    jl_resolve_sysimg_location(rel);

    // If we are able to load the sysimg and get a cpu_target, use that unless user has overridden
    if (jl_compileropts.cpu_target == NULL) {
        const char * sysimg_cpu_target = jl_get_system_image_cpu_target(jl_compileropts.image_file);

        // If we can't load anything from the sysimg, default to native
        jl_compileropts.cpu_target = sysimg_cpu_target ? sysimg_cpu_target : "native";
    }

    jl_page_size = jl_getpagesize();
    jl_arr_xtralloc_limit = uv_get_total_memory() / 100;  // Extra allocation limited to 1% of total RAM
    jl_find_stack_bottom();
    jl_dl_handle = (uv_lib_t *) jl_load_dynamic_library(NULL, JL_RTLD_DEFAULT);
#ifdef RTLD_DEFAULT
    jl_RTLD_DEFAULT_handle->handle = RTLD_DEFAULT;
#else
    jl_RTLD_DEFAULT_handle->handle = jl_dl_handle->handle;
#endif
#ifdef _OS_WINDOWS_
    uv_dlopen("ntdll.dll", jl_ntdll_handle); // bypass julia's pathchecking for system dlls
    uv_dlopen("kernel32.dll", jl_kernel32_handle);
#if _MSC_VER == 1800
    uv_dlopen("msvcr120.dll", jl_crtdll_handle);
#else
    uv_dlopen("msvcrt.dll", jl_crtdll_handle);
#endif
    uv_dlopen("ws2_32.dll", jl_winsock_handle);
    _jl_exe_handle.handle = GetModuleHandleA(NULL);
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                         GetCurrentProcess(), (PHANDLE)&hMainThread, 0,
                         TRUE, DUPLICATE_SAME_ACCESS)) {
        ios_printf(ios_stderr, "WARNING: failed to access handle to main thread\n");
    }
    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES);
    if (!SymInitialize(GetCurrentProcess(), NULL, 1)) {
        ios_printf(ios_stderr, "WARNING: failed to initialize stack walk info\n");
    }
    needsSymRefreshModuleList = 0;
    uv_lib_t jl_dbghelp;
    uv_dlopen("dbghelp.dll",&jl_dbghelp);
    if (uv_dlsym(&jl_dbghelp, "SymRefreshModuleList", (void**)&hSymRefreshModuleList))
        hSymRefreshModuleList = 0;
    ULONG StackSizeInBytes = sig_stack_size;
    if (uv_dlsym(jl_kernel32_handle, "SetThreadStackGuarantee", (void**)&pSetThreadStackGuarantee) || !pSetThreadStackGuarantee(&StackSizeInBytes))
        pSetThreadStackGuarantee = NULL;
#endif
    init_stdio();

#if defined(JL_USE_INTEL_JITEVENTS)
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_intel_jitevents = 1;
#if defined(__linux__)
        // Intel VTune Amplifier needs at least 64k for alternate stack.
        if (SIGSTKSZ < 1<<16)
            sig_stack_size = 1<<16;
#endif
    }
#endif

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

    if (!jl_compileropts.image_file) {
        jl_core_module = jl_new_module(jl_symbol("Core"));
        jl_init_intrinsic_functions();
        jl_init_primitives();

        jl_new_main_module();
        jl_internal_main_module = jl_main_module;

        jl_current_module = jl_core_module;
        jl_root_task->current_module = jl_current_module;

        jl_load("boot.jl");
        jl_get_builtin_hooks();
        jl_boot_file_loaded = 1;
        jl_init_box_caches();
    }

    if (jl_compileropts.image_file) {
        JL_TRY {
            jl_restore_system_image(jl_compileropts.image_file);
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
    jl_root_task->current_module = jl_current_module;

#ifndef _OS_WINDOWS_
    signal_stack = malloc(sig_stack_size);
    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_handler = fpe_handler;
    actf.sa_flags = 0;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (signal(SIGPIPE,SIG_IGN) == SIG_ERR) {
        JL_PRINTF(JL_STDERR, "fatal error: Couldn't set SIGPIPE\n");
        jl_exit(1);
    }
#if defined (_OS_DARWIN_)
    kern_return_t ret;
    mach_port_t self = mach_task_self();
    ret = mach_port_allocate(self,MACH_PORT_RIGHT_RECEIVE,&segv_port);
    HANDLE_MACH_ERROR("mach_port_allocate",ret);
    ret = mach_port_insert_right(self,segv_port,segv_port,MACH_MSG_TYPE_MAKE_SEND);
    HANDLE_MACH_ERROR("mach_port_insert_right",ret);

    // Alright, create a thread to serve as the listener for exceptions
    pthread_t thread;
    pthread_attr_t attr;
    if (pthread_attr_init(&attr) != 0) {
        JL_PRINTF(JL_STDERR, "pthread_attr_init failed");
        jl_exit(1);
    }
    pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
    if (pthread_create(&thread,&attr,mach_segv_listener,NULL) != 0) {
        JL_PRINTF(JL_STDERR, "pthread_create failed");
        jl_exit(1);
    }
    pthread_attr_destroy(&attr);

    ret = thread_set_exception_ports(mach_thread_self(),EXC_MASK_BAD_ACCESS,segv_port,EXCEPTION_DEFAULT,MACHINE_THREAD_STATE);
    HANDLE_MACH_ERROR("thread_set_exception_ports",ret);
#else // defined(_OS_DARWIN_)
    stack_t ss;
    ss.ss_flags = 0;
    ss.ss_size = sig_stack_size;
    ss.ss_sp = signal_stack;
    if (sigaltstack(&ss, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaltstack: %s\n", strerror(errno));
        jl_exit(1);
    }

    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = segv_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    if (sigaction(SIGSEGV, &act, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
#endif // defined(_OS_DARWIN_)
    struct sigaction act_die;
    memset(&act_die, 0, sizeof(struct sigaction));
    sigemptyset(&act_die.sa_mask);
    act_die.sa_sigaction = sigdie_handler;
    act_die.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINFO, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGBUS, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGILL, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGTERM, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGABRT, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGQUIT, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGSYS, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
    if (sigaction(SIGPIPE, &act_die, NULL) < 0) {
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
#else // defined(_OS_WINDOWS_)
    if (signal(SIGFPE, (void (__cdecl *)(int))fpe_handler) == SIG_ERR) {
        JL_PRINTF(JL_STDERR, "fatal error: Couldn't set SIGFPE\n");
        jl_exit(1);
    }
    SetUnhandledExceptionFilter(exception_handler);
#endif

#ifdef JL_GC_MARKSWEEP
    jl_gc_enable();
#endif

    if (jl_compileropts.image_file)
        jl_init_restored_modules();

    jl_install_sigint_handler();
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
        JL_PRINTF(JL_STDERR, "fatal error: sigaction: %s\n", strerror(errno));
        jl_exit(1);
    }
#endif
}

extern int asprintf(char **str, const char *fmt, ...);
extern void *__stack_chk_guard;

void jl_compile_all(void);

DLLEXPORT void julia_save()
{
    const char *build_path = jl_compileropts.build_path;
    if (build_path) {
        if (jl_compileropts.compile_enabled == JL_COMPILEROPT_COMPILE_ALL)
            jl_compile_all();
        char *build_ji;
        if (asprintf(&build_ji, "%s.ji",build_path) > 0) {
            jl_save_system_image(build_ji);
            free(build_ji);
            if (jl_compileropts.dumpbitcode == JL_COMPILEROPT_DUMPBITCODE_ON) {
                char *build_bc;
                if (asprintf(&build_bc, "%s.bc",build_path) > 0) {
                    jl_dump_bitcode(build_bc);
                    free(build_bc);
                }
                else {
                    ios_printf(ios_stderr,"\nWARNING: failed to create string for .bc build path\n");
                }
            }
            char *build_o;
            if (asprintf(&build_o, "%s.o",build_path) > 0) {
                jl_dump_objfile(build_o,0);
                free(build_o);
            }
            else {
                ios_printf(ios_stderr,"\nFATAL: failed to create string for .o build path\n");
            }
        }
        else {
            ios_printf(ios_stderr,"\nFATAL: failed to create string for .ji build path\n");
        }
    }
}

jl_function_t *jl_typeinf_func=NULL;

DLLEXPORT void jl_enable_inference(void)
{
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
    jl_root_task->tls = jl_nothing;
    jl_root_task->consumers = jl_nothing;
    jl_root_task->donenotify = jl_nothing;
    jl_root_task->exception = jl_nothing;
    jl_root_task->result = jl_nothing;

    jl_char_type    = (jl_datatype_t*)core("Char");
    jl_int8_type    = (jl_datatype_t*)core("Int8");
    jl_uint8_type   = (jl_datatype_t*)core("UInt8");
    jl_int16_type   = (jl_datatype_t*)core("Int16");
    jl_uint16_type  = (jl_datatype_t*)core("UInt16");
    jl_uint32_type  = (jl_datatype_t*)core("UInt32");
    jl_uint64_type  = (jl_datatype_t*)core("UInt64");

    jl_float32_type = (jl_datatype_t*)core("Float32");
    jl_float64_type = (jl_datatype_t*)core("Float64");
    jl_floatingpoint_type = (jl_datatype_t*)core("FloatingPoint");
    jl_number_type = (jl_datatype_t*)core("Number");

    jl_stackovf_exception  = jl_new_struct((jl_datatype_t*)core("StackOverflowError"));
    jl_diverror_exception  = jl_new_struct((jl_datatype_t*)core("DivideError"));
    jl_domain_exception    = jl_new_struct((jl_datatype_t*)core("DomainError"));
    jl_overflow_exception  = jl_new_struct((jl_datatype_t*)core("OverflowError"));
    jl_inexact_exception   = jl_new_struct((jl_datatype_t*)core("InexactError"));
    jl_undefref_exception  = jl_new_struct((jl_datatype_t*)core("UndefRefError"));
    jl_undefvarerror_type  = (jl_datatype_t*)core("UndefVarError");
    jl_interrupt_exception = jl_new_struct((jl_datatype_t*)core("InterruptException"));
    jl_bounds_exception    = jl_new_struct((jl_datatype_t*)core("BoundsError"));
    jl_memory_exception    = jl_new_struct((jl_datatype_t*)core("MemoryError"));

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

DLLEXPORT void jl_exit_on_sigint(int on) {exit_on_sigint = on;}

#ifdef __cplusplus
}
#endif
