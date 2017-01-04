// This file is a part of Julia. License is MIT: http://julialang.org/license

// Windows

#define sig_stack_size 131072 // 128k reserved for SEGV handling
static BOOL (*pSetThreadStackGuarantee)(PULONG);

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

static char *strsignal(int sig)
{
    switch (sig) {
    case SIGINT:         return "SIGINT"; break;
    case SIGILL:         return "SIGILL"; break;
    case SIGABRT_COMPAT: return "SIGABRT_COMPAT"; break;
    case SIGFPE:         return "SIGFPE"; break;
    case SIGSEGV:        return "SIGSEGV"; break;
    case SIGTERM:        return "SIGTERM"; break;
    case SIGBREAK:       return "SIGBREAK"; break;
    case SIGABRT:        return "SIGABRT"; break;
    }
    return "?";
}

static void jl_try_throw_sigint(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_safepoint_enable_sigint();
    jl_wake_libuv();
    int force = jl_check_force_sigint();
    if (force || (!ptls->defer_signal && ptls->io_wait)) {
        jl_safepoint_consume_sigint();
        if (force)
            jl_safe_printf("WARNING: Force throwing a SIGINT\n");
        // Force a throw
        jl_clear_force_sigint();
        jl_throw(jl_interrupt_exception);
    }
}

void __cdecl crt_sig_handler(int sig, int num)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    CONTEXT Context;
    switch (sig) {
    case SIGFPE:
        fpreset();
        signal(SIGFPE, (void (__cdecl *)(int))crt_sig_handler);
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
        break;
    case SIGINT:
        signal(SIGINT, (void (__cdecl *)(int))crt_sig_handler);
        if (!jl_ignore_sigint()) {
            if (exit_on_sigint)
                jl_exit(130); // 128 + SIGINT
            jl_try_throw_sigint();
        }
        break;
    default: // SIGSEGV, (SSIGTERM, IGILL)
        memset(&Context, 0, sizeof(Context));
        RtlCaptureContext(&Context);
        jl_critical_error(sig, &Context, ptls->bt_data, &ptls->bt_size);
        raise(sig);
    }
}

void restore_signals(void)
{
    // turn on ctrl-c handler
    SetConsoleCtrlHandler(NULL, 0);
    // see if SetThreadStackGuarantee exists
    pSetThreadStackGuarantee = (BOOL (*)(PULONG)) jl_dlsym_e(jl_kernel32_handle,
        "SetThreadStackGuarantee");
}

void jl_throw_in_ctx(jl_value_t *excpt, CONTEXT *ctxThread, int bt)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(excpt != NULL);
#if defined(_CPU_X86_64_)
    DWORD64 Rsp = (ctxThread->Rsp&(DWORD64)-16) - 8;
#elif defined(_CPU_X86_)
    DWORD32 Esp = (ctxThread->Esp&(DWORD32)-16) - 4;
#else
#error WIN16 not supported :P
#endif
    ptls->bt_size = bt ? rec_backtrace_ctx(ptls->bt_data, JL_MAX_BT_SIZE,
                                           ctxThread) : 0;
    ptls->exception_in_transit = excpt;
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

HANDLE hMainThread = INVALID_HANDLE_VALUE;

// Try to throw the exception in the master thread.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_all_tls_states[0];
    jl_safepoint_enable_sigint();
    jl_wake_libuv();
    if ((DWORD)-1 == SuspendThread(hMainThread)) {
        // error
        jl_safe_printf("error: SuspendThread failed\n");
        return;
    }
    int force = jl_check_force_sigint();
    if (force || (!ptls2->defer_signal && ptls2->io_wait)) {
        jl_safepoint_consume_sigint();
        if (force)
            jl_safe_printf("WARNING: Force throwing a SIGINT\n");
        // Force a throw
        jl_clear_force_sigint();
        CONTEXT ctxThread;
        memset(&ctxThread, 0, sizeof(CONTEXT));
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!GetThreadContext(hMainThread, &ctxThread)) {
            // error
            jl_safe_printf("error: GetThreadContext failed\n");
            return;
        }
        jl_throw_in_ctx(jl_interrupt_exception, &ctxThread, 1);
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!SetThreadContext(hMainThread, &ctxThread)) {
            jl_safe_printf("error: SetThreadContext failed\n");
            // error
            return;
        }
    }
    if ((DWORD)-1 == ResumeThread(hMainThread)) {
        jl_safe_printf("error: ResumeThread failed\n");
        // error
        return;
    }
}

static BOOL WINAPI sigint_handler(DWORD wsig) //This needs winapi types to guarantee __stdcall
{
    int sig;
    //windows signals use different numbers from unix (raise)
    switch(wsig) {
        case CTRL_C_EVENT: sig = SIGINT; break;
        //case CTRL_BREAK_EVENT: sig = SIGTERM; break;
        // etc.
        default: sig = SIGTERM; break;
    }
    if (!jl_ignore_sigint()) {
        if (exit_on_sigint)
            jl_exit(128 + sig); // 128 + SIGINT
        jl_try_deliver_sigint();
    }
    return 1;
}

static LONG WINAPI _exception_handler(struct _EXCEPTION_POINTERS *ExceptionInfo, int in_ctx)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ExceptionInfo->ExceptionRecord->ExceptionFlags == 0) {
        switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
                fpreset();
                jl_throw_in_ctx(jl_diverror_exception,
                    ExceptionInfo->ContextRecord,in_ctx);
                return EXCEPTION_CONTINUE_EXECUTION;
            case EXCEPTION_STACK_OVERFLOW:
                jl_throw_in_ctx(jl_stackovf_exception,
                    ExceptionInfo->ContextRecord,in_ctx&&pSetThreadStackGuarantee);
                return EXCEPTION_CONTINUE_EXECUTION;
            case EXCEPTION_ACCESS_VIOLATION:
                if (jl_addr_is_safepoint(ExceptionInfo->ExceptionRecord->ExceptionInformation[1])) {
#ifdef JULIA_ENABLE_THREADING
                    jl_set_gc_and_wait();
                    // Do not raise sigint on worker thread
                    if (ptls->tid != 0)
                        return EXCEPTION_CONTINUE_EXECUTION;
#endif
                    if (ptls->defer_signal) {
                        jl_safepoint_defer_sigint();
                    }
                    else if (jl_safepoint_consume_sigint()) {
                        jl_clear_force_sigint();
                        jl_throw_in_ctx(jl_interrupt_exception,
                                        ExceptionInfo->ContextRecord, in_ctx);
                    }
                    return EXCEPTION_CONTINUE_EXECUTION;
                }
                if (ExceptionInfo->ExceptionRecord->ExceptionInformation[0] == 1) { // writing to read-only memory (e.g. mmap)
                    jl_throw_in_ctx(jl_readonlymemory_exception,
                        ExceptionInfo->ContextRecord,in_ctx);
                    return EXCEPTION_CONTINUE_EXECUTION;
                }
        }
        jl_safe_printf("\nPlease submit a bug report with steps to reproduce this fault, and any error messages that follow (in their entirety). Thanks.\nException: ");
        switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
            case EXCEPTION_ACCESS_VIOLATION:
                jl_safe_printf("EXCEPTION_ACCESS_VIOLATION"); break;
            case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
                jl_safe_printf("EXCEPTION_ARRAY_BOUNDS_EXCEEDED"); break;
            case EXCEPTION_BREAKPOINT:
                jl_safe_printf("EXCEPTION_BREAKPOINT"); break;
            case EXCEPTION_DATATYPE_MISALIGNMENT:
                jl_safe_printf("EXCEPTION_DATATYPE_MISALIGNMENT"); break;
            case EXCEPTION_FLT_DENORMAL_OPERAND:
                jl_safe_printf("EXCEPTION_FLT_DENORMAL_OPERAND"); break;
            case EXCEPTION_FLT_DIVIDE_BY_ZERO:
                jl_safe_printf("EXCEPTION_FLT_DIVIDE_BY_ZERO"); break;
            case EXCEPTION_FLT_INEXACT_RESULT:
                jl_safe_printf("EXCEPTION_FLT_INEXACT_RESULT"); break;
            case EXCEPTION_FLT_INVALID_OPERATION:
                jl_safe_printf("EXCEPTION_FLT_INVALID_OPERATION"); break;
            case EXCEPTION_FLT_OVERFLOW:
                jl_safe_printf("EXCEPTION_FLT_OVERFLOW"); break;
            case EXCEPTION_FLT_STACK_CHECK:
                jl_safe_printf("EXCEPTION_FLT_STACK_CHECK"); break;
            case EXCEPTION_FLT_UNDERFLOW:
                jl_safe_printf("EXCEPTION_FLT_UNDERFLOW"); break;
            case EXCEPTION_ILLEGAL_INSTRUCTION:
                jl_safe_printf("EXCEPTION_ILLEGAL_INSTRUCTION"); break;
            case EXCEPTION_IN_PAGE_ERROR:
                jl_safe_printf("EXCEPTION_IN_PAGE_ERROR"); break;
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
                jl_safe_printf("EXCEPTION_INT_DIVIDE_BY_ZERO"); break;
            case EXCEPTION_INT_OVERFLOW:
                jl_safe_printf("EXCEPTION_INT_OVERFLOW"); break;
            case EXCEPTION_INVALID_DISPOSITION:
                jl_safe_printf("EXCEPTION_INVALID_DISPOSITION"); break;
            case EXCEPTION_NONCONTINUABLE_EXCEPTION:
                jl_safe_printf("EXCEPTION_NONCONTINUABLE_EXCEPTION"); break;
            case EXCEPTION_PRIV_INSTRUCTION:
                jl_safe_printf("EXCEPTION_PRIV_INSTRUCTION"); break;
            case EXCEPTION_SINGLE_STEP:
                jl_safe_printf("EXCEPTION_SINGLE_STEP"); break;
            case EXCEPTION_STACK_OVERFLOW:
                jl_safe_printf("EXCEPTION_STACK_OVERFLOW"); break;
            default:
                jl_safe_printf("UNKNOWN"); break;
        }
        jl_safe_printf(" at 0x%Ix -- ", (size_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);
        jl_gdblookup((uintptr_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);

        jl_critical_error(0, ExceptionInfo->ContextRecord,
                          ptls->bt_data, &ptls->bt_size);
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
JL_DLLEXPORT EXCEPTION_DISPOSITION __julia_personality(
        PEXCEPTION_RECORD ExceptionRecord,
        void *EstablisherFrame,
        PCONTEXT ContextRecord,
        void *DispatcherContext)
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

JL_DLLEXPORT void jl_install_sigint_handler(void)
{
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)sigint_handler,1);
}

volatile HANDLE hBtThread = 0;
static DWORD WINAPI profile_bt( LPVOID lparam )
{
    // Note: illegal to use jl_* functions from this thread

    TIMECAPS tc;
    if (MMSYSERR_NOERROR!=timeGetDevCaps(&tc, sizeof(tc))) {
        fputs("failed to get timer resolution",stderr);
        hBtThread = 0;
        return 0;
    }
    while (1) {
        if (running && bt_size_cur < bt_size_max) {
            DWORD timeout = nsecprof/GIGA;
            timeout = min(max(timeout,tc.wPeriodMin*2),tc.wPeriodMax/2);
            Sleep(timeout);
            if ((DWORD)-1 == SuspendThread(hMainThread)) {
                fputs("failed to suspend main thread. aborting profiling.",stderr);
                break;
            }
            CONTEXT ctxThread;
            memset(&ctxThread, 0, sizeof(CONTEXT));
            ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
            if (!GetThreadContext(hMainThread, &ctxThread)) {
                fputs("failed to get context from main thread. aborting profiling.",stderr);
                break;
            }
            // Get backtrace data
            bt_size_cur += rec_backtrace_ctx((uintptr_t*)bt_data_prof + bt_size_cur,
                bt_size_max - bt_size_cur - 1, &ctxThread);
            // Mark the end of this block with 0
            bt_data_prof[bt_size_cur] = 0;
            bt_size_cur++;
            if ((DWORD)-1 == ResumeThread(hMainThread)) {
                fputs("failed to resume main thread! aborting.",stderr);
                gc_debug_critical_error();
                abort();
            }
        }
        else {
            SuspendThread(GetCurrentThread());
        }
    }
    hBtThread = 0;
    return 0;
}

JL_DLLEXPORT int jl_profile_start_timer(void)
{
    running = 1;
    if (hBtThread == 0) {
        hBtThread = CreateThread(
            NULL,                   // default security attributes
            0,                      // use default stack size
            profile_bt,            // thread function name
            0,                      // argument to thread function
            0,                      // use default creation flags
            0);                     // returns the thread identifier
        (void)SetThreadPriority(hBtThread,THREAD_PRIORITY_ABOVE_NORMAL);
    }
    else {
        if ((DWORD)-1 == ResumeThread(hBtThread)) {
            fputs("failed to resume profiling thread.",stderr);
            return -2;
        }
    }
    return (hBtThread != NULL ? 0 : -1);
}
JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    running = 0;
}

void jl_install_default_signal_handlers(void)
{
    if (signal(SIGFPE, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGFPE");
    }
    if (signal(SIGILL, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGILL");
    }
    if (signal(SIGINT, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGINT");
    }
    if (signal(SIGSEGV, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGSEGV");
    }
    if (signal(SIGTERM, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGTERM");
    }
    if (signal(SIGABRT, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGABRT");
    }
    SetUnhandledExceptionFilter(exception_handler);
}

void jl_install_thread_signal_handler(jl_ptls_t ptls)
{
    (void)ptls;
    // Ensure the stack overflow handler has enough space to collect the backtrace
    ULONG StackSizeInBytes = sig_stack_size;
    if (pSetThreadStackGuarantee) {
        if (!pSetThreadStackGuarantee(&StackSizeInBytes)) {
            pSetThreadStackGuarantee = NULL;
        }
    }
}
