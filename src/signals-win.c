// This file is a part of Julia. License is MIT: http://julialang.org/license

// Windows
//
DLLEXPORT void gdblookup(ptrint_t ip);
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

void __cdecl crt_sig_handler(int sig, int num)
{
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
        if (jl_defer_signal) {
            jl_signal_pending = sig;
        }
        else {
            jl_signal_pending = 0;
            jl_sigint_action();
        }
        break;
    default: // SIGSEGV, (SSIGTERM, IGILL)
        memset(&Context, 0, sizeof(Context));
        RtlCaptureContext(&Context);
        jl_critical_error(sig, &Context, bt_data, &bt_size);
        raise(sig);
    }
}

BOOL (*pSetThreadStackGuarantee)(PULONG);
void restore_signals(void)
{
    // Ensure the stack overflow handler has enough space to collect the backtrace
    ULONG StackSizeInBytes = sig_stack_size;
    pSetThreadStackGuarantee = (BOOL (*)(PULONG)) jl_dlsym_e(jl_kernel32_handle, "SetThreadStackGuarantee");
    if (!pSetThreadStackGuarantee || !pSetThreadStackGuarantee(&StackSizeInBytes))
        pSetThreadStackGuarantee = NULL;
    //turn on ctrl-c handler
    SetConsoleCtrlHandler(NULL, 0);
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
    int sig;
    //windows signals use different numbers from unix (raise)
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
        if (exit_on_sigint) jl_exit(130);
        if ((DWORD)-1 == SuspendThread(hMainThread)) {
            //error
            jl_safe_printf("error: SuspendThread failed\n");
            return 0;
        }
        CONTEXT ctxThread;
        memset(&ctxThread,0,sizeof(CONTEXT));
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!GetThreadContext(hMainThread, &ctxThread)) {
            //error
            jl_safe_printf("error: GetThreadContext failed\n");
            return 0;
        }
        jl_throw_in_ctx(jl_interrupt_exception, &ctxThread, 1);
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!SetThreadContext(hMainThread,&ctxThread)) {
            jl_safe_printf("error: SetThreadContext failed\n");
            //error
            return 0;
        }
        if ((DWORD)-1 == ResumeThread(hMainThread)) {
            jl_safe_printf("error: ResumeThread failed\n");
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
            case EXCEPTION_ACCESS_VIOLATION:
                if (ExceptionInfo->ExceptionRecord->ExceptionInformation[0] == 1) { // writing to read-only memory (e.g. mmap)
                    jl_throw_in_ctx(jl_readonlymemory_exception, ExceptionInfo->ContextRecord,in_ctx);
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
        gdblookup((ptrint_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);

        jl_critical_error(0, ExceptionInfo->ContextRecord, bt_data, &bt_size);
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

DLLEXPORT void jl_install_sigint_handler(void)
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
            memset(&ctxThread,0,sizeof(CONTEXT));
            ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
            if (!GetThreadContext(hMainThread, &ctxThread)) {
                fputs("failed to get context from main thread. aborting profiling.",stderr);
                break;
            }
            // Get backtrace data
            bt_size_cur += rec_backtrace_ctx((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1, &ctxThread);
            // Mark the end of this block with 0
            bt_data_prof[bt_size_cur] = 0;
            bt_size_cur++;
            if ((DWORD)-1 == ResumeThread(hMainThread)) {
                fputs("failed to resume main thread! aborting.",stderr);
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
DLLEXPORT int jl_profile_start_timer(void)
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
DLLEXPORT void jl_profile_stop_timer(void)
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
    SetUnhandledExceptionFilter(exception_handler);
}
