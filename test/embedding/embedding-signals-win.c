// This file is a part of Julia. License is MIT: https://julialang.org/license
//
// Tests signal handling in --handle-signals=minimal mode (Windows):
//   1. EXCEPTION_ACCESS_VIOLATION forwarding to a pre-existing VEH handler
//   2. Multi-threaded workload with safepoints
//   3. ConsoleCtrlHandler not replaced by Julia

#include <julia.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <pthread.h>
#include <windows.h>

JULIA_DEFINE_FAST_TLS

// --- Test 1: EXCEPTION_ACCESS_VIOLATION forwarding to pre-existing VEH handler ---

static volatile int segv_forwarded = 0;
static jmp_buf segv_jmpbuf;

static LONG WINAPI test_segv_handler(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
    if (ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        segv_forwarded = 1;
        longjmp(segv_jmpbuf, 1);
    }
    return EXCEPTION_CONTINUE_SEARCH;
}

static void *trigger_segv_thread(void *arg)
{
    // Trigger an access violation on a non-Julia thread.
    // Julia's VEH handler should forward this to our pre-existing handler.
    if (setjmp(segv_jmpbuf) == 0) {
        volatile int *p = NULL;
        *p = 42;
    }
    return NULL;
}

// --- Test 3: ConsoleCtrlHandler not replaced by Julia ---

static volatile LONG ctrl_c_received = 0;

static BOOL WINAPI test_ctrl_handler(DWORD dwCtrlType)
{
    if (dwCtrlType == CTRL_C_EVENT) {
        InterlockedExchange(&ctrl_c_received, 1);
        return TRUE;
    }
    return FALSE;
}

// --- Helpers ---

static jl_value_t *checked_eval_string(const char *code)
{
    jl_value_t *result = jl_eval_string(code);
    if (jl_exception_occurred()) {
        jl_call2(jl_get_function(jl_base_module, "showerror"),
                 jl_stderr_obj(),
                 jl_exception_occurred());
        jl_printf(jl_stderr_stream(), "\n");
        jl_atexit_hook(1);
        exit(1);
    }
    assert(result && "Missing return value but no exception occurred!");
    return result;
}

int main()
{
    // Re-enable Ctrl-C events (disabled by CREATE_NEW_PROCESS_GROUP) and install our handler
    SetConsoleCtrlHandler(NULL, FALSE);
    SetConsoleCtrlHandler(test_ctrl_handler, TRUE);

    // Install VEH and ConsoleCtrlHandler before Julia initialization
    PVOID veh_handle = AddVectoredExceptionHandler(0, test_segv_handler);
    if (veh_handle == NULL) {
        fprintf(stderr, "AddVectoredExceptionHandler failed\n");
        return 1;
    }

    // Configure minimal signal handling
    jl_options.handle_signals = JL_OPTIONS_HANDLE_SIGNALS_MINIMAL;

    // Setup multi-threading with 4 threads (2 interactive)
    jl_options.nthreadpools = 2;
    jl_options.nthreads = 4;
    int16_t *nthreads_per_pool = (int16_t *)malloc(sizeof(int16_t) * jl_options.nthreadpools);
    nthreads_per_pool[0] = 2;
    nthreads_per_pool[1] = 2;
    jl_options.nthreads_per_pool = nthreads_per_pool;

    // Initialize Julia
    jl_init();

    // Test 1: Exception forwarding to pre-existing VEH handler
    {
        pthread_t tid;
        if (pthread_create(&tid, NULL, trigger_segv_thread, NULL) != 0) {
            fprintf(stderr, "pthread_create failed\n");
            return 1;
        }
        pthread_join(tid, NULL);
        printf("SIGSEGV forwarded: %s\n", segv_forwarded ? "OK" : "FAIL");
        fflush(stdout);
    }

    // Test 2: Multi-threaded workload, which relies on Julia's exception
    // handler for GC safepoints
    {
        checked_eval_string(
            "let\n"
            "    Threads.@threads for i in 1:1000\n"
            "        zeros(1000)\n"
            "    end\n"
            "    println(\"threading: nthreads=$(Threads.nthreads())\")\n"
            "end\n"
        );
        fflush(stdout);
    }

    // Test 3: ConsoleCtrlHandler should not be replaced in minimal mode
    {
        GenerateConsoleCtrlEvent(CTRL_C_EVENT, GetCurrentProcessId());
        Sleep(100); // ConsoleCtrlHandler is invoked asynchronously
        printf("ConsoleCtrlHandler: %s\n", ctrl_c_received ? "OK" : "not called");
        fflush(stdout);
    }

    jl_atexit_hook(0);
    return 0;
}
