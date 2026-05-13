// This file is a part of Julia. License is MIT: https://julialang.org/license
//
// Tests signal handling in --handle-signals=minimal mode (Unix):
//   1. SIGSEGV forwarding to a pre-existing sigaction handler
//   2. Multi-threaded workload with safepoints
//   3. SIGINFO/SIGUSR1 handler not installed

#include <julia.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include <pthread.h>

JULIA_DEFINE_FAST_TLS

// --- Test 1: SIGSEGV forwarding to pre-existing handler ---

static volatile int segv_forwarded = 0;
static sigjmp_buf segv_jmpbuf;

static void test_segv_handler(int sig, siginfo_t *info, void *ctx)
{
    segv_forwarded = 1;
    siglongjmp(segv_jmpbuf, 1);
}

static void *trigger_segv_thread(void *arg)
{
    // Trigger a SIGSEGV on a non-Julia thread.
    // Julia's handler should forward this to our pre-existing handler.
    if (sigsetjmp(segv_jmpbuf, 1) == 0) {
        volatile int *p = NULL;
        *p = 42;
    }
    return NULL;
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
    // Install SIGSEGV handler before Julia initialization
    struct sigaction sa;
    memset(&sa, 0, sizeof(struct sigaction));
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = test_segv_handler;
    sa.sa_flags = SA_SIGINFO;
    if (sigaction(SIGSEGV, &sa, NULL) < 0) {
        perror("sigaction");
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

    // Test 1: Exception forwarding to pre-existing handler
    {
        pthread_t tid;
        if (pthread_create(&tid, NULL, trigger_segv_thread, NULL) != 0) {
            perror("pthread_create");
            return 1;
        }
        pthread_join(tid, NULL);
        printf("SIGSEGV forwarded: %s\n", segv_forwarded ? "OK" : "FAIL");
        fflush(stdout);
    }

    // Test 2: Multi-threaded workload, which relies on Julia SIGSEGV handler for GC
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

    // Test 3: I/O-like signal handlers should not be installed in minimal mode
    {
        struct sigaction current;
#ifdef SIGINFO
        int test_sig = SIGINFO;
        const char *sig_name = "SIGINFO";
#else
        int test_sig = SIGUSR1;
        const char *sig_name = "SIGUSR1";
#endif
        sigaction(test_sig, NULL, &current);
        int is_default = (current.sa_handler == SIG_DFL);
        printf("%s handler: %s\n", sig_name, is_default ? "SIG_DFL" : "custom");
        fflush(stdout);
    }

    jl_atexit_hook(0);
    return 0;
}
