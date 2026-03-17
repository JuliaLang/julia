// This file is a part of Julia. License is MIT: https://julialang.org/license
//
// Tests Mach exception forwarding in --handle-signals=minimal mode (macOS):
//   1. EXC_BAD_ACCESS forwarding to a pre-existing Mach exception handler
//   2. Multi-threaded workload with safepoints
//   3. Forwarded exception parameters match direct delivery
//
// The test installs a raw Mach exception handler (using mach_msg directly,
// without MIG) before Julia initialization. Julia's init saves our handler
// via thread_swap_exception_ports. We then verify that:
//   - Before Julia init: our handler receives exceptions directly from the kernel
//   - After Julia init: Julia forwards unhandled exceptions to our saved handler
//   - The exception parameters are identical in both cases

#include <julia.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <pthread.h>
#include <mach/mach.h>
#include <assert.h>

JULIA_DEFINE_FAST_TLS

// --- Raw Mach exception message structures ---
// These match the MIG layout for EXCEPTION_DEFAULT | MACH_EXCEPTION_CODES
// (mach_exception_raise, message ID 2405).

typedef struct {
    mach_msg_header_t Head;
    mach_msg_body_t msgh_body;
    mach_msg_port_descriptor_t thread;
    mach_msg_port_descriptor_t task;
    NDR_record_t NDR;
    exception_type_t exception;
    mach_msg_type_number_t codeCnt;
    int64_t code[2];
} exc_request_t;

typedef struct {
    mach_msg_header_t Head;
    NDR_record_t NDR;
    kern_return_t RetCode;
} exc_reply_t;

// --- Exception records for parameter comparison ---

typedef struct {
    mach_port_t exception_port;
    mach_port_t thread;
    mach_port_t task;
    exception_type_t exception;
    int64_t code[2];
    mach_msg_type_number_t codeCnt;
    int received;
} exc_record_t;

#define MAX_EXC_RECORDS 4
static exc_record_t exc_records[MAX_EXC_RECORDS];
static volatile int exc_count = 0;

static mach_port_t test_exc_port = MACH_PORT_NULL;
static jmp_buf exc_jmpbuf;

// --- Mach exception listener thread ---
// Receives and replies to raw mach_msg exception messages without MIG,
// to avoid symbol conflicts with Julia's catch_mach_exception_raise.

static void *exc_listener_thread(void *arg)
{
    (void)arg;
    while (1) {
        // Receive buffer with space for the request + trailer
        struct {
            exc_request_t req;
            mach_msg_audit_trailer_t trailer;
        } buffer;

        mach_msg_return_t mr = mach_msg(&buffer.req.Head, MACH_RCV_MSG, 0,
            sizeof(buffer), test_exc_port,
            MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        if (mr != MACH_MSG_SUCCESS) {
            fprintf(stderr, "mach_msg receive failed: 0x%x\n", mr);
            continue;
        }

        // We only handle EXCEPTION_DEFAULT | MACH_EXCEPTION_CODES (msg ID 2405)
        if (buffer.req.Head.msgh_id != 2405) {
            fprintf(stderr, "Unexpected mach exception message ID: %d\n",
                buffer.req.Head.msgh_id);
            continue;
        }

        // Record exception parameters for later comparison
        int idx = __sync_fetch_and_add((int *)&exc_count, 1);
        if (idx < MAX_EXC_RECORDS) {
            exc_records[idx].exception_port = buffer.req.Head.msgh_local_port;
            exc_records[idx].thread = buffer.req.thread.name;
            exc_records[idx].task = buffer.req.task.name;
            exc_records[idx].exception = buffer.req.exception;
            exc_records[idx].codeCnt = buffer.req.codeCnt;
            exc_records[idx].code[0] = buffer.req.codeCnt > 0 ? buffer.req.code[0] : 0;
            exc_records[idx].code[1] = buffer.req.codeCnt > 1 ? buffer.req.code[1] : 0;
            exc_records[idx].received = 1;
        }

        // Recover the faulting thread by modifying its state to call
        // _longjmp(exc_jmpbuf, 1). This is the same technique Julia uses
        // in jl_throw_in_thread / jl_call_in_state1.
        mach_port_t thread = buffer.req.thread.name;
#if defined(__x86_64__)
        x86_thread_state64_t state;
        mach_msg_type_number_t count = x86_THREAD_STATE64_COUNT;
        kern_return_t kr = thread_get_state(thread, x86_THREAD_STATE64,
            (thread_state_t)&state, &count);
        if (kr == KERN_SUCCESS) {
            uintptr_t sp = (state.__rsp - 256) & ~(uintptr_t)15;
            state.__rsp = sp;
            state.__rip = (uint64_t)_longjmp;
            state.__rdi = (uint64_t)exc_jmpbuf;
            state.__rsi = 1;
            thread_set_state(thread, x86_THREAD_STATE64,
                (thread_state_t)&state, count);
        }
#elif defined(__aarch64__)
        arm_thread_state64_t state;
        mach_msg_type_number_t count = ARM_THREAD_STATE64_COUNT;
        kern_return_t kr = thread_get_state(thread, ARM_THREAD_STATE64,
            (thread_state_t)&state, &count);
        if (kr == KERN_SUCCESS) {
            uintptr_t sp = (state.__sp - 256) & ~(uintptr_t)15;
            state.__sp = sp;
            state.__pc = (uint64_t)_longjmp;
            state.__x[0] = (uint64_t)exc_jmpbuf;
            state.__x[1] = 1;
            thread_set_state(thread, ARM_THREAD_STATE64,
                (thread_state_t)&state, count);
        }
#else
#error "Unsupported architecture"
#endif

        // Send MIG reply
        exc_reply_t reply;
        memset(&reply, 0, sizeof(reply));
        reply.Head.msgh_bits = MACH_MSGH_BITS(
            MACH_MSGH_BITS_REMOTE(buffer.req.Head.msgh_bits), 0);
        reply.Head.msgh_remote_port = buffer.req.Head.msgh_remote_port;
        reply.Head.msgh_local_port = MACH_PORT_NULL;
        reply.Head.msgh_size = sizeof(reply);
        reply.Head.msgh_id = buffer.req.Head.msgh_id + 100;
        reply.NDR = NDR_record;
        reply.RetCode = KERN_SUCCESS;

        mr = mach_msg(&reply.Head, MACH_SEND_MSG, sizeof(reply), 0,
            MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        if (mr != MACH_MSG_SUCCESS) {
            fprintf(stderr, "mach_msg send reply failed: 0x%x\n", mr);
        }
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
    // --- Setup: Create Mach exception port and start listener thread ---

    kern_return_t kr;
    mach_port_t self = mach_task_self();

    kr = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &test_exc_port);
    if (kr != KERN_SUCCESS) {
        fprintf(stderr, "mach_port_allocate failed: 0x%x\n", kr);
        return 1;
    }
    kr = mach_port_insert_right(self, test_exc_port, test_exc_port,
        MACH_MSG_TYPE_MAKE_SEND);
    if (kr != KERN_SUCCESS) {
        fprintf(stderr, "mach_port_insert_right failed: 0x%x\n", kr);
        return 1;
    }

    pthread_t listener;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&listener, &attr, exc_listener_thread, NULL) != 0) {
        perror("pthread_create");
        return 1;
    }
    pthread_attr_destroy(&attr);

    // Install our Mach exception handler on the main thread for EXC_BAD_ACCESS.
    // Julia's init will later save this via thread_swap_exception_ports.
    kr = thread_set_exception_ports(mach_thread_self(),
        EXC_MASK_BAD_ACCESS, test_exc_port,
        EXCEPTION_DEFAULT | MACH_EXCEPTION_CODES,
#if defined(__x86_64__)
        x86_THREAD_STATE64
#elif defined(__aarch64__)
        ARM_THREAD_STATE64
#endif
    );
    if (kr != KERN_SUCCESS) {
        fprintf(stderr, "thread_set_exception_ports failed: 0x%x\n", kr);
        return 1;
    }

    // --- Phase 1: Direct delivery (before Julia init) ---
    // Our handler receives the exception directly from the kernel.
    {
        if (_setjmp(exc_jmpbuf) == 0) {
            volatile int *p = NULL;
            *p = 42; // EXC_BAD_ACCESS with KERN_INVALID_ADDRESS
        }
        // Recovered: handler called _longjmp(exc_jmpbuf, 1)
    }

    // --- Phase 2: Initialize Julia ---
    jl_options.handle_signals = JL_OPTIONS_HANDLE_SIGNALS_MINIMAL;

    jl_options.nthreadpools = 2;
    jl_options.nthreads = 4;
    int16_t *nthreads_per_pool = (int16_t *)malloc(sizeof(int16_t) * 2);
    nthreads_per_pool[0] = 2;
    nthreads_per_pool[1] = 2;
    jl_options.nthreads_per_pool = nthreads_per_pool;

    jl_init();
    // Julia saved our exception port and installed its own via
    // thread_swap_exception_ports in attach_exception_port.

    // --- Phase 3: Forwarded delivery (after Julia init) ---
    // The main thread is now a Julia thread with Julia's Mach exception port.
    // A NULL dereference triggers EXC_BAD_ACCESS with KERN_INVALID_ADDRESS,
    // which Julia's catch_mach_exception_raise cannot handle (it only handles
    // KERN_PROTECTION_FAILURE for safepoints/stack overflow/write faults).
    // Julia forwards the exception to our saved handler.
    {
        if (_setjmp(exc_jmpbuf) == 0) {
            volatile int *p = NULL;
            *p = 42; // EXC_BAD_ACCESS → Julia forwards → our handler
        }
        // Recovered: handler called _longjmp(exc_jmpbuf, 1)
    }

    // --- Report results ---

    // Test 1: Was the exception forwarded after Julia init?
    int forwarded = (exc_count >= 2 && exc_records[1].received);
    printf("Mach exception forwarded: %s\n", forwarded ? "OK" : "FAIL");
    fflush(stdout);

    // Test 2: Multi-threaded workload (GC safepoints use Mach exceptions)
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

    // Test 3: Compare parameters between direct and forwarded delivery.
    // All fields should be identical — the forwarding should be transparent.
    if (exc_count >= 2) {
        int match = 1;
        const char *mismatch = NULL;

        if (exc_records[0].exception != exc_records[1].exception) {
            match = 0; mismatch = "exception type";
        }
        else if (exc_records[0].codeCnt != exc_records[1].codeCnt) {
            match = 0; mismatch = "codeCnt";
        }
        else if (exc_records[0].code[0] != exc_records[1].code[0]) {
            match = 0; mismatch = "code[0]";
        }
        else if (exc_records[0].code[1] != exc_records[1].code[1]) {
            match = 0; mismatch = "code[1] (fault address)";
        }
        else if (exc_records[0].exception_port != exc_records[1].exception_port) {
            match = 0; mismatch = "exception_port";
        }
        else if (exc_records[0].task != exc_records[1].task) {
            match = 0; mismatch = "task port";
        }
        // Thread port should match since both faults are on the main thread
        else if (exc_records[0].thread != exc_records[1].thread) {
            match = 0; mismatch = "thread port";
        }

        printf("Parameters preserved: %s\n", match ? "OK" : mismatch);
    }
    else {
        printf("Parameters preserved: FAIL (only %d records)\n", exc_count);
    }
    fflush(stdout);

    jl_atexit_hook(0);
    return 0;
}
