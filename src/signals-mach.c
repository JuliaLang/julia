// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <mach/clock.h>
#include <mach/clock_types.h>
#include <mach/clock_reply.h>
#include <mach/mach_traps.h>
#include <mach/task.h>
#include <mach/mig_errors.h>
#include <AvailabilityMacros.h>

#ifdef MAC_OS_X_VERSION_10_9
#include <sys/_types/_ucontext64.h>
#else
#define __need_ucontext64_t
#include <sys/_structs.h>
#endif

#include "julia_assert.h"

static void attach_exception_port(thread_port_t thread, int segv_only);

// low 16 bits are the thread id, the next 8 bits are the original gc_state
static arraylist_t suspended_threads;
void jl_mach_gc_end(void)
{
    // Requires the safepoint lock to be held
    for (size_t i = 0;i < suspended_threads.len;i++) {
        uintptr_t item = (uintptr_t)suspended_threads.items[i];
        int16_t tid = (int16_t)item;
        int8_t gc_state = (int8_t)(item >> 8);
        jl_ptls_t ptls2 = jl_all_tls_states[tid];
        jl_atomic_store_release(&ptls2->gc_state, gc_state);
        thread_resume(pthread_mach_thread_np((pthread_t)ptls2->system_id));
    }
    suspended_threads.len = 0;
}

// Suspend the thread and return `1` if the GC is running.
// Otherwise return `0`
static int jl_mach_gc_wait(jl_ptls_t ptls2,
                           mach_port_t thread, int16_t tid)
{
    jl_mutex_lock_nogc(&safepoint_lock);
    if (!jl_gc_running) {
        // GC is done before we get the message or the safepoint is enabled
        // for SIGINT.
        jl_mutex_unlock_nogc(&safepoint_lock);
        return 0;
    }
    // Otherwise, set the gc state of the thread, suspend and record it
    int8_t gc_state = ptls2->gc_state;
    jl_atomic_store_release(&ptls2->gc_state, JL_GC_STATE_WAITING);
    uintptr_t item = tid | (((uintptr_t)gc_state) << 16);
    arraylist_push(&suspended_threads, (void*)item);
    thread_suspend(thread);
    jl_mutex_unlock_nogc(&safepoint_lock);
    return 1;
}

static mach_port_t segv_port = 0;

extern boolean_t exc_server(mach_msg_header_t *, mach_msg_header_t *);

#define STR(x) #x
#define XSTR(x) STR(x)
#define HANDLE_MACH_ERROR(msg, retval) \
    if (retval != KERN_SUCCESS) { mach_error(msg XSTR(: __FILE__:__LINE__:), (retval)); jl_exit(1); }

void *mach_segv_listener(void *arg)
{
    (void)arg;
    while (1) {
        int ret = mach_msg_server(exc_server, 2048, segv_port, MACH_MSG_TIMEOUT_NONE);
        jl_safe_printf("mach_msg_server: %s\n", mach_error_string(ret));
        jl_exit(128 + SIGSEGV);
    }
}

static void allocate_segv_handler()
{
    arraylist_new(&suspended_threads, jl_n_threads);
    pthread_t thread;
    pthread_attr_t attr;
    kern_return_t ret;
    mach_port_t self = mach_task_self();
    ret = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &segv_port);
    HANDLE_MACH_ERROR("mach_port_allocate",ret);
    ret = mach_port_insert_right(self, segv_port, segv_port, MACH_MSG_TYPE_MAKE_SEND);
    HANDLE_MACH_ERROR("mach_port_insert_right",ret);
    // Alright, create a thread to serve as the listener for exceptions
    if (pthread_attr_init(&attr) != 0) {
        jl_error("pthread_attr_init failed");
    }
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&thread, &attr, mach_segv_listener, NULL) != 0) {
        jl_error("pthread_create failed");
    }
    pthread_attr_destroy(&attr);
    for (int16_t tid = 0;tid < jl_n_threads;tid++) {
        attach_exception_port(pthread_mach_thread_np((pthread_t)jl_all_tls_states[tid]->system_id), 0);
    }
}

#ifdef LIBOSXUNWIND
volatile mach_port_t mach_profiler_thread = 0;
static kern_return_t profiler_segv_handler
                (mach_port_t                          exception_port,
                 mach_port_t                                  thread,
                 mach_port_t                                    task,
                 exception_type_t                          exception,
                 exception_data_t                               code,
                 mach_msg_type_number_t                   code_count);
#endif

#if defined(_CPU_X86_64_)
typedef x86_thread_state64_t host_thread_state_t;
typedef x86_exception_state64_t host_exception_state_t;
#define THREAD_STATE x86_THREAD_STATE64
#define THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define HOST_EXCEPTION_STATE x86_EXCEPTION_STATE64
#define HOST_EXCEPTION_STATE_COUNT x86_EXCEPTION_STATE64_COUNT

enum x86_trap_flags {
    USER_MODE = 0x4,
    WRITE_FAULT = 0x2,
    PAGE_PRESENT = 0x1
};

#elif defined(_CPU_AARCH64_)
typedef arm_thread_state64_t host_thread_state_t;
typedef arm_exception_state64_t host_exception_state_t;
#define THREAD_STATE ARM_THREAD_STATE64
#define THREAD_STATE_COUNT ARM_THREAD_STATE64_COUNT
#define HOST_EXCEPTION_STATE ARM_EXCEPTION_STATE64
#define HOST_EXCEPTION_STATE_COUNT ARM_EXCEPTION_STATE64_COUNT

enum aarch64_esr_layout {
    EC_MASK = ((uint32_t)0b111111) << 25,
    EC_DATA_ABORT = ((uint32_t)0b100100) << 25,
    ISR_DA_WnR = ((uint32_t)1) << 6
};
#endif

static void jl_call_in_state(jl_ptls_t ptls2, host_thread_state_t *state,
                             void (*fptr)(void))
{
    uint64_t rsp = (uint64_t)ptls2->signal_stack + sig_stack_size;
    assert(rsp % 16 == 0);

#ifdef _CPU_X86_64_
    // push (null) $RIP onto the stack
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;

    state->__rsp = rsp; // set stack pointer
    state->__rip = (uint64_t)fptr; // "call" the function
#else
    state->__sp = rsp;
    state->__pc = (uint64_t)fptr;
#endif
}

static void jl_throw_in_thread(int tid, mach_port_t thread, jl_value_t *exception)
{
    unsigned int count = THREAD_STATE_COUNT;
    host_thread_state_t state;
    kern_return_t ret = thread_get_state(thread, THREAD_STATE, (thread_state_t)&state, &count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    if (!ptls2->safe_restore) {
        assert(exception);
        ptls2->bt_size = rec_backtrace_ctx(ptls2->bt_data, JL_MAX_BT_SIZE,
                                           (bt_context_t*)&state, ptls2->pgcstack, 0);
        ptls2->sig_exception = exception;
    }
    jl_call_in_state(ptls2, &state, &jl_sig_throw);
    ret = thread_set_state(thread, THREAD_STATE,
                           (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);
}

//exc_server uses dlsym to find symbol
JL_DLLEXPORT
kern_return_t catch_exception_raise(mach_port_t            exception_port,
                                    mach_port_t            thread,
                                    mach_port_t            task,
                                    exception_type_t       exception,
                                    exception_data_t       code,
                                    mach_msg_type_number_t code_count)
{
    unsigned int count = THREAD_STATE_COUNT;
    unsigned int exc_count = HOST_EXCEPTION_STATE_COUNT;
    host_exception_state_t exc_state;
    host_thread_state_t state;
#ifdef LIBOSXUNWIND
    if (thread == mach_profiler_thread) {
        return profiler_segv_handler(exception_port, thread, task, exception, code, code_count);
    }
#endif
    int16_t tid;
    jl_ptls_t ptls2 = NULL;
    for (tid = 0;tid < jl_n_threads;tid++) {
        jl_ptls_t _ptls2 = jl_all_tls_states[tid];
        if (pthread_mach_thread_np((pthread_t)_ptls2->system_id) == thread) {
            ptls2 = _ptls2;
            break;
        }
    }
    if (!ptls2) {
        // We don't know about this thread, let the kernel try another handler
        // instead. This shouldn't actually happen since we only register the
        // handler for the threads we know about.
        jl_safe_printf("ERROR: Exception handler triggered on unmanaged thread.\n");
        return KERN_INVALID_ARGUMENT;
    }
    if (exception == EXC_ARITHMETIC) {
        jl_throw_in_thread(tid, thread, jl_diverror_exception);
        return KERN_SUCCESS;
    }
    assert(exception == EXC_BAD_ACCESS);
    kern_return_t ret = thread_get_state(thread, HOST_EXCEPTION_STATE, (thread_state_t)&exc_state, &exc_count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
#ifdef _CPU_X86_64_
    uint64_t fault_addr = exc_state.__faultvaddr;
#else
    uint64_t fault_addr = exc_state.__far;
#endif
    if (jl_addr_is_safepoint(fault_addr)) {
        if (jl_mach_gc_wait(ptls2, thread, tid))
            return KERN_SUCCESS;
        if (ptls2->tid != 0)
            return KERN_SUCCESS;
        if (ptls2->defer_signal) {
            jl_safepoint_defer_sigint();
        }
        else if (jl_safepoint_consume_sigint()) {
            jl_clear_force_sigint();
            jl_throw_in_thread(tid, thread, jl_interrupt_exception);
        }
        return KERN_SUCCESS;
    }
    if (ptls2->safe_restore) {
        jl_throw_in_thread(tid, thread, jl_stackovf_exception);
        return KERN_SUCCESS;
    }
#ifdef SEGV_EXCEPTION
    if (1) {
#else
    if (msync((void*)(fault_addr & ~(jl_page_size - 1)), 1, MS_ASYNC) == 0) { // check if this was a valid address
#endif
        jl_value_t *excpt;
        if (is_addr_on_stack(ptls2, (void*)fault_addr)) {
            excpt = jl_stackovf_exception;
        }
#ifdef SEGV_EXCEPTION
        else if (msync((void*)(fault_addr & ~(jl_page_size - 1)), 1, MS_ASYNC) != 0) {
            // no page mapped at this address
            excpt = jl_segv_exception;
        }
#endif
        else {
#ifdef _CPU_X86_64_
            if (!(exc_state.__err & WRITE_FAULT))
                return KERN_INVALID_ARGUMENT; // rethrow the SEGV since it wasn't an error with writing to read-only memory
#else
            uint32_t esr = exc_state.__esr;
            if ((esr & EC_MASK) != EC_DATA_ABORT || !(esr & ISR_DA_WnR)) {
                return KERN_INVALID_ARGUMENT;
            }
#endif
            excpt = jl_readonlymemory_exception;
        }
        jl_throw_in_thread(tid, thread, excpt);

        return KERN_SUCCESS;
    }
    else {
        kern_return_t ret = thread_get_state(thread, THREAD_STATE, (thread_state_t)&state, &count);
        HANDLE_MACH_ERROR("thread_get_state", ret);
        jl_critical_error(SIGSEGV, (unw_context_t*)&state,
                          ptls2->bt_data, &ptls2->bt_size);
        return KERN_INVALID_ARGUMENT;
    }
}

static void attach_exception_port(thread_port_t thread, int segv_only)
{
    kern_return_t ret;
    // http://www.opensource.apple.com/source/xnu/xnu-2782.1.97/osfmk/man/thread_set_exception_ports.html
    exception_mask_t mask = EXC_MASK_BAD_ACCESS;
    if (!segv_only)
        mask |= EXC_MASK_ARITHMETIC;
    ret = thread_set_exception_ports(thread, mask, segv_port, EXCEPTION_DEFAULT, MACHINE_THREAD_STATE);
    HANDLE_MACH_ERROR("thread_set_exception_ports", ret);
}

static void jl_thread_suspend_and_get_state(int tid, unw_context_t **ctx)
{
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    mach_port_t tid_port = pthread_mach_thread_np((pthread_t)ptls2->system_id);

    kern_return_t ret = thread_suspend(tid_port);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // Do the actual sampling
    unsigned int count = THREAD_STATE_COUNT;
    static unw_context_t state;
    memset(&state, 0, sizeof(unw_context_t));

    // Get the state of the suspended thread
    ret = thread_get_state(tid_port, THREAD_STATE, (thread_state_t)&state, &count);

    // Initialize the unwind context with the suspend thread's state
    *ctx = &state;
}

static void jl_thread_resume(int tid, int sig)
{
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    mach_port_t thread = pthread_mach_thread_np((pthread_t)ptls2->system_id);
    kern_return_t ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

// Throw jl_interrupt_exception if the master thread is in a signal async region
// or if SIGINT happens too often.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_all_tls_states[0];
    mach_port_t thread = pthread_mach_thread_np((pthread_t)ptls2->system_id);

    kern_return_t ret = thread_suspend(thread);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // This aborts `sleep` and other syscalls.
    ret = thread_abort(thread);
    HANDLE_MACH_ERROR("thread_abort", ret);

    jl_safepoint_enable_sigint();
    int force = jl_check_force_sigint();
    if (force || (!ptls2->defer_signal && ptls2->io_wait)) {
        jl_safepoint_consume_sigint();
        if (force)
            jl_safe_printf("WARNING: Force throwing a SIGINT\n");
        jl_clear_force_sigint();
        jl_throw_in_thread(0, thread, jl_interrupt_exception);
    }
    else {
        jl_wake_libuv();
    }

    ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

static void jl_exit_thread0(int exitstate)
{
    jl_ptls_t ptls2 = jl_all_tls_states[0];
    mach_port_t thread = pthread_mach_thread_np((pthread_t)ptls2->system_id);
    kern_return_t ret = thread_suspend(thread);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // This aborts `sleep` and other syscalls.
    ret = thread_abort(thread);
    HANDLE_MACH_ERROR("thread_abort", ret);

    unsigned int count = THREAD_STATE_COUNT;
    host_thread_state_t state;
    ret = thread_get_state(thread, THREAD_STATE,
                           (thread_state_t)&state, &count);

    void (*exit_func)(int) = &_exit;
    if (thread0_exit_count <= 1) {
        exit_func = &jl_exit;
    }
    else if (thread0_exit_count == 2) {
        exit_func = &exit;
    }

#ifdef _CPU_X86_64_
    // First integer argument. Not portable but good enough =)
    state.__rdi = exitstate;
#elif defined(_CPU_AARCH64_)
    state.__x[0] = exitstate;
#else
#error Fill in first integer argument here
#endif
    jl_call_in_state(ptls2, &state, (void (*)(void))exit_func);
    ret = thread_set_state(thread, THREAD_STATE,
                           (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);

    ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

static int profile_started = 0;
mach_timespec_t timerprof;
static pthread_t profiler_thread;
clock_serv_t clk;
static mach_port_t profile_port = 0;

#ifdef LIBOSXUNWIND
volatile static int forceDwarf = -2;
static unw_context_t profiler_uc;

static kern_return_t profiler_segv_handler
                (mach_port_t                          exception_port,
                 mach_port_t                                  thread,
                 mach_port_t                                    task,
                 exception_type_t                          exception,
                 exception_data_t                               code,
                 mach_msg_type_number_t                   code_count)
{
    assert(thread == mach_profiler_thread);
    host_thread_state_t state;

    // Not currently unwinding. Raise regular segfault
    if (forceDwarf == -2)
        return KERN_INVALID_ARGUMENT;

    if (forceDwarf == 0)
        forceDwarf = 1;
    else
        forceDwarf = -1;

    unsigned int count = THREAD_STATE_COUNT;

    thread_get_state(thread, THREAD_STATE, (thread_state_t)&state, &count);

#ifdef _CPU_X86_64_
    // don't change cs fs gs rflags
    uint64_t cs = state.__cs;
    uint64_t fs = state.__fs;
    uint64_t gs = state.__gs;
    uint64_t rflags = state.__rflags;
#elif defined(_CPU_AARCH64_)
    uint64_t cpsr = state.__cpsr;
#else
#error Unknown CPU
#endif

    memcpy(&state, &profiler_uc, sizeof(state));

#ifdef _CPU_X86_64_
    state.__cs = cs;
    state.__fs = fs;
    state.__gs = gs;
    state.__rflags = rflags;
#else
    state.__cpsr = cpsr;
#endif

    kern_return_t ret = thread_set_state(thread, THREAD_STATE, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);

    return KERN_SUCCESS;
}
#endif

void *mach_profile_listener(void *arg)
{
    (void)arg;
    int i;
    const int max_size = 512;
    attach_exception_port(mach_thread_self(), 1);
#ifdef LIBOSXUNWIND
    mach_profiler_thread = mach_thread_self();
#endif
    mig_reply_error_t *bufRequest = (mig_reply_error_t*)malloc_s(max_size);
    while (1) {
        kern_return_t ret = mach_msg(&bufRequest->Head, MACH_RCV_MSG,
                                     0, max_size, profile_port,
                                     MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        HANDLE_MACH_ERROR("mach_msg", ret);
        // sample each thread, round-robin style in reverse order
        // (so that thread zero gets notified last)
        for (i = jl_n_threads; i-- > 0; ) {
            // if there is no space left, break early
            if (bt_size_cur >= bt_size_max - 1)
                break;

            unw_context_t *uc;
            jl_thread_suspend_and_get_state(i, &uc);
            if (running) {
#ifdef LIBOSXUNWIND
                /*
                 *  Unfortunately compact unwind info is incorrectly generated for quite a number of
                 *  libraries by quite a large number of compilers. We can fall back to DWARF unwind info
                 *  in some cases, but in quite a number of cases (especially libraries not compiled in debug
                 *  mode, only the compact unwind info may be available). Even more unfortunately, there is no
                 *  way to detect such bogus compact unwind info (other than noticing the resulting segfault).
                 *  What we do here is ugly, but necessary until the compact unwind info situation improves.
                 *  We try to use the compact unwind info and if that results in a segfault, we retry with DWARF info.
                 *  Note that in a small number of cases this may result in bogus stack traces, but at least the topmost
                 *  entry will always be correct, and the number of cases in which this is an issue is rather small.
                 *  Other than that, this implementation is not incorrect as the other thread is paused while we are profiling
                 *  and during stack unwinding we only ever read memory, but never write it.
                 */

                forceDwarf = 0;
                unw_getcontext(&profiler_uc); // will resume from this point if the next lines segfault at any point

                if (forceDwarf == 0) {
                    // Save the backtrace
                    bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc, NULL, 1);
                }
                else if (forceDwarf == 1) {
                    bt_size_cur += rec_backtrace_ctx_dwarf((jl_bt_element_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc, NULL);
                }
                else if (forceDwarf == -1) {
                    jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
                }

                forceDwarf = -2;
#else
                bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc, NULL);
#endif

                // Mark the end of this block with 0
                bt_data_prof[bt_size_cur++].uintptr = 0;

                // Reset the alarm
                kern_return_t ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
                HANDLE_MACH_ERROR("clock_alarm", ret)
            }
            // We're done! Resume the thread.
            jl_thread_resume(i, 0);
        }
    }
}

JL_DLLEXPORT int jl_profile_start_timer(void)
{
    kern_return_t ret;
    if (!profile_started) {
        mach_port_t self = mach_task_self();

        ret = host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, (clock_serv_t *)&clk);
        HANDLE_MACH_ERROR("host_get_clock_service", ret);

        ret = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &profile_port);
        HANDLE_MACH_ERROR("mach_port_allocate", ret);

        // Alright, create a thread to serve as the listener for exceptions
        pthread_attr_t attr;
        if (pthread_attr_init(&attr) != 0) {
            jl_error("pthread_attr_init failed");
        }
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&profiler_thread, &attr, mach_profile_listener, NULL) != 0) {
            jl_error("pthread_create failed");
        }
        pthread_attr_destroy(&attr);

        profile_started = 1;
    }

    timerprof.tv_sec = nsecprof/GIGA;
    timerprof.tv_nsec = nsecprof%GIGA;

    running = 1;
    ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
    HANDLE_MACH_ERROR("clock_alarm", ret);

    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    running = 0;
}
