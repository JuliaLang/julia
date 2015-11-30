// This file is a part of Julia. License is MIT: http://julialang.org/license

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
    attach_exception_port();
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

enum x86_trap_flags {
    USER_MODE = 0x4,
    WRITE_FAULT = 0x2,
    PAGE_PRESENT = 0x1
};

void jl_throw_in_thread(int tid, mach_port_t thread, jl_value_t *exception)
{
    unsigned int count = MACHINE_THREAD_STATE_COUNT;
    x86_thread_state64_t state;
    kern_return_t ret = thread_get_state(thread, x86_THREAD_STATE64, (thread_state_t)&state, &count);
    HANDLE_MACH_ERROR("thread_get_state", ret);

    jl_all_task_states[tid].ptls->bt_size =
        rec_backtrace_ctx(jl_all_task_states[tid].ptls->bt_data,
                          JL_MAX_BT_SIZE, (bt_context_t)&state);
    jl_all_task_states[tid].ptls->exception_in_transit = exception;

    uint64_t rsp = (uint64_t)jl_all_task_states[tid].signal_stack + sig_stack_size;
    rsp &= -16; // ensure 16-byte alignment

    // push (null) $RIP onto the stack
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;

    state.__rsp = rsp; // set stack pointer
    state.__rip = (uint64_t)&jl_rethrow; // "call" the function

    ret = thread_set_state(thread, x86_THREAD_STATE64, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state",ret);
}

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
    x86_exception_state64_t exc_state;
    x86_thread_state64_t state;
#ifdef LIBOSXUNWIND
    if (thread == mach_profiler_thread) {
        return profiler_segv_handler(exception_port, thread, task, exception, code, code_count);
    }
#endif
    kern_return_t ret = thread_get_state(thread, x86_EXCEPTION_STATE64, (thread_state_t)&exc_state, &exc_count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
    uint64_t fault_addr = exc_state.__faultvaddr;
#ifdef SEGV_EXCEPTION
    if (1) {
#else
    if (msync((void*)(fault_addr & ~(jl_page_size - 1)), 1, MS_ASYNC) == 0) { // check if this was a valid address
#endif
        jl_value_t *excpt;
        if (is_addr_on_stack((void*)fault_addr)) {
            excpt = jl_stackovf_exception;
        }
#ifdef SEGV_EXCEPTION
        else if (msync((void*)(fault_addr & ~(jl_page_size - 1)), 1, MS_ASYNC) != 0) {
            // no page mapped at this address
            excpt = jl_segv_exception;
        }
#endif
        else {
            if (!(exc_state.__err & WRITE_FAULT))
                return KERN_INVALID_ARGUMENT; // rethrow the SEGV since it wasn't an error with writing to read-only memory
            excpt = jl_readonlymemory_exception;
        }
        jl_throw_in_thread(0, thread, excpt);

        return KERN_SUCCESS;
    }
    else {
        kern_return_t ret = thread_get_state(thread, x86_THREAD_STATE64, (thread_state_t)&state, &count);
        HANDLE_MACH_ERROR("thread_get_state", ret);
        jl_critical_error(SIGSEGV, (unw_context_t*)&state,
                          jl_bt_data, &jl_bt_size);
        return KERN_INVALID_ARGUMENT;
    }
}

void attach_exception_port()
{
    kern_return_t ret;
    // http://www.opensource.apple.com/source/xnu/xnu-2782.1.97/osfmk/man/thread_set_exception_ports.html
    ret = thread_set_exception_ports(mach_thread_self(), EXC_MASK_BAD_ACCESS, segv_port, EXCEPTION_DEFAULT, MACHINE_THREAD_STATE);
    HANDLE_MACH_ERROR("thread_set_exception_ports", ret);
}

static void jl_thread_suspend_and_get_state(int tid, unw_context_t **ctx, int sig)
{
    (void)sig;
    mach_port_t tid_port = pthread_mach_thread_np(jl_all_task_states[tid].system_id);

    kern_return_t ret = thread_suspend(tid_port);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // Do the actual sampling
    unsigned int count = MACHINE_THREAD_STATE_COUNT;
    static unw_context_t state;
    memset(&state, 0, sizeof(unw_context_t));

    // Get the state of the suspended thread
    ret = thread_get_state(tid_port, x86_THREAD_STATE64, (thread_state_t)&state, &count);

    // Initialize the unwind context with the suspend thread's state
    *ctx = &state;
}

static void jl_thread_resume(int tid, int sig)
{
    mach_port_t thread = pthread_mach_thread_np(jl_all_task_states[tid].system_id);

    if (tid == 0 && sig == SIGINT) {
        if (jl_defer_signal) {
            jl_signal_pending = sig;
        }
        else {
            jl_signal_pending = 0;
            jl_throw_in_thread(tid, thread, jl_interrupt_exception);
        }
    }

    kern_return_t ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret)
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
    x86_thread_state64_t state;

    // Not currently unwinding. Raise regular segfault
    if (forceDwarf == -2)
        return KERN_INVALID_ARGUMENT;

    if (forceDwarf == 0)
        forceDwarf = 1;
    else
        forceDwarf = -1;

    unsigned int count = MACHINE_THREAD_STATE_COUNT;

    thread_get_state(thread, x86_THREAD_STATE64, (thread_state_t)&state, &count);

    // don't change cs fs gs rflags
    uint64_t cs = state.__cs;
    uint64_t fs = state.__fs;
    uint64_t gs = state.__gs;
    uint64_t rflags = state.__rflags;

    memcpy(&state, &profiler_uc, sizeof(x86_thread_state64_t));

    state.__cs = cs;
    state.__fs = fs;
    state.__gs = gs;
    state.__rflags = rflags;

    kern_return_t ret = thread_set_state(thread, x86_THREAD_STATE64, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);

    return KERN_SUCCESS;
}
#endif

void *mach_profile_listener(void *arg)
{
    (void)arg;
    int i;
    const int max_size = 512;
    attach_exception_port();
#ifdef LIBOSXUNWIND
    mach_profiler_thread = mach_thread_self();
#endif
    mig_reply_error_t *bufRequest = (mig_reply_error_t *) malloc(max_size);
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
            jl_thread_suspend_and_get_state(i, &uc, -1);

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
                bt_size_cur += rec_backtrace_ctx((ptrint_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc);
            }
            else if (forceDwarf == 1) {
                bt_size_cur += rec_backtrace_ctx_dwarf((ptrint_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc);
            }
            else if (forceDwarf == -1) {
                jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
            }

            forceDwarf = -2;
#else
            bt_size_cur += rec_backtrace_ctx((ptrint_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc);
#endif

            // Mark the end of this block with 0
            bt_data_prof[bt_size_cur++] = 0;

            // We're done! Resume the thread.
            jl_thread_resume(i, 0);

            if (running) {
                // Reset the alarm
                kern_return_t ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
                HANDLE_MACH_ERROR("clock_alarm", ret)
            }
        }
    }
}

DLLEXPORT int jl_profile_start_timer(void)
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

DLLEXPORT void jl_profile_stop_timer(void)
{
    running = 0;
}

