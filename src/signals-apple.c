// This file is a part of Julia. License is MIT: http://julialang.org/license

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

void *mach_segv_listener(void *arg)
{
    (void)arg;
    while (1) {
        int ret = mach_msg_server(exc_server,2048,segv_port,MACH_MSG_TIMEOUT_NONE);
        jl_safe_printf("mach_msg_server: %s\n", mach_error_string(ret));
        jl_exit(128+SIGSEGV);
    }
}

#ifdef SEGV_EXCEPTION
void darwin_segv_handler(unw_context_t *uc)
{
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, uc);
    jl_exception_in_transit = jl_segv_exception;
    jl_rethrow();
}
#endif

void darwin_stack_overflow_handler(unw_context_t *uc)
{
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, uc);
    jl_exception_in_transit = jl_stackovf_exception;
    jl_rethrow();
}

void darwin_accerr_handler(unw_context_t *uc)
{
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, uc);
    jl_exception_in_transit = jl_readonlymemory_exception;
    jl_rethrow();
}

#define HANDLE_MACH_ERROR(msg, retval) \
    if (retval!=KERN_SUCCESS) { mach_error(msg ":", (retval)); jl_exit(1); }

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
#ifdef SEGV_EXCEPTION
    if (1) {
#else
    if (msync((void*)(fault_addr & ~(jl_page_size - 1)), 1, MS_ASYNC) == 0) { // check if this was a valid address
#endif
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
        if (is_addr_on_stack((void*)fault_addr)) {
            state.__rip = (uint64_t)darwin_stack_overflow_handler;
        }
#ifdef SEGV_EXCEPTION
        else if (msync((void*)(fault_addr & ~(jl_page_size - 1)), 1, MS_ASYNC) != 0) {
            // no page mapped at this address
            state.__rip = (uint64_t)darwin_segv_handler;
        }
#endif
        else {
            if (!(exc_state.__err & WRITE_FAULT))
                return KERN_INVALID_ARGUMENT; // rethrow the SEGV since it wasn't an error with writing to read-only memory
            state.__rip = (uint64_t)darwin_accerr_handler;
        }

        state.__rbp = state.__rsp;
        ret = thread_set_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,count);
        HANDLE_MACH_ERROR("thread_set_state",ret);
        return KERN_SUCCESS;
    }
    else {
        ret = thread_get_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,&count);
        HANDLE_MACH_ERROR("thread_get_state(3)",ret);
        jl_critical_error(SIGSEGV, (unw_context_t*)&state, bt_data, &bt_size);
        return KERN_INVALID_ARGUMENT;
    }
}

void attach_exception_port()
{
    kern_return_t ret;
    // http://www.opensource.apple.com/source/xnu/xnu-2782.1.97/osfmk/man/thread_set_exception_ports.html
    ret = thread_set_exception_ports(mach_thread_self(),EXC_MASK_BAD_ACCESS,segv_port,EXCEPTION_DEFAULT,MACHINE_THREAD_STATE);
    HANDLE_MACH_ERROR("thread_set_exception_ports",ret);
}

#ifdef LIBOSXUNWIND
//
// OS X
//
#include <mach/mach_traps.h>
#include <mach/task.h>
#include <mach/mig_errors.h>
#include <mach/clock.h>
#include <mach/clock_types.h>
#include <mach/clock_reply.h>
#include <assert.h>

#define HANDLE_MACH_ERROR(msg, retval) \
    if (retval!=KERN_SUCCESS) { mach_error(msg ":", (retval)); jl_exit(1); }

static pthread_t profiler_thread;
static mach_port_t main_thread;
clock_serv_t clk;
static int profile_started = 0;
static mach_port_t profile_port = 0;
volatile static int forceDwarf = -2;
static unw_context_t profiler_uc;
mach_timespec_t timerprof;

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

    thread_get_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,&count);

    // don't change cs fs gs rflags
    uint64_t cs = state.__cs;
    uint64_t fs = state.__fs;
    uint64_t gs = state.__gs;
    uint64_t rflags = state.__rflags;

    memcpy(&state,&profiler_uc,sizeof(x86_thread_state64_t));

    state.__cs = cs;
    state.__fs = fs;
    state.__gs = gs;
    state.__rflags = rflags;

    kern_return_t ret = thread_set_state(thread,x86_THREAD_STATE64,(thread_state_t)&state,count);
    HANDLE_MACH_ERROR("thread_set_state",ret);

    return KERN_SUCCESS;
}

void *mach_profile_listener(void *arg)
{
    (void)arg;
    int max_size = 512;
    attach_exception_port();
    mach_profiler_thread = mach_thread_self();
    mig_reply_error_t *bufRequest = (mig_reply_error_t *) malloc(max_size);
    while (1) {
        kern_return_t ret = mach_msg(&bufRequest->Head, MACH_RCV_MSG,
                                     0, max_size, profile_port,
                                     MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        HANDLE_MACH_ERROR("mach_msg",ret);
        if (bt_size_cur < bt_size_max) {
            kern_return_t ret;
            // Suspend the thread so we may safely sample it
            ret = thread_suspend(main_thread);
            HANDLE_MACH_ERROR("thread_suspend",ret);

            // Do the actual sampling
            unsigned int count = MACHINE_THREAD_STATE_COUNT;
            x86_thread_state64_t state;

            // Get the state of the suspended thread
            ret = thread_get_state(main_thread,x86_THREAD_STATE64,(thread_state_t)&state,&count);
            HANDLE_MACH_ERROR("thread_get_state",ret);

            // Initialize the unwind context with the suspend thread's state
            unw_context_t uc;
            memset(&uc,0,sizeof(unw_context_t));
            memcpy(&uc,&state,sizeof(x86_thread_state64_t));

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
            unw_getcontext(&profiler_uc);

            if (forceDwarf == 0) {
                // Save the backtrace
                bt_size_cur += rec_backtrace_ctx((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1, &uc);
            }
            else if (forceDwarf == 1) {
                bt_size_cur += rec_backtrace_ctx_dwarf((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1, &uc);
            }
            else if (forceDwarf == -1) {
                jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
            }

            forceDwarf = -2;

            // Mark the end of this block with 0
            bt_data_prof[bt_size_cur] = 0;
            bt_size_cur++;

            // We're done! Resume the thread.
            ret = thread_resume(main_thread);
            HANDLE_MACH_ERROR("thread_resume",ret)

            if (running) {
                // Reset the alarm
                ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
                HANDLE_MACH_ERROR("clock_alarm",ret)
            }
        }
    }
}

DLLEXPORT int jl_profile_start_timer(void)
{
    kern_return_t ret;
    if (!profile_started) {
        mach_port_t self = mach_task_self();
        main_thread = mach_thread_self();

        ret = host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, (clock_serv_t *)&clk);
        HANDLE_MACH_ERROR("host_get_clock_service", ret);

        ret = mach_port_allocate(self,MACH_PORT_RIGHT_RECEIVE,&profile_port);
        HANDLE_MACH_ERROR("mach_port_allocate",ret);

        // Alright, create a thread to serve as the listener for exceptions
        pthread_attr_t attr;
        if (pthread_attr_init(&attr) != 0) {
            jl_error("pthread_attr_init failed");
        }
        pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
        if (pthread_create(&profiler_thread,&attr,mach_profile_listener,NULL) != 0) {
            jl_error("pthread_create failed");
        }
        pthread_attr_destroy(&attr);

        profile_started = 1;
    }

    timerprof.tv_sec = nsecprof/GIGA;
    timerprof.tv_nsec = nsecprof%GIGA;

    running = 1;
    ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
    HANDLE_MACH_ERROR("clock_alarm",ret);

    return 0;
}

DLLEXPORT void jl_profile_stop_timer(void)
{
    running = 0;
}
#else
#include "signals-bsd.c"
#endif
