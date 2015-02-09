#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

static volatile ptrint_t *bt_data_prof = NULL;
static volatile size_t bt_size_max = 0;
static volatile size_t bt_size_cur = 0;
static volatile u_int64_t nsecprof = 0;
static volatile int running = 0;
static const    u_int64_t GIGA = 1000000000ULL;
/////////////////////////////////////////
// Timers to take samples at intervals //
/////////////////////////////////////////
DLLEXPORT void jl_profile_stop_timer(void);
DLLEXPORT int jl_profile_start_timer(void);

#if defined(_WIN32)
//
// Windows
//
volatile HANDLE hBtThread = 0;
static DWORD WINAPI profile_bt( LPVOID lparam )
{
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
#else
#include <signal.h>
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
volatile mach_port_t mach_profiler_thread = 0;
static unw_context_t profiler_uc;
mach_timespec_t timerprof;

kern_return_t profiler_segv_handler
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
                JL_PRINTF(JL_STDERR, "Warning: Profiler attempt to access an invalid memory location\n");
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
            JL_PRINTF(JL_STDERR, "pthread_attr_init failed");
            jl_exit(1);
        }
        pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
        if (pthread_create(&profiler_thread,&attr,mach_profile_listener,NULL) != 0) {
            JL_PRINTF(JL_STDERR, "pthread_create failed");
            jl_exit(1);
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

#elif defined(__FreeBSD__) || defined(__APPLE__)
//
// BSD / Apple-System
//
#include <string.h>
#include <sys/time.h>
struct itimerval timerprof;

// The handler function, called whenever the profiling timer elapses
static void profile_bt(int sig)
{
    if (running && bt_size_cur < bt_size_max) {
        // Get backtrace data
        bt_size_cur += rec_backtrace((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
        // Mark the end of this block with 0
        bt_data_prof[bt_size_cur] = 0;
        bt_size_cur++;
    }
    if (bt_size_cur >= bt_size_max) {
        // Buffer full: Delete the  timer
        jl_profile_stop_timer();
    }
}

DLLEXPORT int jl_profile_start_timer(void)
{
    struct sigaction sa;
    sigset_t ss;

    // Make sure SIGPROF is unblocked
    sigemptyset(&ss);
    sigaddset(&ss, SIGPROF);
    if (sigprocmask(SIG_UNBLOCK, &ss, NULL) == -1)
        return -4;

    // Establish the signal handler
    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_handler = profile_bt;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGPROF, &sa, NULL) == -1)
        return -1;

    timerprof.it_interval.tv_sec = nsecprof/GIGA;
    timerprof.it_interval.tv_usec = (nsecprof%GIGA)/1000;
    timerprof.it_value.tv_sec = nsecprof/GIGA;
    timerprof.it_value.tv_usec = (nsecprof%GIGA)/1000;
    if (setitimer(ITIMER_PROF, &timerprof, 0) == -1)
        return -3;

    running = 1;

    return 0;
}

DLLEXPORT void jl_profile_stop_timer(void)
{
    if (running) {
        memset(&timerprof, 0, sizeof(timerprof));
        setitimer(ITIMER_PROF, &timerprof, 0);
    }
    running = 0;
}
#else
//
// Linux
//
// Linux can use the BSD timers, but this is the more careful approach.
#include <time.h>
#include <string.h>  // for memset

static timer_t timerprof;
static struct itimerspec itsprof;

// The handler function, called whenever the profiling timer elapses
static void profile_bt(int signal, siginfo_t *si, void *uc)
{
    if (running && si->si_value.sival_ptr == &timerprof && bt_size_cur < bt_size_max) {
        // Get backtrace data
        bt_size_cur += rec_backtrace((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
        // Mark the end of this block with 0
        bt_data_prof[bt_size_cur] = 0;
        bt_size_cur++;
    }
    if (bt_size_cur >= bt_size_max) {
        // Buffer full: Delete the  timer
        jl_profile_stop_timer();
    }
}

DLLEXPORT int jl_profile_start_timer(void)
{
    struct sigevent sigprof;
    struct sigaction sa;
    sigset_t ss;

    // Make sure SIGUSR2 is unblocked
    sigemptyset(&ss);
    sigaddset(&ss, SIGUSR2);
    if (sigprocmask(SIG_UNBLOCK, &ss, NULL) == -1)
        return -4;

    // Establish the signal handler
    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = profile_bt;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGUSR2, &sa, NULL) == -1)
        return -1;

    // Establish the signal event
    memset(&sigprof, 0, sizeof(struct sigevent));
    sigprof.sigev_notify = SIGEV_SIGNAL;
    sigprof.sigev_signo = SIGUSR2;
    sigprof.sigev_value.sival_ptr = &timerprof;
    if (timer_create(CLOCK_REALTIME, &sigprof, &timerprof) == -1)
        return -2;

    // Start the timer
    itsprof.it_interval.tv_sec = nsecprof/GIGA;
    itsprof.it_interval.tv_nsec = nsecprof%GIGA;
    itsprof.it_value.tv_sec = nsecprof/GIGA;
    itsprof.it_value.tv_nsec = nsecprof%GIGA;
    if (timer_settime(timerprof, 0, &itsprof, NULL) == -1)
        return -3;

    running = 1;
    return 0;
}

DLLEXPORT void jl_profile_stop_timer(void)
{
    if (running)
        timer_delete(timerprof);
    running = 0;
}
#endif
#endif


///////////////////////
// Utility functions //
///////////////////////
DLLEXPORT int jl_profile_init(size_t maxsize, u_int64_t delay_nsec)
{
    bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);
    bt_data_prof = (ptrint_t*) malloc(maxsize*sizeof(ptrint_t));
    if (bt_data_prof == NULL && maxsize > 0)
        return -1;
    bt_size_cur = 0;
    return 0;
}

DLLEXPORT u_int8_t *jl_profile_get_data(void)
{
    return (u_int8_t*) bt_data_prof;
}

DLLEXPORT size_t jl_profile_len_data(void)
{
    return bt_size_cur;
}

DLLEXPORT size_t jl_profile_maxlen_data(void)
{
    return bt_size_max;
}

DLLEXPORT u_int64_t jl_profile_delay_nsec(void)
{
    return nsecprof;
}

DLLEXPORT void jl_profile_clear_data(void)
{
    bt_size_cur = 0;
}

DLLEXPORT int jl_profile_is_running(void)
{
    return running;
}

#ifdef __cplusplus
}
#endif
