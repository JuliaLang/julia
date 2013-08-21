#include <stdlib.h>
#include <stddef.h>
#include "julia.h"

static volatile ptrint_t* bt_data_prof = NULL;
static volatile size_t bt_size_max = 0;
static volatile size_t bt_size_cur = 0;
static volatile u_int64_t nsecprof = 0;

/////////////////////////////////////////
// Timers to take samples at intervals //
/////////////////////////////////////////
#if defined(__WIN32__)
//
// Windows
//
volatile HANDLE hBtThread = 0;
volatile int running = 0;
static DWORD WINAPI profile_bt( LPVOID lparam ) {
    TIMECAPS tc;
    if (MMSYSERR_NOERROR!=timeGetDevCaps(&tc, sizeof(tc))) {
        fputs("failed to get get timer resulution",stderr);
        hBtThread = 0;
        return 0;
    }
    while (1) {
        if (running && bt_size_cur < bt_size_max) {
            DWORD timeout = nsecprof/1000000;
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
DLLEXPORT int profile_start_timer(void) {
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
DLLEXPORT void profile_stop_timer(void) {
    running = 0;
}
#else
#include <signal.h>
#if defined (__APPLE__) || defined(__FreeBSD___)
//
// BSD/OSX
//
#include <sys/time.h>
struct itimerval timerprof;
volatile int running;

// The handler function, called whenever the profiling timer elapses
static void profile_bt(int dummy)
{
    // Get backtrace data
    bt_size_cur += rec_backtrace((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
    // Mark the end of this block with 0
    bt_data_prof[bt_size_cur] = 0;
    bt_size_cur++;
    // Re-arm the  timer
    if (running && bt_size_cur < bt_size_max) {
        timerprof.it_value.tv_usec = nsecprof/1000;
        setitimer(ITIMER_REAL, &timerprof, 0);
        signal(SIGALRM, profile_bt);
    }
}

DLLEXPORT int profile_start_timer(void)
{
    timerprof.it_interval.tv_sec = 0;
    timerprof.it_interval.tv_usec = 0;
    timerprof.it_value.tv_sec = 0;
    timerprof.it_value.tv_usec = nsecprof/1000;
    if (setitimer(ITIMER_REAL, &timerprof, 0) == -1)
        return -3;

    running = 1;
    signal(SIGALRM, profile_bt);

    return 0;
}

DLLEXPORT void profile_stop_timer(void)
{
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
    if (si->si_value.sival_ptr == &timerprof && bt_size_cur < bt_size_max) {
        // Get backtrace data
       bt_size_cur += rec_backtrace((ptrint_t*)bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
        // Mark the end of this block with 0
        bt_data_prof[bt_size_cur] = 0;
        bt_size_cur++;
        // Re-arm the  timer
        if (bt_size_cur < bt_size_max) {
            itsprof.it_value.tv_nsec = nsecprof;
            timer_settime(timerprof, 0, &itsprof, NULL);
        }
    }
}

DLLEXPORT int profile_start_timer(void)
{
    struct sigevent sigprof;
    struct sigaction sa;

    // Establish the signal handler
    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = profile_bt;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGUSR1, &sa, NULL) == -1)
        return -1;

    // Establish the signal event
    memset(&sigprof, 0, sizeof(struct sigevent));
    sigprof.sigev_notify = SIGEV_SIGNAL;
    sigprof.sigev_signo = SIGUSR1;
    sigprof.sigev_value.sival_ptr = &timerprof;
    if (timer_create(CLOCK_REALTIME, &sigprof, &timerprof) == -1)
        return -2;

    // Start the timer
    itsprof.it_value.tv_sec = 0;
    itsprof.it_interval.tv_sec = 0;       // make it fire once
    itsprof.it_interval.tv_nsec = 0;
    itsprof.it_value.tv_nsec = nsecprof;
    if (timer_settime(timerprof, 0, &itsprof, NULL) == -1)
        return -3;

    return 0;
}

DLLEXPORT void profile_stop_timer(void)
{
    timer_delete(timerprof);
}
#endif
#endif


///////////////////////
// Utility functions //
///////////////////////
DLLEXPORT int profile_init(size_t maxsize, u_int64_t delay_nsec)
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

DLLEXPORT u_int8_t* profile_get_data(void)
{
    return (u_int8_t*) bt_data_prof;
}

DLLEXPORT size_t profile_len_data(void)
{
    return bt_size_cur;
}

DLLEXPORT size_t profile_maxlen_data(void)
{
    return bt_size_max;
}

DLLEXPORT void profile_clear_data(void)
{
    bt_size_cur = 0;
}
