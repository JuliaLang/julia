#include <stdlib.h>
#include <stddef.h>
#include "julia.h"

extern size_t rec_backtrace(ptrint_t *bt_data, size_t maxsize);

static ptrint_t* bt_data_prof = NULL;
static size_t bt_size_max = 0;
static size_t bt_size_cur = 0;
static u_int64_t nsecprof;

//
// Timer section
//
#if defined(__WIN32__)
//Need timer implementation for windows
#else
#include <signal.h>
#if defined (__APPLE__) || defined(__FreeBSD___)
#include <sys/time.h>
struct itimerval timerprof;
int running;

// The handler function, called whenever the profiling timer elapses
static void sprofile_bt(int dummy)
{
    // Get backtrace data
    bt_size_cur += rec_backtrace(bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
    // Mark the end of this block with 0
    bt_data_prof[bt_size_cur] = 0;
    bt_size_cur++;
    // Re-arm the  timer
    if (running && bt_size_cur < bt_size_max) {
        timerprof.it_value.tv_usec = nsecprof/1000;
        setitimer(ITIMER_REAL, &timerprof, 0);
        signal(SIGALRM, sprofile_bt);
    }
}

DLLEXPORT int sprofile_start_timer(void)
{
    timerprof.it_interval.tv_sec = 0;
    timerprof.it_interval.tv_usec = 0;
    timerprof.it_value.tv_sec = 0;
    timerprof.it_value.tv_usec = nsecprof/1000;
    if (setitimer(ITIMER_REAL, &timerprof, 0) == -1)
        return -3;

    running = 1;
    signal(SIGALRM, sprofile_bt);

    return 0;
}

DLLEXPORT void sprofile_stop_timer(void)
{
    running = 0;
}
#else
// Linux implementation. Linux can use the BSD timers, but this is
// the more careful approach.
#include <time.h>
#include <string.h>  // for memset

static timer_t timerprof;
static struct itimerspec itsprof;

// The handler function, called whenever the profiling timer elapses
static void sprofile_bt(int signal, siginfo_t *si, void *uc)
{
    if (si->si_value.sival_ptr == &timerprof && bt_size_cur < bt_size_max) {
        // Get backtrace data
       bt_size_cur += rec_backtrace(bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
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

DLLEXPORT int sprofile_start_timer(void)
{
    struct sigevent sigprof;
    struct sigaction sa;

    // Establish the signal handler
    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = sprofile_bt;
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

DLLEXPORT void sprofile_stop_timer(void)
{
    timer_delete(timerprof);
}
#endif
#endif

//
// Utility functions
//
DLLEXPORT int sprofile_init(size_t maxsize, u_int64_t delay_nsec)
{
    bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (bt_data_prof != NULL)
        free(bt_data_prof);
    bt_data_prof = (ptrint_t*) malloc(maxsize*sizeof(ptrint_t));
    if (bt_data_prof == NULL && maxsize > 0)
        return -1;
    return 0;
}

DLLEXPORT u_int8_t* sprofile_get_data(void)
{
    return (u_int8_t*) bt_data_prof;
}

DLLEXPORT size_t sprofile_len_data(void)
{
    return bt_size_cur;
}

DLLEXPORT size_t sprofile_maxlen_data(void)
{
    return bt_size_max;
}

DLLEXPORT void sprofile_clear_data(void)
{
    bt_size_cur = 0;
}
