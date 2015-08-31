// This file is a part of Julia. License is MIT: http://julialang.org/license

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

