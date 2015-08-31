// This file is a part of Julia. License is MIT: http://julialang.org/license

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

