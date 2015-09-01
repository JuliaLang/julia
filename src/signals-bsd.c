// This file is a part of Julia. License is MIT: http://julialang.org/license

// BSD / Apple-System
//
#include <string.h>
#include <sys/time.h>
struct itimerval timerprof;

DLLEXPORT int jl_profile_start_timer(void)
{
    struct sigaction sa;
    sigset_t ss;

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

