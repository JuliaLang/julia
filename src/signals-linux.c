// This file is a part of Julia. License is MIT: http://julialang.org/license

// Linux
//
// Linux can use the BSD timers, but this is the more careful approach.
#include <time.h>
#include <string.h>  // for memset

static timer_t timerprof;
static struct itimerspec itsprof;

DLLEXPORT int jl_profile_start_timer(void)
{
    struct sigevent sigprof;
    struct sigaction sa;
    sigset_t ss;

    // Establish the signal event
    memset(&sigprof, 0, sizeof(struct sigevent));
    sigprof.sigev_notify = SIGEV_SIGNAL;
    sigprof.sigev_signo = SIGUSR1;
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

