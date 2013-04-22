#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "dtypes.h"

#ifdef __WIN32__
#include <malloc.h>
#include <sys/timeb.h>
#include <windows.h>
#else
#include <sys/time.h>
#include <sys/poll.h>
#include <unistd.h>
#endif

#include "timefuncs.h"

#ifdef __WIN32__
double floattime(void)
{
    struct timeb tstruct;

    ftime(&tstruct);
    return (double)tstruct.time + (double)tstruct.millitm/1.0e3;
}
#else
double tv2float(struct timeval *tv)
{
    return (double)tv->tv_sec + (double)tv->tv_usec/1.0e6;
}

double diff_time(struct timeval *tv1, struct timeval *tv2)
{
    return tv2float(tv1) - tv2float(tv2);
}
#endif

// return as many bits of system randomness as we can get our hands on
u_int64_t i64time(void)
{
    u_int64_t a;
#ifdef WIN32
    struct timeb tstruct;
    ftime(&tstruct);
    a = (((u_int64_t)tstruct.time)<<32) + (u_int64_t)tstruct.millitm;
#else
    struct timeval now;
    gettimeofday(&now, NULL);
    a = (((u_int64_t)now.tv_sec)<<32) + (u_int64_t)now.tv_usec;
#endif

    return a;
}

double clock_now(void)
{
#ifdef WIN32
    return floattime();
#else
    struct timeval now;

    gettimeofday(&now, NULL);
    return tv2float(&now);
#endif
}

void sleep_ms(int ms)
{
    if (ms == 0)
        return;

#ifdef WIN32
    Sleep(ms);
#else
    struct timeval timeout;

    timeout.tv_sec = ms/1000;
    timeout.tv_usec = (ms % 1000) * 1000;
    select(0, NULL, NULL, NULL, &timeout);
#endif
}
