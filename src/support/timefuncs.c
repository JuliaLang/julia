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

#if defined(_OS_WINDOWS_)
#include <malloc.h>
#include <sys/timeb.h>
#include <windows.h>
#else
#include <sys/time.h>
#include <sys/poll.h>
#include <unistd.h>
#endif

#include "timefuncs.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_)
static double floattime(void)
{
    struct timeb tstruct;

    ftime(&tstruct);
    return (double)tstruct.time + (double)tstruct.millitm/1.0e3;
}
#else
static double tv2float(struct timeval *tv)
{
    return (double)tv->tv_sec + (double)tv->tv_usec/1.0e6;
}
#endif

double clock_now(void)
{
#if defined(_OS_WINDOWS_)
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

#if defined(_OS_WINDOWS_)
    Sleep(ms);
#else
    struct timeval timeout;

    timeout.tv_sec = ms / 1000;
    timeout.tv_usec = (ms % 1000) * 1000;

    select(0, NULL, NULL, NULL, &timeout);
#endif
}

#ifdef __cplusplus
}
#endif
