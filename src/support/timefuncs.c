// This file is a part of Julia. License is MIT: https://julialang.org/license

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

JL_DLLEXPORT int jl_gettimeofday(struct jl_timeval *jtv)
{
#if defined(_OS_WINDOWS_)
    struct __timeb64 tb;
    errno_t code = _ftime64_s(&tb);
    jtv->sec = tb.time;
    jtv->usec = tb.millitm * 1000;
#else
    struct timeval tv;
    int code = gettimeofday(&tv, NULL);
    jtv->sec = tv.tv_sec;
    jtv->usec = tv.tv_usec;
#endif
    return code;
}

JL_DLLEXPORT double jl_clock_now(void)
{
    struct jl_timeval now;
    jl_gettimeofday(&now);
    return now.sec + now.usec * 1e-6;
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
