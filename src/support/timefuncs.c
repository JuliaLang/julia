// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <assert.h>

#include "dtypes.h"

#if defined(_OS_WINDOWS_)
#include <sys/timeb.h>
#else
#include <time.h>
#include <sys/select.h>
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
    jtv->nsec = tb.millitm * 1000000;
#else
    struct timespec ts;
    int code = clock_gettime(CLOCK_REALTIME, &ts);
    // TODO: warn/error on EINVAL/EOVERFLOW?
    jtv->sec = ts.tv_sec;
    jtv->nsec = ts.tv_nsec;
#endif
    return code;
}

JL_DLLEXPORT double jl_clock_now(void)
{
    struct jl_timeval now;
    jl_gettimeofday(&now);
    return now.sec + now.nsec * 1e-9;
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
