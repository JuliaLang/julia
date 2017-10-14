// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_TIMEFUNCS_H
#define JL_TIMEFUNCS_H

#ifdef __cplusplus
extern "C" {
#endif

struct jl_timeval {
    int64_t sec;    /* seconds */
    int64_t usec;   /* microseconds */
};

JL_DLLEXPORT int jl_gettimeofday(struct jl_timeval *jtv);
JL_DLLEXPORT double jl_clock_now(void);
void sleep_ms(int ms);

#ifdef __cplusplus
}
#endif

#endif
