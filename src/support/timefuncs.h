// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef TIMEFUNCS_H
#define TIMEFUNCS_H

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT double clock_now(void);
void sleep_ms(int ms);

#ifdef __cplusplus
}
#endif

#endif
