// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef TRACE_H
#define TRACE_H

#include "trace_events.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#ifdef ENABLE_TRACING

#define TRACE_ADDR              "127.0.0.1"
#define TRACE_PORT              49149
#define TRACE_FILTER            0xFFFFFFFFFFFFFFFF
#define TRACEPKTS_BUFSIZE       100

void jl_init_tracing(void);

#endif // ENABLE_TRACING

JL_DLLEXPORT void jl_trace(uint32_t event);
JL_DLLEXPORT void jl_flush_traces(void);
JL_DLLEXPORT void jl_flush_all_traces(void);

#ifdef __cplusplus
}
#endif
#endif // TRACE_H
