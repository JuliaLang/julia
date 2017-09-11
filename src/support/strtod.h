// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_STRTOD_H
#define JL_STRTOD_H

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT double jl_strtod_c(const char *nptr, char **endptr);
JL_DLLEXPORT float jl_strtof_c(const char *nptr, char **endptr);

#ifdef __cplusplus
}
#endif

#endif
