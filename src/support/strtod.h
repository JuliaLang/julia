// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef STRTOD_H
#define STRTOD_H

#ifdef __cplusplus
extern "C" {
#endif

double strtod_c(const char *nptr, char **endptr);
float strtof_c(const char *nptr, char **endptr);

#ifdef __cplusplus
}
#endif

#endif

