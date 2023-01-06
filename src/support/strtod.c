#define _GNU_SOURCE
#include "libsupport.h"
#include <stdlib.h>
#include <locale.h>
#include "gdtoa/gdtoa.h"

#if defined(__APPLE__) || defined(__FreeBSD__)
#include <xlocale.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT double jl_strtod_c(const char *nptr, char **endptr)
{
    return __strtod(nptr, endptr);
}

JL_DLLEXPORT float jl_strtof_c(const char *nptr, char **endptr)
{
    return __strtof(nptr, endptr);
}

#ifdef __cplusplus
}
#endif
