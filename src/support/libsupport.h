// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LIBSUPPORT_H
#define JL_LIBSUPPORT_H

#include "platform.h"

#include <stdlib.h>
#include <stdarg.h>
#include "dtypes.h"
#include "utf8.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT void libsupport_init(void);

#ifdef __cplusplus
}
#endif

#endif
