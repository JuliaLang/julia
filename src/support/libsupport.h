// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef LIBSUPPORT_H
#define LIBSUPPORT_H

#include "platform.h"

#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "dtypes.h"
#include "utils.h"
#include "utf8.h"
#include "ios.h"
#include "timefuncs.h"
#include "hashing.h"
#include "ptrhash.h"
#include "bitvector.h"
#include "dirpath.h"
#include "strtod.h"
#include "crc32c.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT void libsupport_init(void);

#ifdef __cplusplus
}
#endif

#endif
