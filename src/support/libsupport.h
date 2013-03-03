#ifndef LIBSUPPORT_H
#define LIBSUPPORT_H

// Check windows
#if _WIN32 || _WIN64
#if _WIN64
#define _P64
#else
#define _P32
#endif
#endif

// Check GCC
#if __GNUC__
#if __x86_64__ || __ppc64__
#define _P64
#else
#define _P32
#endif
#else
#error pointer size not known for your platform / compiler
#endif

#include <stdlib.h>
#include <stdarg.h>
#include "dtypes.h"
#include "utils.h"
#include "utf8.h"
#include "ios.h"
#include "timefuncs.h"
#include "hashing.h"
#include "ptrhash.h"
#include "bitvector.h"
#include "dirpath.h"

DLLEXPORT void libsupport_init(void);

#endif
