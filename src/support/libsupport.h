#ifndef LIBSUPPORT_H
#define LIBSUPPORT_H

#define MY_HACK

#include "platform.h"

#if defined(_CPU_X86_64_)
#  define _P64
#elif defined(_CPU_X86_)
#  define _P32
#elif defined(_OS_WINDOWS_)
/* Not sure how to determine pointer size on Windows running ARM. */
#  if _WIN64
#    define _P64
#  else
#    define _P32
#  endif
#elif defined(_COMPILER_GCC_)
#  if __x86_64__ || __ppc64__
#    define _P64
#  else
#    define _P32
#  endif
#else
#  error pointer size not known for your platform / compiler
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
