// This file is a part of Julia. License is MIT: https://julialang.org/license
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <stdint.h>
#include <inttypes.h>

#include "../src/support/platform.h"
#include "../src/support/dtypes.h"

// Borrow definition from `support/dtypes.h`
#ifdef _OS_WINDOWS_
#  define DLLEXPORT __declspec(dllexport)
#else
# if defined(_OS_LINUX_) && !defined(_COMPILER_CLANG_)
// Clang and ld disagree about the proper relocation for STV_PROTECTED, causing
// linker errors.
#  define DLLEXPORT __attribute__ ((visibility("protected")))
# else
#  define DLLEXPORT __attribute__ ((visibility("default")))
# endif
#endif

#ifdef _P64
#define jint int64_t
#define PRIjint PRId64
#else
#define jint int32_t
#define PRIjint PRId32
#endif
