// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/Config/llvm-config.h>
#include "julia_assert.h"

// The LLVM version used, JL_LLVM_VERSION, is represented as a 5-digit integer
// of the form ABBCC, where A is the major version, B is minor, and C is patch.
// So for example, LLVM 3.7.0 is 30700.
#define JL_LLVM_VERSION (LLVM_VERSION_MAJOR * 10000 + LLVM_VERSION_MINOR * 100 \
                        + LLVM_VERSION_PATCH)

#if JL_LLVM_VERSION < 110000
    #error Only LLVM versions >= 11.0.0 are supported by Julia
#endif

#ifndef LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING
#define LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING 0
#endif

#ifndef LLVM_ENABLE_STATS
#define LLVM_ENABLE_STATS 0
#endif

#if defined(__GNUC__) && (__GNUC__ >= 9)
// Added in GCC 9, this warning is annoying
#pragma GCC diagnostic ignored "-Winit-list-lifetime"
#endif
