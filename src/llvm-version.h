// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/Config/llvm-config.h>
#include "julia_assert.h"
#include "platform.h"

// The LLVM version used, JL_LLVM_VERSION, is represented as a 5-digit integer
// of the form ABBCC, where A is the major version, B is minor, and C is patch.
// So for example, LLVM 3.7.0 is 30700.
#define JL_LLVM_VERSION (LLVM_VERSION_MAJOR * 10000 + LLVM_VERSION_MINOR * 100 \
                        + LLVM_VERSION_PATCH)

#if JL_LLVM_VERSION < 120000
    #error Only LLVM versions >= 12.0.0 are supported by Julia
#endif

#if JL_LLVM_VERSION >= 150000
#define JL_LLVM_OPAQUE_POINTERS 1
#endif

// Pre GCC 12 libgcc defined the ABI for Float16->Float32
// to take an i16. GCC 12 silently changed the ABI to now pass
// Float16 in Float32 registers.
#if JL_LLVM_VERSION < 150000 || defined(_CPU_PPC64_) || defined(_CPU_PPC_)
#define JULIA_FLOAT16_ABI 1
#else
#define JULIA_FLOAT16_ABI 2
#endif

#ifdef __cplusplus
#if defined(__GNUC__) && (__GNUC__ >= 9)
// Added in GCC 9, this warning is annoying
#pragma GCC diagnostic ignored "-Winit-list-lifetime"
#endif
#endif
