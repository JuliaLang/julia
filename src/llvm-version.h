// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <llvm/Config/llvm-config.h>

#ifndef LLVM_VERSION_PATCH // for LLVM 3.3
#define LLVM_VERSION_PATCH 0
#endif

// The LLVM version used, JL_LLVM_VERSION, is represented as a 5-digit integer
// of the form ABBCC, where A is the major version, B is minor, and C is patch.
// So for example, LLVM 3.7.0 is 30700.
#define JL_LLVM_VERSION (LLVM_VERSION_MAJOR * 10000 + LLVM_VERSION_MINOR * 100 \
                        + LLVM_VERSION_PATCH)

#if JL_LLVM_VERSION != 30300 && JL_LLVM_VERSION < 30701
    #error Only LLVM versions 3.3 and >= 3.7.1 are supported by Julia
#endif

#if JL_LLVM_VERSION >= 30800
    #define USE_ORCJIT
// We enable ORCJIT only if we have our custom patches
#elif JL_LLVM_VERSION >= 30700 && !defined(SYSTEM_LLVM)
    #define USE_ORCJIT
#endif

#if JL_LLVM_VERSION >= 30400
    #define USE_MCJIT
#endif


//temporary, since in some places USE_MCJIT may be used instead of the correct LLVM version test
#ifdef USE_ORCJIT
    #define USE_MCJIT
#endif
#ifdef USE_ORCMCJIT
    #define USE_MCJIT
#endif
