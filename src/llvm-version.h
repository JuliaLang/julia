// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <llvm/Config/llvm-config.h>

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR >= 0
#define LLVM40 1
#endif

#if defined(LLVM40) || (defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 9)
#define LLVM39 1
#endif

#if defined(LLVM40) || (defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 8)
#define LLVM38 1
#define USE_ORCJIT
#endif

#if defined(LLVM40) || (defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 7)
#define LLVM37 1

// We enable ORCJIT only if we have our custom patches
#ifndef SYSTEM_LLVM
#define USE_ORCJIT
#endif

#endif

#if defined(LLVM40) || (defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 6)
#define LLVM36 1
#endif

#if defined(LLVM40) || (defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 5)
#define LLVM35 1
#endif

#if defined(LLVM40) || (defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 4)
#define LLVM34 1
#define USE_MCJIT
#endif

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR >= 3
#if defined(LLVM40) || LLVM_VERSION_MINOR >= 3
#define LLVM33 1
#endif
#else
#error LLVM versions < 3.3 are not supported by Julia
#endif

#ifdef USE_ORCJIT //temporary, since in some places USE_MCJIT may be used instead of the correct LLVM version test
#define USE_MCJIT
#endif
#ifdef USE_ORCMCJIT //temporary, since in some places USE_MCJIT may be used instead of the correct LLVM version test
#define USE_MCJIT
#endif
