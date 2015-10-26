// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <llvm/Config/llvm-config.h>

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 8
#define LLVM38 1
#endif

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 7
#define LLVM37 1
// Experimental:
//#define USE_ORCMCJIT
//#define USE_ORCJIT
#endif

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 6
#define LLVM36 1
#endif

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 5
#define LLVM35 1
#endif

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 4
#define LLVM34 1
#define USE_MCJIT
#endif

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 3
#define LLVM33 1
#else
#error LLVM versions < 3.3 are not supported by Julia
#endif

#ifdef USE_ORCJIT //temporary, since in some places USE_MCJIT may be used instead of the correct LLVM version test
#define USE_MCJIT
#endif
#ifdef USE_ORCMCJIT //temporary, since in some places USE_MCJIT may be used instead of the correct LLVM version test
#define USE_MCJIT
#endif
