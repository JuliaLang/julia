// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/Config/llvm-config.h>
#include "julia_assert.h"
#include "platform.h"

// The LLVM version used, JL_LLVM_VERSION, is represented as a 5-digit integer
// of the form ABBCC, where A is the major version, B is minor, and C is patch.
// So for example, LLVM 3.7.0 is 30700.
#define JL_LLVM_VERSION (LLVM_VERSION_MAJOR * 10000 + LLVM_VERSION_MINOR * 100 \
                        + LLVM_VERSION_PATCH)

#if JL_LLVM_VERSION < 170000
    #error Only LLVM versions >= 17.0.0 are supported by Julia
#endif

#if JL_LLVM_VERSION < 19000 && defined(_CPU_RISCV64_)
    #error Only LLVM versions >= 19.0.0 are supported by Julia on RISC-V
#endif

// clang 22 destroys a by-value parameter both in the callee, at any `return`
// statement, and again in the caller (llvm/llvm-project#169320), so the static
// analyzer sees a double free whenever ownership of a pointer is passed by
// value: notably any `llvm::Error` handed to a `unique_function`. Statements
// that trip over this have to be hidden from the analyzer while this is
// defined. Fixed in LLVM 23 by llvm/llvm-project#177363, which moved the
// parameter destructors behind a CFG option that the analyzer does not set.
#if defined(__clang_analyzer__) && JL_LLVM_VERSION >= 220000 && JL_LLVM_VERSION < 230000
#define JL_SA_BROKEN_PARAM_DTORS 1
#endif

#ifdef __cplusplus
#if defined(__GNUC__) && (__GNUC__ >= 9)
// Added in GCC 9, this warning is annoying
#pragma GCC diagnostic ignored "-Winit-list-lifetime"
#endif
#endif
