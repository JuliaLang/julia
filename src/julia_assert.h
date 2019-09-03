// This file is a part of Julia. License is MIT: https://julialang.org/license

// Include this file instead of `assert.h` directly.
// This is necessary because LLVM sometimes has bugs that cause runtime assertion if
// the `NDEBUG` setting is different from the one used to compile LLVM.
// For C++ files, we set `NDEBUG` to match what LLVM expects and use `JL_NDEBUG` to
// enable assertions in julia code. After including this file, the definition of `assert` will
// match the setting given in `JL_NDEBUG` and `NDEBUG` will remain unchanged.
//
// Files that need `assert` should include this file after all other includes.
// All files should also check `JL_NDEBUG` instead of `NDEBUG`.

#ifdef NDEBUG
#  ifndef JL_NDEBUG
#    undef NDEBUG
#    include <assert.h>
// Set NDEBUG back so that we can include another LLVM header right after
#    define NDEBUG
#  else
#    include <assert.h>
#  endif
#else
#  ifdef JL_NDEBUG
#    undef JL_NDEBUG
#  endif
#  include <assert.h>
#endif
