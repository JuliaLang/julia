// This file is a part of Julia. License is MIT: https://julialang.org/license

// Include this file after every blocks of LLVM includes to set the assertion back.

#ifdef NDEBUG
#  ifndef JL_NDEBUG
#    undef NDEBUG
#    include <assert.h>
// Set NDEBUG back so that we can include another LLVM header right after
#    define NDEBUG
#  endif
#else
#  ifdef JL_NDEBUG
#    undef JL_NDEBUG
#  endif
#endif
