// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_ASSERT_H
#define JL_ASSERT_H

// Include this file instead of `assert.h` directly.
// This is necessary because LLVM sometimes has bugs that cause runtime assertion if
// the `NDEBUG` setting is different from the one used to compile LLVM.
// For C++ files, we set `NDEBUG` to match what LLVM expects and use `JL_NDEBUG` to
// enable assertions in julia code. After including this file, the definition of `assert` will
// match the setting given in `JL_NDEBUG` and `NDEBUG` will remain unchanged.
//
// Files that need `assert` should include this file after all other includes.
// All files should also check `JL_NDEBUG` instead of `NDEBUG`.

#include "support/platform.h"

#pragma GCC visibility push(default)

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
#    define NDEBUG
#    include <assert.h>
#    undef NDEBUG
#  else
#    include <assert.h>
#  endif
#endif

#if defined __has_builtin
#  define jl_has_builtin(x) __has_builtin(x)
#else
#  define jl_has_builtin(x) 0
#endif

#ifdef JL_NDEBUG
# if jl_has_builtin(__builtin_assume)
#define jl_assume(cond)                  \
    (__extension__({                     \
        __typeof__(cond) cond_ = (cond); \
        __builtin_assume(!!(cond_));     \
        cond_;                           \
    }))
#elif defined(__GNUC__)
static inline void jl_assume_(int cond)
{
    if (!cond) {
        __builtin_unreachable();
    }
}
#define jl_assume(cond)                  \
    (__extension__({                     \
        __typeof__(cond) cond_ = (cond); \
        jl_assume_(!!(cond_));           \
        cond_;                           \
    }))
#else
#  define jl_assume(cond) (cond)
# endif
# if jl_has_builtin(__builtin_assume_aligned) || defined(_COMPILER_GCC_)
#  define jl_assume_aligned(ptr, align) __builtin_assume_aligned(ptr, align)
# elif defined(__GNUC__)
#define jl_assume_aligned(ptr, align)               \
    (__extension__({                                \
        __typeof__(ptr) ptr_ = (ptr);               \
        jl_assume(((uintptr_t)ptr_) % (align) == 0); \
        ptr_;                                       \
    }))
#elif defined(__cplusplus)
template<typename T>
static inline T
jl_assume_aligned(T ptr, unsigned align)
{
    (void)jl_assume(((uintptr_t)ptr) % align == 0);
    return ptr;
}
# else
#  define jl_assume_aligned(ptr, align) (ptr)
# endif
#else
# if defined(__GNUC__)
#define jl_assume(cond)                  \
    (__extension__({                     \
        __typeof__(cond) cond_ = (cond); \
        assert(!!(cond_));               \
        cond_;                           \
    }))
#define jl_assume_aligned(ptr, align)             \
    (__extension__({                              \
        __typeof__(ptr) ptr_ = (ptr);             \
        assert(((uintptr_t)ptr_) % (align) == 0); \
        ptr_;                                     \
    }))
# elif defined(__cplusplus)
#define jl_assume(cond)      \
    (([&]() {                \
        auto cond_ = (cond); \
        assert(!!(cond_));   \
        return cond_;        \
    })())
#define jl_assume_aligned(ptr, align)             \
    (([&]() {                                     \
        auto ptr_ = (ptr);                        \
        assert(((uintptr_t)ptr_) % (align) == 0); \
        return ptr_;                              \
    })())
#else
#  define jl_assume(cond) (cond)
#  define jl_assume_aligned(ptr, align) (ptr)
# endif
#endif

#ifdef JL_NDEBUG
# if jl_has_builtin(__builtin_unreachable) || defined(_COMPILER_GCC_) || defined(_COMPILER_INTEL_)
#   define jl_unreachable() __builtin_unreachable()
# else
#   define jl_unreachable() ((void)jl_assume(0))
# endif
#else
# define jl_unreachable() assert(0 && "unreachable")
#endif

#pragma GCC visibility pop

#endif /* JL_ASSERT_H */
