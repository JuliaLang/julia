// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_DTYPES_H
#define JL_DTYPES_H

#include <stddef.h>
#include <stddef.h> // double include of stddef.h fixes #3421
#include <stdint.h>
#if defined(_COMPILER_INTEL_)
#include <mathimf.h>
#else
#include <math.h>
#endif

#include "platform.h"
#include "analyzer_annotations.h"

#if !defined(_OS_WINDOWS_)
#include <inttypes.h>
#endif

#if defined(_OS_WINDOWS_)

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#if !defined(_COMPILER_MINGW_)

#define strtoull                                            _strtoui64
#define strtoll                                             _strtoi64
#define strcasecmp                                          _stricmp
#define strncasecmp                                         _strnicmp
#define snprintf                                            _snprintf
#define stat                                                _stat

#define STDIN_FILENO                                        0
#define STDOUT_FILENO                                       1
#define STDERR_FILENO                                       2

#endif /* !_COMPILER_MINGW_ */

#endif /* _OS_WINDOWS_ */


/*
  This file defines sane integer types for our target platforms. This
  library only runs on machines with the following characteristics:

  - supports integer word sizes of 8, 16, 32, and 64 bits
  - uses unsigned and signed 2's complement representations
  - all pointer types are the same size
  - there is an integer type with the same size as a pointer

  Some features require:
  - IEEE 754 single- and double-precision floating point

  We assume the LP64 convention for 64-bit platforms.
*/

#ifdef _OS_WINDOWS_
#define STDCALL __stdcall
# ifdef LIBRARY_EXPORTS
#  define JL_DLLEXPORT __declspec(dllexport)
# else
#  define JL_DLLEXPORT __declspec(dllimport)
# endif
#else
#define STDCALL
#define JL_DLLEXPORT __attribute__ ((visibility("default")))
#endif

#ifdef _OS_LINUX_
#include <endian.h>
#define LITTLE_ENDIAN  __LITTLE_ENDIAN
#define BIG_ENDIAN     __BIG_ENDIAN
#define PDP_ENDIAN     __PDP_ENDIAN
#define BYTE_ORDER     __BYTE_ORDER
#endif

#if defined(__APPLE__) || defined(__FreeBSD__)
#include <machine/endian.h>
#define __LITTLE_ENDIAN  LITTLE_ENDIAN
#define __BIG_ENDIAN     BIG_ENDIAN
#define __PDP_ENDIAN     PDP_ENDIAN
#define __BYTE_ORDER     BYTE_ORDER
#endif

#ifdef _OS_WINDOWS_
#define __LITTLE_ENDIAN    1234
#define __BIG_ENDIAN       4321
#define __PDP_ENDIAN       3412
#define __BYTE_ORDER       __LITTLE_ENDIAN
#define __FLOAT_WORD_ORDER __LITTLE_ENDIAN
#define LITTLE_ENDIAN      __LITTLE_ENDIAN
#define BIG_ENDIAN         __BIG_ENDIAN
#define PDP_ENDIAN         __PDP_ENDIAN
#define BYTE_ORDER         __BYTE_ORDER
#endif

#define LLT_ALLOC(n) malloc(n)
#define LLT_REALLOC(p,n) realloc((p),(n))
#define LLT_FREE(x) free(x)

#if defined(_OS_WINDOWS_) && defined(_COMPILER_INTEL_)
#  define STATIC_INLINE static
#elif defined(_OS_WINDOWS_) && defined(_COMPILER_MICROSOFT_)
#  define STATIC_INLINE static __inline
#else
#  define STATIC_INLINE static inline
#endif

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
#  define NOINLINE __declspec(noinline)
#  define NOINLINE_DECL(f) __declspec(noinline) f
#else
#  define NOINLINE __attribute__((noinline))
#  define NOINLINE_DECL(f) f __attribute__((noinline))
#endif

#ifdef _COMPILER_MICROSOFT_
# ifdef _P64
#  define JL_ATTRIBUTE_ALIGN_PTRSIZE(x) __declspec(align(8)) x
# else
#  define JL_ATTRIBUTE_ALIGN_PTRSIZE(x) __declspec(align(4)) x
# endif
#elif defined(__GNUC__)
#  define JL_ATTRIBUTE_ALIGN_PTRSIZE(x) x __attribute__ ((aligned (sizeof(void*))))
#else
#  define JL_ATTRIBUTE_ALIGN_PTRSIZE(x)
#endif

typedef int bool_t;
typedef unsigned char  byte_t;   /* 1 byte */

#ifdef _P64
#define TOP_BIT 0x8000000000000000
#define NBITS 64
typedef uint64_t uint_t;  // preferred int type on platform
typedef int64_t int_t;
#else
#define TOP_BIT 0x80000000
#define NBITS 32
typedef uint32_t uint_t;
typedef int32_t int_t;
#endif

STATIC_INLINE unsigned int next_power_of_two(unsigned int val) JL_NOTSAFEPOINT
{
    /* this function taken from libuv src/unix/core.c */
    val -= 1;
    val |= val >> 1;
    val |= val >> 2;
    val |= val >> 4;
    val |= val >> 8;
    val |= val >> 16;
    val += 1;
    return val;
}

#define LLT_ALIGN(x, sz) (((x) + (sz)-1) & -(sz))

// branch prediction annotations
#ifdef __GNUC__
#define __unlikely(x) __builtin_expect(!!(x), 0)
#define __likely(x)   __builtin_expect(!!(x), 1)
#define JL_EXTENSION __extension__
#else
#define __unlikely(x) (x)
#define __likely(x)   (x)
#define JL_EXTENSION
#endif

#define DBL_MAXINT 9007199254740992LL
#define FLT_MAXINT 16777216
#define U64_MAX    18446744073709551615ULL
#define S64_MAX    9223372036854775807LL
#define S64_MIN    (-S64_MAX - 1LL)
#define BIT63      0x8000000000000000LL
#define U32_MAX    4294967295L
#define S32_MAX    2147483647L
#define S32_MIN    (-S32_MAX - 1L)
#define BIT31      0x80000000

#define D_PNAN ((double)+NAN)
#define D_NNAN ((double)-NAN)
#define D_PINF ((double)+INFINITY)
#define D_NINF ((double)-INFINITY)
#define F_PNAN ((float)+NAN)
#define F_NNAN ((float)-NAN)
#define F_PINF ((float)+INFINITY)
#define F_NINF ((float)-INFINITY)

typedef enum { T_INT8, T_UINT8, T_INT16, T_UINT16, T_INT32, T_UINT32,
               T_INT64, T_UINT64, T_FLOAT, T_DOUBLE } numerictype_t;

#define N_NUMTYPES ((int)T_DOUBLE+1)

#ifdef _P64
# define T_PTRDIFF T_INT64
# define T_SIZE T_UINT64
#else
# define T_PTRDIFF T_INT32
# define T_SIZE T_UINT32
#endif

#if defined(__GNUC__) && __GNUC__ >= 7
#define JL_FALLTHROUGH __attribute__((fallthrough))
#elif defined(__cplusplus) && defined(__clang_major__) && \
    defined(__clang_minor__) && (__clang_major__ > 4 || __clang_minor__ >= 5)
// We require at least clang 3.x
#define JL_FALLTHROUGH [[clang::fallthrough]]
#else
#define JL_FALLTHROUGH
#endif

#if defined(__GNUC__)
#define JL_UNUSED __attribute__((__unused__))
#else
#define JL_UNUSED
#endif

#endif /* DTYPES_H */
