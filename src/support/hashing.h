// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_HASHING_H
#define JL_HASHING_H

#include "utils.h"
#include "dtypes.h"
#include "analyzer_annotations.h"

#ifdef __cplusplus
extern "C" {
#endif

uint_t nextipow2(uint_t i) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t int32hash(uint32_t a) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint64_t int64hash(uint64_t key) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t int64to32hash(uint64_t key) JL_NOTSAFEPOINT;
#ifdef _P64
#define inthash int64hash
#else
#define inthash int32hash
#endif
JL_DLLEXPORT uint64_t memhash(const char *buf, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint64_t memhash_seed(const char *buf, size_t n, uint32_t seed) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t memhash32(const char *buf, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t memhash32_seed(const char *buf, size_t n, uint32_t seed) JL_NOTSAFEPOINT;

#ifdef _P64
STATIC_INLINE uint64_t bitmix(uint64_t a, uint64_t b) JL_NOTSAFEPOINT
{
    return int64hash(a^bswap_64(b));
}
#else
STATIC_INLINE uint32_t bitmix(uint32_t a, uint32_t b) JL_NOTSAFEPOINT
{
    return int64to32hash((((uint64_t)a) << 32) | (uint64_t)b);
}
#endif
#define bitmix(a, b) (bitmix)((uintptr_t)(a), (uintptr_t)(b))

#ifdef __cplusplus
}
#endif

#endif
