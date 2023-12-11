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
uint32_t int32hash(uint32_t a) JL_NOTSAFEPOINT;
uint64_t int64hash(uint64_t key) JL_NOTSAFEPOINT;
uint32_t int64to32hash(uint64_t key) JL_NOTSAFEPOINT;
#ifdef _P64
#define inthash int64hash
#else
#define inthash int32hash
#endif
JL_DLLEXPORT uint64_t memhash(const char *buf, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint64_t memhash_seed(const char *buf, size_t n, uint32_t seed) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t memhash32(const char *buf, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t memhash32_seed(const char *buf, size_t n, uint32_t seed) JL_NOTSAFEPOINT;

// FxHasher
#ifdef _P64
STATIC_INLINE uint64_t bitmix(uint64_t h, uint64_t a) JL_NOTSAFEPOINT
{
    h = (h << 5) | (h >> (sizeof(h) * 8 - 5)); // rotate 5 bits to the left
    h ^= a;
    h *= 0x517cc1b727220a95;
    return h;
}
#else
STATIC_INLINE uint32_t bitmix(uint32_t h, uint32_t a) JL_NOTSAFEPOINT
{
    h = (h << 5) | (h >> (sizeof(h) * 8 - 5)); // rotate 5 bits to the left
    h ^= a;
    h *= 0x9e3779b9;
    return h;
}
#endif
#define bitmix(h, a) (bitmix)((uintptr_t)(h), (uintptr_t)(a))

#ifdef __cplusplus
}
#endif

#endif
