// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Hashing
*/
#include <stdlib.h>
#include "dtypes.h"
#include "utils.h"
#include "hashing.h"
#include "timefuncs.h"
#include "ios.h"
#include <xxhash.h>

#ifdef __cplusplus
extern "C" {
#endif

uint32_t int32hash(uint32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}

uint64_t int64hash(uint64_t key)
{
    key = (~key) + (key << 21);            // key = (key << 21) - key - 1;
    key =   key  ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); // key * 265
    key =  key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); // key * 21
    key =  key ^ (key >> 28);
    key =  key + (key << 31);
    return key;
}

uint32_t int64to32hash(uint64_t key)
{
    key = (~key) + (key << 18); // key = (key << 18) - key - 1;
    key =   key  ^ (key >> 31);
    key = key * 21;             // key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return (uint32_t)key;
}

#define JL_XXHASH_DEFAULT_SEED ((XXH64_hash_t)0xcafe8881ULL)

static inline XXH64_hash_t jl_xxhash_seed(uint32_t seed)
{
    return (XXH64_hash_t)seed;
}

uint64_t memhash(const char *buf, size_t n)
{
    return (uint64_t)XXH3_64bits_withSeed(buf, n, JL_XXHASH_DEFAULT_SEED);
}

uint64_t memhash_seed(const char *buf, size_t n, uint32_t seed)
{
    const XXH64_hash_t seed64 = jl_xxhash_seed(seed);
    return (uint64_t)XXH3_64bits_withSeed(buf, n, seed64);
}

uint32_t memhash32(const char *buf, size_t n)
{
    return (uint32_t)XXH3_64bits_withSeed(buf, n, JL_XXHASH_DEFAULT_SEED);
}

uint32_t memhash32_seed(const char *buf, size_t n, uint32_t seed)
{
    const XXH64_hash_t seed64 = jl_xxhash_seed(seed);
    return (uint32_t)XXH3_64bits_withSeed(buf, n, seed64);
}

#ifdef __cplusplus
}
#endif
