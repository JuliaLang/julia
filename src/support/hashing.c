/*
  Hashing
*/
#include <stdlib.h>
#include "dtypes.h"
#include "utils.h"
#include "hashing.h"
#include "timefuncs.h"
#include "ios.h"

uint_t nextipow2(uint_t i)
{
    if (i==0) return 1;
    if ((i&(i-1))==0) return i;
    if (i&TOP_BIT) return TOP_BIT;

    // repeatedly clear bottom bit
    while (i&(i-1))
        i = i&(i-1);

    return i<<1;
}

u_int32_t int32hash(u_int32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}

u_int64_t int64hash(u_int64_t key)
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

u_int32_t int64to32hash(u_int64_t key)
{
    key = (~key) + (key << 18); // key = (key << 18) - key - 1;
    key =   key  ^ (key >> 31);
    key = key * 21;             // key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return (u_int32_t)key;
}

#include "MurmurHash3.c"

#define _MHASH_SEED_ 0xcafe8881

uint64_t memhash(const char* buf, size_t n)
{
    uint64_t out[2];

    // TODO: expose 128-bit hash
#ifdef _P64
    MurmurHash3_x64_128(buf, n, _MHASH_SEED_, out);
#else
    MurmurHash3_x86_128(buf, n, _MHASH_SEED_, out);
#endif
    return out[1];
}

uint64_t memhash_seed(const char* buf, size_t n, uint32_t seed)
{
    uint64_t out[2];

    // TODO: expose 128-bit hash
#ifdef _P64
    MurmurHash3_x64_128(buf, n, seed, out);
#else
    MurmurHash3_x86_128(buf, n, seed, out);
#endif
    return out[1];
}

uint32_t memhash32(const char* buf, size_t n)
{
    uint32_t out;

    MurmurHash3_x86_32(buf, n, _MHASH_SEED_, &out);
    return out;
}

uint32_t memhash32_seed(const char* buf, size_t n, uint32_t seed)
{
    uint32_t out;

    MurmurHash3_x86_32(buf, n, seed, &out);
    return out;
}
