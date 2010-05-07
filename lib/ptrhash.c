/*
  pointer hash table
  optimized for storing info about particular values
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "dtypes.h"
#include "ptrhash.h"

#define OP_EQ(x,y) ((x)==(y))

#ifdef BITS64
static u_int64_t _pinthash(u_int64_t key)
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
#else
static u_int32_t _pinthash(u_int32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}
#endif

#include "htable.inc"

HTIMPL(ptrhash, _pinthash, OP_EQ)
