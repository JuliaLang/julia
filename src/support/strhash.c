// This file is a part of Julia. License is MIT: https://julialang.org/license

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
#include "hashing.h"
#include "strhash.h"

#define STR_EQ(x,y) (!strcmp((char*)(x), (char*)(y)))

#include "htable.inc"

#ifdef __cplusplus
extern "C" {
#endif

size_t simple_strhash(size_t key) JL_NOTSAFEPOINT
{
  size_t hash = 0;
  const char *str = (const char*)key;
  while (*str) {
    hash = (hash << 5) - hash + (unsigned char)*str++;
  }
  return hash;
}

HTIMPL(strhash, simple_strhash, STR_EQ)

#ifdef __cplusplus
}
#endif
