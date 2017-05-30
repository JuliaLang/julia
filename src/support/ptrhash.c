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
#include "ptrhash.h"

#define OP_EQ(x,y) ((x)==(y))

#include "htable.inc"

#ifdef __cplusplus
extern "C" {
#endif

HTIMPL(ptrhash, inthash, OP_EQ)

#ifdef __cplusplus
}
#endif
