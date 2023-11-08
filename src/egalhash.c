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

#include "support/dtypes.h"
#include "support/hashing.h"
#include "egalhash.h"
#include "julia.h"

#include "support/htable.inc"

#ifdef __cplusplus
extern "C" {
#endif

#define OP_EQ(x, y) jl_egal((jl_value_t*)(x), (jl_value_t*)(y))

#define OP_HASH(x) jl_object_id((jl_value_t*)(x))

HTIMPL(egalhash, OP_HASH, OP_EQ)

#ifdef __cplusplus
}
#endif
