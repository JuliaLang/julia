// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  bit vector primitives
*/

#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "dtypes.h"
#include "bitvector.h"

#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

uint32_t *bitvector_resize(uint32_t *b, uint64_t oldsz, uint64_t newsz,
                            int initzero)
{
    uint32_t *p;
    size_t sz = ((newsz+31)>>5) * sizeof(uint32_t);
    p = (uint32_t*)LLT_REALLOC(b, sz);
    if (p == NULL) return NULL;
    if (initzero && newsz>oldsz) {
        size_t osz = ((oldsz+31)>>5) * sizeof(uint32_t);
        memset(&p[osz/sizeof(uint32_t)], 0, sz-osz);
    }
    return p;
}

uint32_t *bitvector_new(uint64_t n, int initzero)
{
    return bitvector_resize(NULL, 0, n, initzero);
}

size_t bitvector_nwords(uint64_t nbits)
{
    return ((nbits+31)>>5);
}

void bitvector_set(uint32_t *b, uint64_t n, uint32_t c)
{
    if (c)
        b[n>>5] |= (1<<(n&31));
    else
        b[n>>5] &= ~(1<<(n&31));
}

uint32_t bitvector_get(uint32_t *b, uint64_t n)
{
    return b[n>>5] & (1<<(n&31));
}

#ifdef __cplusplus
}
#endif
