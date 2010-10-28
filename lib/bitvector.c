/*
  bit vector primitives

  todo:
  * reverse
  * nreverse
 (- rotate left/right)
  * shl_to
  * not
  - shr_row, shl_row

  These routines are the back end supporting bit matrices. Many operations
  on bit matrices are slow (such as accessing or setting a single element!)
  but certain operations are privileged and lend themselves to extremely
  efficient implementation due to the bit-vector nature of machine integers.
  These are:
  done:
    &  |  $  ~  copy  reverse  fill  sum  prod
  todo:
    shift  trans  rowswap
  would be nice:
    channel  interleave

  Important note:
  Out-of-place functions always assume dest and source have the same amount
  of space available.

  shr_to, shl_to, not_to, and reverse_to assume source and dest don't overlap
  and_to, or_to, and xor_to allow overlap.
*/

#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "dtypes.h"
#include "bitvector.h"

#ifdef WIN32
#include <malloc.h>
#endif

u_int32_t *bitvector_resize(u_int32_t *b, uint64_t oldsz, uint64_t newsz,
                            int initzero)
{
    u_int32_t *p;
    size_t sz = ((newsz+31)>>5) * sizeof(uint32_t);
    p = LLT_REALLOC(b, sz);
    if (p == NULL) return NULL;
    if (initzero && newsz>oldsz) {
        size_t osz = ((oldsz+31)>>5) * sizeof(uint32_t);
        memset(&p[osz/sizeof(uint32_t)], 0, sz-osz);
    }
    return p;
}

u_int32_t *bitvector_new(u_int64_t n, int initzero)
{
    return bitvector_resize(NULL, 0, n, initzero);
}

size_t bitvector_nwords(u_int64_t nbits)
{
    return ((nbits+31)>>5);
}

void bitvector_set(u_int32_t *b, u_int64_t n, u_int32_t c)
{
    if (c)
        b[n>>5] |= (1<<(n&31));
    else
        b[n>>5] &= ~(1<<(n&31));
}

u_int32_t bitvector_get(u_int32_t *b, u_int64_t n)
{
    return b[n>>5] & (1<<(n&31));
}

static int ntz(uint32_t x)
{
    int n;

    if (x == 0) return 32;
    n = 1;
    if ((x & 0x0000FFFF) == 0) {n = n +16; x = x >>16;}
    if ((x & 0x000000FF) == 0) {n = n + 8; x = x >> 8;}
    if ((x & 0x0000000F) == 0) {n = n + 4; x = x >> 4;}
    if ((x & 0x00000003) == 0) {n = n + 2; x = x >> 2;}
    return n - (x & 1);
}

// given a bitvector of n bits, starting at bit n0 find the next
// set bit, including n0.
// returns n if no set bits.
uint32_t bitvector_next(uint32_t *b, uint64_t n0, uint64_t n)
{
    if (n0 >= n) return n;

    uint32_t i = n0>>5;
    uint32_t nb = n0&31;
    uint32_t nw = (n+31)>>5;
    uint32_t w;

    if (i < nw-1 || (n&31)==0)
        w = b[i]>>nb;
    else
        w = (b[i]&lomask(n&31))>>nb;
    if (w != 0)
        return ntz(w)+n0;
    if (i == nw-1)
        return n;
    i++;
    while (i < nw-1) {
        w = b[i];
        if (w != 0) {
            return ntz(w) + (i<<5);
        }
        i++;
    }
    w = b[i];
    nb = n&31;
    i = ntz(w);
    if (nb == 0)
        return i + (n-32);
    if (i >= nb)
        return n;
    return i + (n-nb);
}
