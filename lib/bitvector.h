#ifndef __BITVECTOR_H_
#define __BITVECTOR_H_

// a mask with n set lo or hi bits
#define lomask(n) (u_int32_t)((((u_int32_t)1)<<(n))-1)
#define himask(n) (~lomask(32-n))
#define ONES32 ((u_int32_t)0xffffffff)

#ifdef __INTEL_COMPILER
#define count_bits(b) _popcnt32(b)
#else
static inline u_int32_t count_bits(u_int32_t b)
{
    b = b - ((b>>1)&0x55555555);
    b = ((b>>2)&0x33333333) + (b&0x33333333);
    b = ((b>>4)+b)&0x0f0f0f0f;
    b += (b>>8);
    b += (b>>16);
    return b & 0x3f;
    // here is the non-optimized version, for clarity:
    /*
    b = ((b>> 1)&0x55555555) + (b&0x55555555);
    b = ((b>> 2)&0x33333333) + (b&0x33333333);
    b = ((b>> 4)&0x0f0f0f0f) + (b&0x0f0f0f0f);
    b = ((b>> 8)&0x00ff00ff) + (b&0x00ff00ff);
    b = ((b>>16)&0x0000ffff) + (b&0x0000ffff);
    return b & 0x3f;
    */
}
#endif

u_int32_t bitreverse(u_int32_t x);

u_int32_t *bitvector_new(u_int64_t n, int initzero);
u_int32_t *bitvector_resize(u_int32_t *b, uint64_t oldsz, uint64_t newsz,
                            int initzero);
size_t bitvector_nwords(u_int64_t nbits);
void bitvector_set(u_int32_t *b, u_int64_t n, u_int32_t c);
u_int32_t bitvector_get(u_int32_t *b, u_int64_t n);

uint32_t bitvector_next(uint32_t *b, uint64_t n0, uint64_t n);

void bitvector_shr(u_int32_t *b, size_t n, u_int32_t s);
void bitvector_shr_to(u_int32_t *dest, u_int32_t *b, size_t n, u_int32_t s);
void bitvector_shl(u_int32_t *b, size_t n, u_int32_t s);
void bitvector_shl_to(u_int32_t *dest, u_int32_t *b, size_t n, u_int32_t s,
                      bool_t scrap);
void bitvector_fill(u_int32_t *b,u_int32_t offs, u_int32_t c, u_int32_t nbits);
void bitvector_copy(u_int32_t *dest, u_int32_t doffs,
                    u_int32_t *a, u_int32_t aoffs, u_int32_t nbits);
void bitvector_not(u_int32_t *b, u_int32_t offs, u_int32_t nbits);
void bitvector_not_to(u_int32_t *dest, u_int32_t doffs,
                      u_int32_t *a, u_int32_t aoffs, u_int32_t nbits);
void bitvector_reverse(u_int32_t *b, u_int32_t offs, u_int32_t nbits);
void bitvector_reverse_to(u_int32_t *dest, u_int32_t *src, u_int32_t soffs,
                          u_int32_t nbits);
void bitvector_and_to(u_int32_t *dest, u_int32_t doffs,
                      u_int32_t *a, u_int32_t aoffs,
                      u_int32_t *b, u_int32_t boffs, u_int32_t nbits);
void bitvector_or_to(u_int32_t *dest, u_int32_t doffs,
                     u_int32_t *a, u_int32_t aoffs,
                     u_int32_t *b, u_int32_t boffs, u_int32_t nbits);
void bitvector_xor_to(u_int32_t *dest, u_int32_t doffs,
                      u_int32_t *a, u_int32_t aoffs,
                      u_int32_t *b, u_int32_t boffs, u_int32_t nbits);
u_int64_t bitvector_count(u_int32_t *b, u_int32_t offs, u_int64_t nbits);
u_int32_t bitvector_any0(u_int32_t *b, u_int32_t offs, u_int32_t nbits);
u_int32_t bitvector_any1(u_int32_t *b, u_int32_t offs, u_int32_t nbits);

#endif
