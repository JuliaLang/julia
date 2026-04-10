// This file is a part of Julia. License is MIT: https://julialang.org/license

// Pure C implementation of arbitrary-width integer arithmetic
// for Julia's runtime intrinsics.

#include "APInt-C.h"
#include "julia_assert.h"
#include "julia_internal.h"

#include <math.h>
#include <string.h>

// bfloat conversion functions defined in runtime_intrinsics.c
// (half-float equivalents are declared in julia_internal.h)
extern uint16_t julia_float_to_bfloat(float param) JL_NOTSAFEPOINT;
extern float julia_bfloat_to_float(uint16_t param) JL_NOTSAFEPOINT;

// ---- Constants and macros ----

#define WORD_SIZE 64
#define APINT_NWORDS(nbits) (((nbits) + 63) / 64)
#define APINT_NBYTES(nbits) (((nbits) + 7) / 8)

// ---- Internal helper functions ----

static inline uint64_t top_word_mask(unsigned numbits)
{
    unsigned r = numbits % WORD_SIZE;
    return r ? ((UINT64_C(1) << r) - 1) : UINT64_MAX;
}

static inline void load(uint64_t *dst, const void *src, unsigned numbits)
{
    unsigned nw = APINT_NWORDS(numbits);
    unsigned nb = APINT_NBYTES(numbits);
    memset(dst, 0, nw * sizeof(uint64_t));
    memcpy(dst, src, nb);
    dst[nw - 1] &= top_word_mask(numbits);
}

static inline void store(void *dst, const uint64_t *src, unsigned numbits)
{
    memcpy(dst, src, APINT_NBYTES(numbits));
}

static inline void mask_top(uint64_t *a, unsigned nw, unsigned numbits)
{
    if (nw == 0) jl_unreachable();
    a[nw - 1] &= top_word_mask(numbits);
}

static inline int is_zero(const uint64_t *a, unsigned nw)
{
    for (unsigned i = 0; i < nw; i++)
        if (a[i] != 0) return 0;
    return 1;
}

static inline int is_negative(const uint64_t *a, unsigned numbits)
{
    unsigned nw = APINT_NWORDS(numbits);
    unsigned signbit = (numbits - 1) % WORD_SIZE;
    return (a[nw - 1] >> signbit) & 1;
}

static inline int compare_words(const uint64_t *a, const uint64_t *b, unsigned nw)
{
    for (int i = (int)nw - 1; i >= 0; i--) {
        if (a[i] < b[i]) return -1;
        if (a[i] > b[i]) return 1;
    }
    return 0;
}

// Add (negate_b=0) or subtract (negate_b=1) at the word level.
// a - b is computed as a + ~b + 1.
static inline unsigned addsub_words(uint64_t *r, const uint64_t *a,
                                    const uint64_t *b, unsigned nw,
                                    int negate_b)
{
    unsigned carry = negate_b; // +1 for two's complement subtraction
    for (unsigned i = 0; i < nw; i++) {
        uint64_t bw = negate_b ? ~b[i] : b[i];
        uint64_t s = a[i] + bw;
        unsigned c1 = s < a[i];
        uint64_t s2 = s + carry;
        unsigned c2 = s2 < s;
        r[i] = s2;
        carry = c1 | c2;
    }
    return carry;
}

#define add_words(r, a, b, nw)  addsub_words(r, a, b, nw, 0)
#define sub_words(r, a, b, nw)  addsub_words(r, a, b, nw, 1)

static inline void negate_words(uint64_t *r, const uint64_t *a, unsigned nw)
{
    unsigned carry = 1;
    for (unsigned i = 0; i < nw; i++) {
        uint64_t val = ~a[i] + carry;
        carry = (a[i] == 0) & carry;
        r[i] = val;
    }
}

// ---- Multiplication ----

#ifndef __SIZEOF_INT128__
static inline void mul64(uint64_t a, uint64_t b, uint64_t *lo, uint64_t *hi)
{
#ifdef _MSC_VER
    *lo = _umul128(a, b, hi);
#else
    uint32_t a0 = (uint32_t)a, a1 = (uint32_t)(a >> 32);
    uint32_t b0 = (uint32_t)b, b1 = (uint32_t)(b >> 32);
    uint64_t p00 = (uint64_t)a0 * b0;
    uint64_t p01 = (uint64_t)a0 * b1;
    uint64_t p10 = (uint64_t)a1 * b0;
    uint64_t p11 = (uint64_t)a1 * b1;
    uint64_t mid = p01 + p10;
    uint64_t mid_carry = (mid < p01) ? UINT64_C(1) : 0;
    uint64_t lo_result = p00 + (mid << 32);
    uint64_t lo_carry = (lo_result < p00) ? UINT64_C(1) : 0;
    *lo = lo_result;
    *hi = p11 + (mid >> 32) + (mid_carry << 32) + lo_carry;
#endif
}
#endif

// Multiply-accumulate helper for one limb pair
static inline void mac_limb(uint64_t *rword, uint64_t *carry,
                            uint64_t a, uint64_t b)
{
#ifdef __SIZEOF_INT128__
    __uint128_t prod = (__uint128_t)a * b + *rword + *carry;
    *rword = (uint64_t)prod;
    *carry = (uint64_t)(prod >> 64);
#else
    uint64_t plo, phi;
    mul64(a, b, &plo, &phi);
    uint64_t s1 = plo + *rword;
    unsigned c1 = s1 < plo;
    uint64_t s2 = s1 + *carry;
    unsigned c2 = s2 < s1;
    *rword = s2;
    *carry = phi + c1 + c2;
#endif
}

// Truncated multiply: r = (a * b) mod 2^(nw*64)
static void mul_words(uint64_t *r, const uint64_t *a,
                      const uint64_t *b, unsigned nw)
{
    memset(r, 0, nw * sizeof(uint64_t));
    for (unsigned i = 0; i < nw; i++) {
        if (a[i] == 0) continue;
        uint64_t carry = 0;
        for (unsigned j = 0; j < nw - i; j++)
            mac_limb(&r[i + j], &carry, a[i], b[j]);
    }
}


// ---- Division (binary long division) ----

static void udivrem(uint64_t *q, uint64_t *rem, const uint64_t *a,
                    const uint64_t *b, unsigned numbits, unsigned nw)
{
    memset(q, 0, nw * sizeof(uint64_t));
    memset(rem, 0, nw * sizeof(uint64_t));
    for (int i = (int)numbits - 1; i >= 0; i--) {
        // rem <<= 1
        for (int j = (int)nw - 1; j > 0; j--)
            rem[j] = (rem[j] << 1) | (rem[j - 1] >> 63);
        rem[0] <<= 1;
        // rem |= bit i of a
        rem[0] |= (a[i / WORD_SIZE] >> (i % WORD_SIZE)) & 1;
        // if rem >= b
        if (compare_words(rem, b, nw) >= 0) {
            sub_words(rem, rem, b, nw);
            q[i / WORD_SIZE] |= UINT64_C(1) << (i % WORD_SIZE);
        }
    }
}

// ---- Shift helpers ----

static inline unsigned extract_shift(const uint64_t *b, unsigned nw,
                                     unsigned numbits)
{
    for (unsigned i = 1; i < nw; i++)
        if (b[i] != 0) return numbits;
    return b[0] >= numbits ? numbits : (unsigned)b[0];
}

static void shl_n(uint64_t *r, const uint64_t *a,
                  unsigned shift, unsigned nw)
{
    unsigned ws = shift / WORD_SIZE;
    unsigned bs = shift % WORD_SIZE;
    for (int i = (int)nw - 1; i >= 0; i--) {
        uint64_t lo = ((unsigned)i >= ws) ? a[i - ws] : 0;
        uint64_t hi = ((unsigned)i > ws) ? a[i - ws - 1] : 0;
        r[i] = (bs == 0) ? lo : (lo << bs) | (hi >> (WORD_SIZE - bs));
    }
}

// Right shift by `shift` bits, filling vacated positions with `fill`.
// fill=0 for logical shift, fill=UINT64_MAX for arithmetic shift of negative.
static void shr_n(uint64_t *r, const uint64_t *a,
                  unsigned shift, unsigned nw, uint64_t fill)
{
    unsigned ws = shift / WORD_SIZE;
    unsigned bs = shift % WORD_SIZE;
    for (unsigned i = 0; i < nw; i++) {
        uint64_t hi = (i + ws < nw) ? a[i + ws] : fill;
        uint64_t lo = (i + ws + 1 < nw) ? a[i + ws + 1] : fill;
        r[i] = (bs == 0) ? hi : (hi >> bs) | (lo << (WORD_SIZE - bs));
    }
}

// Zero- or Sign-extend a numbits-wide value to fill total_nw words.
static inline void extend(uint64_t *a, unsigned numbits, unsigned total_nw, int is_sign)
{
    unsigned orig_nw = APINT_NWORDS(numbits);
    uint64_t fill = (is_sign && is_negative(a, numbits)) ? UINT64_MAX : 0;
    unsigned top_bits = numbits % WORD_SIZE;
    if (top_bits > 0 && fill)
        a[orig_nw - 1] |= ~top_word_mask(numbits);
    for (unsigned i = orig_nw; i < total_nw; i++)
        a[i] = fill;
}

// Check whether the bits above numbits in a total_nw-word value
// are all equal to `expected` (0 for unsigned, or sign-extension for signed).
static inline int check_overflow(const uint64_t *r, unsigned numbits,
                                 unsigned total_nw, uint64_t expected)
{
    unsigned nw = APINT_NWORDS(numbits);
    unsigned top_bits = numbits % WORD_SIZE;
    if (top_bits) {
        uint64_t high = r[nw - 1] >> top_bits;
        uint64_t exp_high = expected ? (UINT64_MAX >> top_bits) : 0;
        if (high != exp_high) return 1;
    }
    for (unsigned i = nw; i < total_nw; i++)
        if (r[i] != expected) return 1;
    return 0;
}

// ---- Int to double conversion ----

// Convert a multi-word unsigned integer to double, rounding correctly for a
// mantissa of `mbits` bits. The returned double fits losslessly in a (possibly
// narrower) floating-point type of `mbits` mantissa bits, avoiding double rounding.
static double uint_to_fp(const uint64_t *a, unsigned nw, unsigned mbits)
{
    // Find highest non-zero word
    int hw = (int)nw - 1;
    while (hw >= 0 && a[hw] == 0) hw--;
    if (hw < 0) return 0.0;

    // Only the top two words matter for rounding, except for the sticky bit.
    assert(mbits <= 53);
    uint64_t hi = a[hw];
    uint64_t lo = hw > 0 ? a[hw - 1] : 0;
    unsigned lz = __builtin_clzll(hi);
    unsigned shift = 64 - mbits - 2; // extract significand + round bit + "sticky" bit

    // Normalize: shift hi:lo left by lz so MSB sits at bit 63 of hi.
    uint64_t hi_n = lz ? (hi << lz) | (lo >> (64 - lz)) : hi;
    uint64_t lo_n = lz ? (lo << lz) : lo;
    uint64_t m = hi_n >> shift;

    // Sticky: OR of all bits below those extracted bits.
    int sticky = (hi_n & ((UINT64_C(1) << shift) - 1)) != 0 || lo_n != 0;
    for (int i = hw - 2; !sticky && i >= 0; i--) {
        if (a[i] != 0) { sticky = 1; }
    }
    m |= (uint64_t)sticky;

    // (double) on this integer does a single correctly-rounded conversion.
    // ldexp then scales to the original magnitude.
    return ldexp((double)m, (int)(hw * 64 + shift - lz));
}

// ---- Signed division helpers ----

static void sdivrem(uint64_t *q, uint64_t *rem,
                    const uint64_t *a, const uint64_t *b,
                    unsigned numbits, unsigned nw)
{
    int neg_a = is_negative(a, numbits);
    int neg_b = is_negative(b, numbits);
    uint64_t *abs_a = (uint64_t *)alloca(nw * sizeof(uint64_t));
    uint64_t *abs_b = (uint64_t *)alloca(nw * sizeof(uint64_t));
    if (neg_a) { negate_words(abs_a, a, nw); mask_top(abs_a, nw, numbits); }
    else memcpy(abs_a, a, nw * sizeof(uint64_t));
    if (neg_b) { negate_words(abs_b, b, nw); mask_top(abs_b, nw, numbits); }
    else memcpy(abs_b, b, nw * sizeof(uint64_t));
    udivrem(q, rem, abs_a, abs_b, numbits, nw);
    if (neg_a != neg_b) { negate_words(q, q, nw); mask_top(q, nw, numbits); }
    if (neg_a) { negate_words(rem, rem, nw); mask_top(rem, nw, numbits); }
}

// Count trailing zeros (count_ones=0) or ones (count_ones=1) in a loaded word array.
static inline unsigned countr(const uint64_t *a, unsigned nw, unsigned numbits,
                              int count_ones)
{
    unsigned count = 0;
    for (unsigned i = 0; i < nw; i++) {
        uint64_t v = count_ones ? ~a[i] : a[i];
        if (v != 0) {
#if defined(__GNUC__) || defined(__clang__)
            return count + (unsigned)__builtin_ctzll(v);
#else
            while ((v & 1) == 0) { v >>= 1; count++; }
            return count;
#endif
        }
        count += 64;
    }
    return numbits;
}

// Count leading zeros (count_ones=0) or ones (count_ones=1) in a loaded word array.
static inline unsigned countl(const uint64_t *a, unsigned nw, unsigned numbits,
                              int count_ones)
{
    unsigned count = 0;
    unsigned top_bits = numbits % WORD_SIZE;
    for (int i = (int)nw - 1; i >= 0; i--) {
        uint64_t val = a[i];
        if (i == (int)nw - 1 && top_bits > 0 && count_ones)
            val |= ~top_word_mask(numbits);
        uint64_t v = count_ones ? ~val : val;
        if (v != 0) {
#if defined(__GNUC__) || defined(__clang__)
            unsigned lz = (unsigned)__builtin_clzll(v);
#else
            unsigned lz = 0;
            while (!(v & (UINT64_C(1) << 63))) { v <<= 1; lz++; }
#endif
            if (i == (int)nw - 1 && top_bits > 0)
                return count + lz - (WORD_SIZE - top_bits);
            return count + lz;
        }
        if (i == (int)nw - 1 && top_bits > 0)
            count += top_bits;
        else
            count += WORD_SIZE;
    }
    return numbits;
}

static inline int is_min_int(const uint64_t *a, unsigned numbits)
{
    unsigned nw = APINT_NWORDS(numbits);
    return is_negative(a, numbits) && countr(a, nw, numbits, 0) == numbits - 1;
}

static inline int is_neg_one(const uint64_t *a, unsigned numbits, unsigned nw)
{
    for (unsigned i = 0; i < nw - 1; i++)
        if (a[i] != UINT64_MAX) return 0;
    return a[nw - 1] == top_word_mask(numbits);
}

// =====================================================================
// Exported functions
// =====================================================================

// ---- Basic arithmetic ----

JL_DLLEXPORT void APInt_neg(unsigned numbits, integerPart *pa, integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    negate_words(r, a, nw);
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
}

// Checked add (is_sub=0) or sub (is_sub=1), signed or unsigned.
// For subtraction, the overflow check uses the original sign of b,
// not the negated value, so the signed check is correct even for SMIN.
static int APInt_addsub_ov(unsigned numbits, integerPart *pa,
                           integerPart *pb, integerPart *pr,
                           int is_sub, int is_signed)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    int neg_a = is_negative(a, numbits);
    int neg_b = is_negative(b, numbits);
    if (is_sub) neg_b = !neg_b;
    addsub_words(r, a, b, nw, is_sub);
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
    int neg_r = is_negative(r, numbits);
    if (is_signed)
        return (neg_a == neg_b) && (neg_a != neg_r);
    int carry = (neg_a & neg_b) | ((neg_a | neg_b) & !neg_r);
    return is_sub ? !carry : carry;
}

JL_DLLEXPORT void APInt_add(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { APInt_addsub_ov(n, pa, pb, pr, 0, 0); }
JL_DLLEXPORT void APInt_sub(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { APInt_addsub_ov(n, pa, pb, pr, 1, 0); }

JL_DLLEXPORT void APInt_mul(unsigned numbits, integerPart *pa,
                            integerPart *pb, integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    mul_words(r, a, b, nw);
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
}

JL_DLLEXPORT void APInt_sdiv(unsigned numbits, integerPart *pa,
                             integerPart *pb, integerPart *pr)
{
    if (APInt_div_sov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

JL_DLLEXPORT void APInt_udiv(unsigned numbits, integerPart *pa,
                             integerPart *pb, integerPart *pr)
{
    if (APInt_div_uov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

JL_DLLEXPORT void APInt_srem(unsigned numbits, integerPart *pa,
                             integerPart *pb, integerPart *pr)
{
    if (APInt_rem_sov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

JL_DLLEXPORT void APInt_urem(unsigned numbits, integerPart *pa,
                             integerPart *pb, integerPart *pr)
{
    if (APInt_rem_uov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

// ---- Comparisons ----

static int APInt_cmp(unsigned numbits, integerPart *pa, integerPart *pb,
                     int is_signed)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    if (is_signed) {
        int neg_a = is_negative(a, numbits);
        int neg_b = is_negative(b, numbits);
        if (neg_a != neg_b) return neg_a ? -1 : 1;
    }
    return compare_words(a, b, nw);
}

JL_DLLEXPORT int APInt_eq(unsigned n, integerPart *pa, integerPart *pb)  { return APInt_cmp(n, pa, pb, 0) == 0; }
JL_DLLEXPORT int APInt_ne(unsigned n, integerPart *pa, integerPart *pb)  { return APInt_cmp(n, pa, pb, 0) != 0; }
JL_DLLEXPORT int APInt_slt(unsigned n, integerPart *pa, integerPart *pb) { return APInt_cmp(n, pa, pb, 1) <  0; }
JL_DLLEXPORT int APInt_ult(unsigned n, integerPart *pa, integerPart *pb) { return APInt_cmp(n, pa, pb, 0) <  0; }
JL_DLLEXPORT int APInt_sle(unsigned n, integerPart *pa, integerPart *pb) { return APInt_cmp(n, pa, pb, 1) <= 0; }
JL_DLLEXPORT int APInt_ule(unsigned n, integerPart *pa, integerPart *pb) { return APInt_cmp(n, pa, pb, 0) <= 0; }

// ---- Bitwise operations ----

JL_DLLEXPORT void APInt_and(unsigned numbits, integerPart *pa,
                           integerPart *pb, integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    for (unsigned i = 0; i < nw; i++) r[i] = a[i] & b[i];
    store(pr, r, numbits);
}

JL_DLLEXPORT void APInt_or(unsigned numbits, integerPart *pa,
                          integerPart *pb, integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    for (unsigned i = 0; i < nw; i++) r[i] = a[i] | b[i];
    store(pr, r, numbits);
}

JL_DLLEXPORT void APInt_xor(unsigned numbits, integerPart *pa,
                           integerPart *pb, integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    for (unsigned i = 0; i < nw; i++) r[i] = a[i] ^ b[i];
    store(pr, r, numbits);
}

JL_DLLEXPORT void APInt_not(unsigned numbits, integerPart *pa,
                                   integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    for (unsigned i = 0; i < nw; i++) r[i] = ~a[i];
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
}

// ---- Shifts ----

JL_DLLEXPORT void APInt_shl(unsigned numbits, integerPart *pa,
                           integerPart *pb, integerPart *pr)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    unsigned shift = extract_shift(b, nw, numbits);
    shl_n(r, a, shift, nw);
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
}

static void APInt_shr(unsigned numbits, integerPart *pa,
                      integerPart *pb, integerPart *pr, int is_signed)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    unsigned shift = extract_shift(b, nw, numbits);
    int neg = is_signed && is_negative(a, numbits);
    if (neg) extend(a, numbits, nw, is_signed);
    shr_n(r, a, shift, nw, neg ? UINT64_MAX : 0);
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
}

JL_DLLEXPORT void APInt_lshr(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { APInt_shr(n, pa, pb, pr, 0); }
JL_DLLEXPORT void APInt_ashr(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { APInt_shr(n, pa, pb, pr, 1); }

// ---- Overflow-checked arithmetic ----

JL_DLLEXPORT int APInt_add_uov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_addsub_ov(n, pa, pb, pr, 0, 0); }
JL_DLLEXPORT int APInt_add_sov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_addsub_ov(n, pa, pb, pr, 0, 1); }
JL_DLLEXPORT int APInt_sub_uov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_addsub_ov(n, pa, pb, pr, 1, 0); }
JL_DLLEXPORT int APInt_sub_sov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_addsub_ov(n, pa, pb, pr, 1, 1); }

static int APInt_mul_ov(unsigned numbits, integerPart *pa,
                       integerPart *pb, integerPart *pr, int is_signed)
{
    unsigned nw = APINT_NWORDS(numbits);
    unsigned nw2 = 2 * nw;
    uint64_t *a = (uint64_t *)alloca(nw2 * 8);
    uint64_t *b = (uint64_t *)alloca(nw2 * 8);
    uint64_t *r = (uint64_t *)alloca(nw2 * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    extend(a, numbits, nw2, is_signed);
    extend(b, numbits, nw2, is_signed);
    mul_words(r, a, b, nw2);
    uint64_t expected = (is_signed && is_negative(r, numbits)) ? UINT64_MAX : 0;
    int overflow = check_overflow(r, numbits, nw2, expected);
    mask_top(r, nw, numbits);
    store(pr, r, numbits);
    return overflow;
}

JL_DLLEXPORT int APInt_mul_uov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_mul_ov(n, pa, pb, pr, 0); }
JL_DLLEXPORT int APInt_mul_sov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_mul_ov(n, pa, pb, pr, 1); }

// returns quotient in *pq, remainder in *pr; returns 1 on div-by-zero or signed overflow
static int APInt_divrem(unsigned numbits, integerPart *pa, integerPart *pb,
                        integerPart *pq, integerPart *pr, int is_signed)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    uint64_t *b = (uint64_t *)alloca(nw * 8);
    uint64_t *q = (uint64_t *)alloca(nw * 8);
    uint64_t *rem = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    load(b, pb, numbits);
    if (is_zero(b, nw)) return 1;
    int overflow = 0;
    if (is_signed) {
        // SMIN / -1 overflows (quotient doesn't fit), but SMIN % -1 = 0 is
        // well-defined. Only flag overflow when the quotient is requested.
        overflow = pq && is_min_int(a, numbits) && is_neg_one(b, numbits, nw);
        sdivrem(q, rem, a, b, numbits, nw);
    }
    else {
        udivrem(q, rem, a, b, numbits, nw);
    }
    mask_top(q, nw, numbits);
    mask_top(rem, nw, numbits);
    if (pq) store(pq, q, numbits);
    if (pr) store(pr, rem, numbits);
    return overflow;
}

JL_DLLEXPORT int APInt_div_sov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_divrem(n, pa, pb, pr, NULL, 1); }
JL_DLLEXPORT int APInt_div_uov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_divrem(n, pa, pb, pr, NULL, 0); }
JL_DLLEXPORT int APInt_rem_sov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_divrem(n, pa, pb, NULL, pr, 1); }
JL_DLLEXPORT int APInt_rem_uov(unsigned n, integerPart *pa, integerPart *pb, integerPart *pr) { return APInt_divrem(n, pa, pb, NULL, pr, 0); }

// ---- Byte swap ----

JL_DLLEXPORT void APInt_bswap(unsigned numbits, integerPart *pa,
                                integerPart *pr)
{
    unsigned nb = APINT_NBYTES(numbits);
    unsigned nw = APINT_NWORDS(numbits);
    uint8_t *src = (uint8_t *)pa;
    uint64_t *r = (uint64_t *)alloca(nw * 8);
    memset(r, 0, nw * 8);
    uint8_t *dst = (uint8_t *)r;
    for (unsigned i = 0; i < nb; i++)
        dst[nb - 1 - i] = src[i];
    store(pr, r, numbits);
}

// ---- Bit counting ----

JL_DLLEXPORT unsigned APInt_popcount(unsigned numbits, integerPart *pa)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    unsigned count = 0;
    for (unsigned i = 0; i < nw; i++) {
#if defined(__GNUC__) || defined(__clang__)
        count += (unsigned)__builtin_popcountll(a[i]);
#else
        uint64_t v = a[i];
        for (; v; count++)
            v &= v - 1;
#endif
    }
    return count;
}

static unsigned APInt_countr(unsigned numbits, integerPart *pa, int count_ones)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    return countr(a, nw, numbits, count_ones);
}

static unsigned APInt_countl(unsigned numbits, integerPart *pa, int count_ones)
{
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);
    return countl(a, nw, numbits, count_ones);
}

JL_DLLEXPORT unsigned APInt_countr_zero(unsigned n, integerPart *pa) { return APInt_countr(n, pa, 0); }
JL_DLLEXPORT unsigned APInt_countr_one(unsigned n, integerPart *pa)  { return APInt_countr(n, pa, 1); }
JL_DLLEXPORT unsigned APInt_countl_zero(unsigned n, integerPart *pa) { return APInt_countl(n, pa, 0); }
JL_DLLEXPORT unsigned APInt_countl_one(unsigned n, integerPart *pa)  { return APInt_countl(n, pa, 1); }

// ---- FP conversions ----

static int FPtoInt(jl_datatype_t *ty, void *pa, jl_datatype_t *oty,
                   integerPart *pr, int isSigned)
{
    double Val;
    if (ty == jl_float16_type)
        Val = julia_half_to_float(*(uint16_t *)pa);
    else if (ty == jl_bfloat16_type)
        Val = julia_bfloat_to_float(*(uint16_t *)pa);
    else if (ty == jl_float32_type)
        Val = *(float *)pa;
    else if (ty == jl_float64_type)
        Val = *(double *)pa;
    else
        jl_error("FPtoSI: runtime floating point intrinsics are not "
                 "implemented for bit sizes other than 16, 32 and 64");

    unsigned onumbytes = jl_datatype_size(oty);
    unsigned onumbits = onumbytes * 8;
    unsigned nw = APINT_NWORDS(onumbits);
    uint64_t *result = (uint64_t *)alloca(nw * sizeof(uint64_t));
    memset(result, 0, nw * sizeof(uint64_t));

    double absVal = fabs(Val);
    if (isfinite(Val)) {
        if (absVal >= 0x1p53) {
            int exp;
            double frac = frexp(absVal, &exp);
            uint64_t mantissa = (uint64_t)ldexp(frac, 53);
            unsigned shift = (unsigned)(exp - 53);
            unsigned ws = shift / WORD_SIZE;
            unsigned bs = shift % WORD_SIZE;
            if (ws < nw) {
                result[ws] = mantissa << bs;
                if (bs > 0 && ws + 1 < nw)
                    result[ws + 1] = mantissa >> (WORD_SIZE - bs);
            }
        }
        else {
            result[0] = (uint64_t)absVal;
        }
    }

    if (isSigned && Val < 0) {
        negate_words(result, result, nw);
        mask_top(result, nw, onumbits);
    }

    memcpy(pr, result, onumbytes);

    double limit = ldexp(1.0, isSigned ? onumbits - 1 : onumbits);
    return (trunc(Val) == Val) && (absVal < limit);
}

JL_DLLEXPORT void APInt_fptosi(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr) { FPtoInt(ty, pa, oty, pr, 1); }
JL_DLLEXPORT void APInt_fptoui(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr) { FPtoInt(ty, pa, oty, pr, 0); }
JL_DLLEXPORT int APInt_fptosi_exact(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr) { return FPtoInt(ty, pa, oty, pr, 1); }
JL_DLLEXPORT int APInt_fptoui_exact(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr) { return FPtoInt(ty, pa, oty, pr, 0); }

static void APInt_inttofp(jl_datatype_t *ty, integerPart *pa,
                         jl_datatype_t *oty, integerPart *pr, int is_signed)
{
    unsigned numbytes = jl_datatype_size(ty);
    unsigned numbits = numbytes * 8;
    unsigned nw = APINT_NWORDS(numbits);
    uint64_t *a = (uint64_t *)alloca(nw * 8);
    load(a, pa, numbits);

    // Mantissa bits (including implicit leading 1) for each target format.
    unsigned mbits;
    if (oty == jl_float16_type)
        mbits = 11;
    else if (oty == jl_bfloat16_type)
        mbits = 8;
    else if (oty == jl_float32_type)
        mbits = 24;
    else if (oty == jl_float64_type)
        mbits = 53;
    else
        jl_error("IntToFP: runtime floating point intrinsics are not "
                 "implemented for bit sizes other than 16, 32 and 64");

    // uint_to_fp returns a double that is already rounded sufficiently
    // to cast to the target type losslessly.
    double val;
    if (is_signed && is_negative(a, numbits)) {
        uint64_t *abs_a = (uint64_t *)alloca(nw * 8);
        negate_words(abs_a, a, nw);
        mask_top(abs_a, nw, numbits);
        val = -uint_to_fp(abs_a, nw, mbits);
    }
    else {
        val = uint_to_fp(a, nw, mbits);
    }

    if (oty == jl_float16_type)
        *(uint16_t *)pr = julia_float_to_half((float)val);
    else if (oty == jl_bfloat16_type)
        *(uint16_t *)pr = julia_float_to_bfloat((float)val);
    else if (oty == jl_float32_type)
        *(float *)pr = (float)val;
    else if (oty == jl_float64_type)
        *(double *)pr = val;
}

JL_DLLEXPORT void APInt_sitofp(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr) { APInt_inttofp(ty, pa, oty, pr, 1); }
JL_DLLEXPORT void APInt_uitofp(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr) { APInt_inttofp(ty, pa, oty, pr, 0); }

// ---- Sign/Zero extend, Truncate ----
// (These are already pure C in the original, copied here.)

JL_DLLEXPORT void APInt_sext(jl_datatype_t *ty, integerPart *pa,
                            jl_datatype_t *otys, integerPart *pr)
{
    unsigned inumbytes = jl_datatype_size(ty);
    unsigned onumbytes = jl_datatype_size(otys);
    if (!(onumbytes > inumbytes))
        jl_error("SExt: output bitsize must be > input bitsize");
    unsigned inumbits = inumbytes * 8;
    int bits = (0 - inumbits) % 8;
    int signbit = (inumbits - 1) % 8;
    int sign = ((unsigned char *)pa)[inumbytes - 1] & (1 << signbit) ? -1 : 0;
    memcpy(pr, pa, inumbytes);
    if (bits) {
        ((signed char *)pr)[inumbytes - 1] =
            ((signed char *)pa)[inumbytes - 1] << bits >> bits;
    }
    memset((char *)pr + inumbytes, sign, onumbytes - inumbytes);
}

JL_DLLEXPORT void APInt_zext(jl_datatype_t *ty, integerPart *pa,
                            jl_datatype_t *otys, integerPart *pr)
{
    unsigned inumbytes = jl_datatype_size(ty);
    unsigned onumbytes = jl_datatype_size(otys);
    if (!(onumbytes > inumbytes))
        jl_error("ZExt: output bitsize must be > input bitsize");
    unsigned inumbits = inumbytes * 8;
    int bits = (0 - inumbits) % 8;
    memcpy(pr, pa, inumbytes);
    if (bits) {
        ((unsigned char *)pr)[inumbytes - 1] =
            ((unsigned char *)pa)[inumbytes - 1] << bits >> bits;
    }
    memset((char *)pr + inumbytes, 0, onumbytes - inumbytes);
}

JL_DLLEXPORT void APInt_trunc(jl_datatype_t *ty, integerPart *pa,
                             jl_datatype_t *otys, integerPart *pr)
{
    unsigned inumbytes = jl_datatype_size(ty);
    unsigned onumbytes = jl_datatype_size(otys);
    if (!(onumbytes < inumbytes))
        jl_error("Trunc: output bitsize must be < input bitsize");
    memcpy(pr, pa, onumbytes);
}

// ---- Misc ----


JL_DLLEXPORT void APInt_flipsign(unsigned numbits, integerPart *pa,
                                   integerPart *pb, integerPart *pr)
{
    unsigned numbytes = APINT_NBYTES(numbits);
    int signbit = (numbits - 1) % 8;
    int sign = ((unsigned char *)pb)[numbytes - 1] & (1 << signbit);
    if (sign)
        APInt_neg(numbits, pa, pr);
    else
        memcpy(pr, pa, numbytes);
}
