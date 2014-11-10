#ifndef UTILS_H
#define UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

DLLEXPORT char *uint2str(char *dest, size_t len, uint64_t num, uint32_t base);
int str2int(char *str, size_t len, int64_t *res, uint32_t base);
int isdigit_base(char c, int base);

double conv_to_double(void *data, numerictype_t tag);
int64_t conv_to_int64(void *data, numerictype_t tag);
uint64_t conv_to_uint64(void *data, numerictype_t tag);
int32_t conv_to_int32(void *data, numerictype_t tag);
uint32_t conv_to_uint32(void *data, numerictype_t tag);
#ifdef _P64
#define conv_to_ptrdiff conv_to_int64
#define conv_to_size conv_to_uint64
#else
#define conv_to_ptrdiff conv_to_int32
#define conv_to_size conv_to_uint32
#endif
int cmp_same_lt(void *a, void *b, numerictype_t tag);
int cmp_same_eq(void *a, void *b, numerictype_t tag);
int cmp_lt(void *a, numerictype_t atag, void *b, numerictype_t btag);
int cmp_eq(void *a, numerictype_t atag, void *b, numerictype_t btag,
           int equalnans);

#ifdef __x86_64__
#  define LEGACY_REGS "=Q"
#else
#  define LEGACY_REGS "=q"
#endif

#if (!defined(__INTEL_COMPILER) || defined(__clang__)) && (defined(__i386__) || defined(__x86_64__))
STATIC_INLINE u_int16_t ByteSwap16(u_int16_t x)
{
  __asm("xchgb %b0,%h0" :
        LEGACY_REGS (x)	:
        "0" (x));
    return x;
}
#define bswap_16(x) ByteSwap16(x)

STATIC_INLINE u_int32_t ByteSwap32(u_int32_t x)
{
 __asm("bswap	%0":
      "=r" (x)     :
      "0" (x));
  return x;
}

#define bswap_32(x) ByteSwap32(x)

STATIC_INLINE u_int64_t ByteSwap64(u_int64_t x)
{
#ifdef __x86_64__
  __asm("bswap	%0":
        "=r" (x)     :
        "0" (x));
  return x;
#else
  register union { __extension__ u_int64_t __ll;
          u_int32_t __l[2]; } __x;
  asm("xchgl	%0,%1":
      "=r"(__x.__l[0]),"=r"(__x.__l[1]):
      "0"(bswap_32((unsigned long)x)),"1"(bswap_32((unsigned long)(x>>32))));
  return __x.__ll;
#endif
}
#define bswap_64(x) ByteSwap64(x)

#else

#define bswap_16(x) (((x) & 0x00ff) << 8 | ((x) & 0xff00) >> 8)

#if defined(__INTEL_COMPILER) && !defined(__clang__)
#define bswap_32(x) _bswap(x)
#else
#define bswap_32(x) \
     ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))
#endif

STATIC_INLINE u_int64_t ByteSwap64(u_int64_t x)
{
    union {
        u_int64_t ll;
        u_int32_t l[2];
    } w, r;
    w.ll = x;
    r.l[0] = bswap_32 (w.l[1]);
    r.l[1] = bswap_32 (w.l[0]);
    return r.ll;
}
#define bswap_64(x) ByteSwap64(x)

#endif

#ifdef __cplusplus
}
#endif

#endif
