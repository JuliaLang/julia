#ifndef __UTILS_H_
#define __UTILS_H_

/* these functions byteswap any-size units --------------------- */
void bswap(byte_t *s, size_t n);
void bswap_to(byte_t *dest, byte_t *src, size_t n);
void bswap_buffer(byte_t *data, size_t sz, size_t npts);
/* ------------------------------------------------------------- */

DLLEXPORT int double_exponent(double d);
DLLEXPORT double double_mantissa(double d);
DLLEXPORT int float_exponent(float f);
DLLEXPORT float float_mantissa(float f);
void snprint_real(char *s, size_t cnt, double r,
                  int width,    // printf field width, or 0
                  int dec,      // # decimal digits desired, recommend 16
                  // # of zeros in .00...0x before using scientific notation
                  // recommend 3-4 or so
                  int max_digs_rt,
                  // # of digits left of decimal before scientific notation
                  // recommend 10
                  int max_digs_lf);
void snprint_cplx(char *s, size_t cnt, double re, double im,
                  // args to pass on to snprint_real
                  int width, int dec,
                  int max_digs_rt, int max_digs_lf,
                  // print spaces around sign in a+bi
                  int spflag);

DLLEXPORT char *uint2str(char *dest, size_t len, uint64_t num, uint32_t base);
int str2int(char *str, size_t len, int64_t *res, uint32_t base);
int isdigit_base(char c, int base);

extern double trunc(double x);

STATIC_INLINE double fpart(double arg)
{
    return arg - trunc(arg);
}

#define ipart(x) trunc(x)

numerictype_t effective_numerictype(double r);
double conv_to_double(void *data, numerictype_t tag);
void conv_from_double(void *data, double d, numerictype_t tag);
int64_t conv_to_int64(void *data, numerictype_t tag);
uint64_t conv_to_uint64(void *data, numerictype_t tag);
int32_t conv_to_int32(void *data, numerictype_t tag);
uint32_t conv_to_uint32(void *data, numerictype_t tag);
#ifdef __LP64__
#define conv_to_long conv_to_int64
#define conv_to_ulong conv_to_uint64
#else
#define conv_to_long conv_to_int32
#define conv_to_ulong conv_to_uint32
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

#if !defined(__INTEL_COMPILER) && (defined(__i386__) || defined(__x86_64__))
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

#ifdef __INTEL_COMPILER
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

#endif
