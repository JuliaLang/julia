// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef UTILS_H
#define UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT char *uint2str(char *dest, size_t len, uint64_t num, uint32_t base);
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

#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ > 4 || __GNUC_MINOR__ >= 8))
#define bswap_16(x) __builtin_bswap16(x)
#define bswap_32(x) __builtin_bswap32(x)
#define bswap_64(x) __builtin_bswap64(x)
#elif defined(_MSC_VER)
#define bswap_16(x) _byteswap_ushort(x)
#define bswap_32(x) _byteswap_ulong(x)
#define bswap_64(x) _byteswap_uint64(x)
#elif defined(__INTEL_COMPILER)
#define bswap_16(x) _bswap16(x)
#define bswap_32(x) _bswap(x)
#define bswap_64(x) _bswap64(x)
#else
#define bswap_16(x) (((x) & 0x00ff) << 8 | ((x) & 0xff00) >> 8)
#define bswap_32(x) \
     ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))
STATIC_INLINE uint64_t ByteSwap64(uint64_t x)
{
    uint32_t high = (uint32_t) (x >> 32);
    uint32_t low  = (uint32_t)  x;
    return  ((uint64_t) bswap_32 (high)) |
           (((uint64_t) bswap_32 (low)) << 32);
}
#define bswap_64(x) ByteSwap64(x)
#endif

#ifdef __cplusplus
}
#endif

#endif
