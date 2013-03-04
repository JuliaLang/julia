#ifndef HASHING_H
#define HASHING_H

uint_t nextipow2(uint_t i);
DLLEXPORT u_int32_t int32hash(u_int32_t a);
DLLEXPORT u_int64_t int64hash(u_int64_t key);
DLLEXPORT u_int32_t int64to32hash(u_int64_t key);
#ifdef _P64
#define inthash int64hash
#else
#define inthash int32hash
#endif
DLLEXPORT u_int64_t memhash(const char* buf, size_t n);
DLLEXPORT u_int64_t memhash_seed(const char* buf, size_t n, u_int32_t seed);
DLLEXPORT u_int32_t memhash32(const char* buf, size_t n);
DLLEXPORT u_int32_t memhash32_seed(const char* buf, size_t n, u_int32_t seed);
#endif
