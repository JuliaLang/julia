#ifndef __HASHING_H_
#define __HASHING_H_

uint_t nextipow2(uint_t i);
u_int32_t int32hash(u_int32_t a);
u_int64_t int64hash(u_int64_t key);
u_int32_t int64to32hash(u_int64_t key);
#ifdef BITS64
#define inthash int64hash
#else
#define inthash int32hash
#endif
u_int64_t memhash(const char* buf, size_t n);
u_int32_t memhash32(const char* buf, size_t n);

#endif
