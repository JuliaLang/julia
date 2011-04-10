#ifndef __LLTRANDOM_H_
#define __LLTRANDOM_H_

DLLEXPORT double rand_double();
DLLEXPORT float rand_float();
DLLEXPORT double randn();
void randomize();
DLLEXPORT uint32_t genrand_int32();
u_int64_t i64time();
DLLEXPORT void randomseed32(uint32_t s);
DLLEXPORT void randomseed64(uint64_t s);

#endif
