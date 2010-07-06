#ifndef __LLTRANDOM_H_
#define __LLTRANDOM_H_

DLLEXPORT double rand_double();
DLLEXPORT float rand_float();
double randn();
void randomize();
DLLEXPORT uint32_t genrand_int32();
void init_genrand(uint32_t s);
u_int64_t i64time();

#endif
