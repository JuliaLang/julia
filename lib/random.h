#ifndef __LLTRANDOM_H_
#define __LLTRANDOM_H_

#define random() genrand_int32()
#define srandom(n) init_genrand(n)
double rand_double();
float rand_float();
double randn();
void randomize();
unsigned long genrand_int32();
void init_genrand(unsigned long s);
u_int64_t i64time();

#endif
