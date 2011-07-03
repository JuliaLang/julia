#ifndef __JL_RANDOM_H_
#define __JL_RANDOM_H_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <stdint.h>
#ifdef __APPLE__
#include "ieee754.h"
#else
#include <ieee754.h>
#endif

double rand_double();
float rand_float();
double randn();
void randomize();
uint32_t genrand_int32();
void randomseed32(uint32_t s);
void randomseed64(uint64_t s);

#endif
