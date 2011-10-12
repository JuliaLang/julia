#ifndef __JL_RANDOM_H_
#define __JL_RANDOM_H_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <stdint.h>

double rand_double();
float rand_float();
double randn();
void randomize();
uint32_t genrand_int32();
void randomseed32(uint32_t s);
void randomseed64(uint64_t s);
double dsfmt_randn();
uint32_t dsfmt_gv_genrand_uint32(void);

void randmtzig_init_by_int (uint32_t s, uint32_t *state);
double randmtzig_randn (uint32_t *state, uint64_t *ki, uint64_t *ke, double *wi, double *fi, double *we, double *fe);
void randmtzig_fill_drandn (int32_t n, double *p, uint32_t *state, uint64_t *ki, uint64_t *ke, double *wi, double *fi, double *we, double *fe);

void randn_zig_init(uint32_t seed);
double randn_zig(void);
double *randn_zig_fill_array(double *a, uint32_t n);

#endif
