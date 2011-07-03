/*
  random numbers
*/
#include "jl_random.h"

#include "mt19937ar.c"

double rand_double()
{
    union ieee754_double d;

    d.ieee.mantissa0 = genrand_int32();
    d.ieee.mantissa1 = genrand_int32();
    d.ieee.negative = 0;
    d.ieee.exponent = IEEE754_DOUBLE_BIAS + 0;    /* 2^0 */
    return d.d - 1.0;
}

float rand_float()
{
    union ieee754_float f;

    f.ieee.mantissa = genrand_int32();
    f.ieee.negative = 0;
    f.ieee.exponent = IEEE754_FLOAT_BIAS + 0;     /* 2^0 */
    return f.f - 1.0;
}

static double randn_next = -42;
double randn()
{
    double s, vre, vim, ure, uim;

    if (randn_next != -42) {
        s = randn_next;
        randn_next = -42;
        return s;
    }
    do {
        ure = rand_double();
        uim = rand_double();
        vre = 2*ure - 1;
        vim = 2*uim - 1;
        s = vre*vre + vim*vim;
    } while (s >= 1);
    s = sqrt(-2*log(s)/s);
    randn_next = s * vre;
    return s * vim;
}

void randomseed32(uint32_t s)
{
    init_genrand(s);
    randn_next = -42;
}

void randomseed64(uint64_t s)
{
    init_by_array((uint32_t*)&s, 2);
    randn_next = -42;
}

void randomize()
{
    struct timeval now;
    uint64_t a;

    gettimeofday(&now, NULL);
    a = (((u_int64_t)now.tv_sec)<<32) + (u_int64_t)now.tv_usec;

    randomseed64(a);
}
