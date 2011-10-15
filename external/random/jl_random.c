/*
  random numbers
*/

#include "jl_random.h"
#include "dsfmt-2.1/dSFMT.c"
#include "randmtzig.c"

#define RANDN_RESET -99999999

static double randn_bm_next = RANDN_RESET;

void randn_bm_reset(void);
double randn_bm(void);

void randn_bm_reset(void)
{
    randn_bm_next = RANDN_RESET;
}

// Box Muller
double randn_bm(void)
{
    double s, vre, vim, ure, uim;

    if (randn_bm_next != RANDN_RESET) {
        s = randn_bm_next;
        randn_bm_next = RANDN_RESET;
        return s;
    }
    do {
        ure = dsfmt_gv_genrand_close1_open2();
        uim = dsfmt_gv_genrand_close1_open2();
        vre = 2*ure - 3;
        vim = 2*uim - 3;
        s = vre*vre + vim*vim;
    } while (s >= 1);
    s = sqrt(-2*log(s)/s);
    randn_bm_next = s * vre;
    return s * vim;
}
