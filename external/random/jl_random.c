/*
  random numbers
*/

#include "jl_random.h"
#include "dsfmt-2.1/dSFMT.c"

#define RANDN_RESET -99999999

static double randn_next = RANDN_RESET;

void dsfmt_randn_reset()
{
    randn_next = RANDN_RESET;
}

double dsfmt_randn()
{
    double s, vre, vim, ure, uim;

    if (randn_next != RANDN_RESET) {
        s = randn_next;
        randn_next = RANDN_RESET;
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
    randn_next = s * vre;
    return s * vim;
}
