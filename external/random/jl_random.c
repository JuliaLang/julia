/*
  random numbers
*/

#include "jl_random.h"
#include "dsfmt-2.1/dSFMT.c"
#include "randmtzig.c"

#define RANDN_RESET -99999999

static double randn_bm_next = RANDN_RESET;

void dsfmt_randn_bm_reset(void)
{
    randn_bm_next = RANDN_RESET;
}

// Box Muller
double dsfmt_randn_bm(void)
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

// Ziggurat

uint32_t *ZT_STATE;
uint64_t *KI;
uint64_t *KE;
double *WI;
double *FI;
double *WE;
double *FE;

void randn_zig_init(uint32_t seed) {
    ZT_STATE = (uint32_t *) malloc ((MT_N+4) * sizeof(uint32_t));
    KI = (uint64_t *) malloc(ZIGGURAT_TABLE_SIZE * sizeof(uint64_t));
    KE = (uint64_t *) malloc(ZIGGURAT_TABLE_SIZE * sizeof(uint64_t));
    WI = (double *) malloc(ZIGGURAT_TABLE_SIZE * sizeof(double));
    FI = (double *) malloc(ZIGGURAT_TABLE_SIZE * sizeof(double));
    WE = (double *) malloc(ZIGGURAT_TABLE_SIZE * sizeof(double));
    FE = (double *) malloc(ZIGGURAT_TABLE_SIZE * sizeof(double));
    
    randmtzig_init_by_int(seed, ZT_STATE);

    return;
}

double randn_zig(void) {
    return randmtzig_randn(ZT_STATE, KI, KE, WI, FI, WE, FE);
}

double *randn_zig_fill_array(double *a, uint32_t n) {
    randmtzig_fill_drandn(n, a, ZT_STATE, KI, KE, WI, FI, WE, FE);
    return a;
}
