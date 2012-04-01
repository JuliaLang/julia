/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2003-2007     The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *
 *  SYNOPSIS
 *
 *	#include <Rmath.h>
 *	void rmultinom(int n, double* prob, int K, int* rN);
 *
 *  DESCRIPTION
 *
 *	Random Vector from the multinomial distribution.
 *             ~~~~~~
 *  NOTE
 *	Because we generate random _vectors_ this doesn't fit easily
 *	into the do_random[1-4](.) framework setup in ../main/random.c
 *	as that is used only for the univariate random generators.
 *      Multivariate distributions typically have too complex parameter spaces
 *	to be treated uniformly.
 *	=> Hence also can have  int arguments.
 */

#include "nmath.h"
#include <stdlib.h>

#ifdef MATHLIB_STANDALONE
#define ML_ERR_ret_NAN(_k_) {ML_ERROR(ME_DOMAIN, "rmultinom"); rN[_k_]=-1; return;}
#else
#define ML_ERR_ret_NAN(_k_) {ML_ERROR(ME_DOMAIN, "rmultinom"); rN[_k_]=NA_INTEGER; return;}
#endif

void rmultinom(int n, double* prob, int K, int* rN)
/* `Return' vector  rN[1:K] {K := length(prob)}
 *  where rN[j] ~ Bin(n, prob[j]) ,  sum_j rN[j] == n,  sum_j prob[j] == 1,
 */
{
    int k;
    double pp;
    long double p_tot = 0.;
    /* This calculation is sensitive to exact values, so we try to
       ensure that the calculations are as accurate as possible
       so different platforms are more likely to give the same
       result. */

#ifdef MATHLIB_STANDALONE
    if (K < 1) { ML_ERROR(ME_DOMAIN, "rmultinom"); return;}
    if (n < 0)  ML_ERR_ret_NAN(0);
#else
    if (K == NA_INTEGER || K < 1) { ML_ERROR(ME_DOMAIN, "rmultinom"); return;}
    if (n == NA_INTEGER || n < 0)  ML_ERR_ret_NAN(0);
#endif

    /* Note: prob[K] is only used here for checking  sum_k prob[k] = 1 ;
     *       Could make loop one shorter and drop that check !
     */
    for(k = 0; k < K; k++) {
	pp = prob[k];
	if (!R_FINITE(pp) || pp < 0. || pp > 1.) ML_ERR_ret_NAN(k);
	p_tot += pp;
	rN[k] = 0;
    }
    if(fabs(p_tot - 1.) > 1e-7)
	MATHLIB_ERROR(_("rbinom: probability sum should be 1, but is %g"), 
		      (double) p_tot);
    if (n == 0) return;
    if (K == 1 && p_tot == 0.) return;/* trivial border case: do as rbinom */

    /* Generate the first K-1 obs. via binomials */

    for(k = 0; k < K-1; k++) { /* (p_tot, n) are for "remaining binomial" */
	if(prob[k]) {
	    pp = prob[k] / p_tot;
	    /* printf("[%d] %.17f\n", k+1, pp); */
	    rN[k] = ((pp < 1.) ? (int) rbinom((double) n,  pp) :
		     /*>= 1; > 1 happens because of rounding */
		     n);
	    n -= rN[k];
	}
	else rN[k] = 0;
	if(n <= 0) /* we have all*/ return;
	p_tot -= prob[k]; /* i.e. = sum(prob[(k+1):K]) */
    }
    rN[K-1] = n;
    return;
}
#undef ML_ERR_ret_NAN
