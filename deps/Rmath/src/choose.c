/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998      Ross Ihaka
 *  Copyright (C) 2004-2009 The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
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
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double choose(double n, double k);
 *    double lchoose(double n, double k);
 * (and private)
 *    double lfastchoose(double n, double k);
 *
 *  DESCRIPTION
 *
 *	Binomial coefficients.
 *	choose(n, k)   and  lchoose(n,k) := log(abs(choose(n,k))
 *
 *	These work for the *generalized* binomial theorem,
 *	i.e., are also defined for non-integer n  (integer k).
 *
 *  We use the simple explicit product formula for  k <= k_small_max
 *  and also have added statements to make sure that the symmetry
 *    (n \\ k ) == (n \\ n-k)  is preserved for non-negative integer n.
 */

#include "nmath.h"

double attribute_hidden lfastchoose(double n, double k)
{
    return -log(n + 1.) - lbeta(n - k + 1., k + 1.);
}
/* mathematically the same:
   less stable typically, but useful if n-k+1 < 0 : */
static
double lfastchoose2(double n, double k, int *s_choose)
{
    double r;
    r = lgammafn_sign(n - k + 1., s_choose);
    return lgammafn(n + 1.) - lgammafn(k + 1.) - r;
}

#define ODD(_K_) ((_K_) != 2 * floor((_K_) / 2.))
/* matching R_D_nonint() in ./dpq.h : */
#define R_IS_INT(x)	  (fabs((x) - floor((x)+0.5)) <= 1e-7)

double lchoose(double n, double k)
{
    double k0 = k;
    k = floor(k + 0.5);
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if(ISNAN(n) || ISNAN(k)) return n + k;
#endif
    if (fabs(k - k0) > 1e-7)
	MATHLIB_WARNING2(_("'k' (%.2f) must be integer, rounded to %.0f"), k0, k);
    if (k < 2) {
	if (k <	 0) return ML_NEGINF;
	if (k == 0) return 0.;
	/* else: k == 1 */
	return log(fabs(n));
    }
    /* else: k >= 2 */
    if (n < 0) {
	return lchoose(-n+ k-1, k);
    }
    else if (R_IS_INT(n)) {
	if(n < k) return ML_NEGINF;
	/* k <= n :*/
	if(n - k < 2) return lchoose(n, n-k); /* <- Symmetry */
	/* else: n >= k+2 */
	return lfastchoose(n, k);
    }
    /* else non-integer n >= 0 : */
    if (n < k-1) {
	int s;
	return lfastchoose2(n, k, &s);
    }
    return lfastchoose(n, k);
}

#define k_small_max 30
/* 30 is somewhat arbitrary: it is on the *safe* side:
 * both speed and precision are clearly improved for k < 30.
*/
double choose(double n, double k)
{
    double r, k0 = k;
    k = floor(k + 0.5);
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if(ISNAN(n) || ISNAN(k)) return n + k;
#endif
    if (fabs(k - k0) > 1e-7)
	MATHLIB_WARNING2(_("'k' (%.2f) must be integer, rounded to %.0f"), k0, k);
    if (k < k_small_max) {
	int j;
	if(n-k < k && n >= 0 && R_IS_INT(n)) k = n-k; /* <- Symmetry */
	if (k <	 0) return 0.;
	if (k == 0) return 1.;
	/* else: k >= 1 */
	r = n;
	for(j = 2; j <= k; j++)
	    r *= (n-j+1)/j;
	return R_IS_INT(n) ? floor(r + 0.5) : r;
	/* might have got rounding errors */
    }
    /* else: k >= k_small_max */
    if (n < 0) {
	r = choose(-n+ k-1, k);
	if (ODD(k)) r = -r;
	return r;
    }
    else if (R_IS_INT(n)) {
	if(n < k) return 0.;
	if(n - k < k_small_max) return choose(n, n-k); /* <- Symmetry */
	return floor(exp(lfastchoose(n, k)) + 0.5);
    }
    /* else non-integer n >= 0 : */
    if (n < k-1) {
	int s_choose;
	r = lfastchoose2(n, k, /* -> */ &s_choose);
	return s_choose * exp(r);
    }
    return exp(lfastchoose(n, k));
}

#undef ODD
#undef R_IS_INT
#undef k_small_max
