/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000-2007   The R Development Core Team
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
 */

#include "nmath.h"
#include "dpq.h"

double pt(double x, double n, int lower_tail, int log_p)
{
/* return  P[ T <= x ]	where
 * T ~ t_{n}  (t distrib. with n degrees of freedom).

 *	--> ./pnt.c for NON-central
 */
    double val, nx;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n))
	return x + n;
#endif
    if (n <= 0.0) ML_ERR_return_NAN;

    if(!R_FINITE(x))
	return (x < 0) ? R_DT_0 : R_DT_1;
    if(!R_FINITE(n))
	return pnorm(x, 0.0, 1.0, lower_tail, log_p);

#ifdef R_version_le_260
    if (n > 4e5) { /*-- Fixme(?): test should depend on `n' AND `x' ! */
	/* Approx. from	 Abramowitz & Stegun 26.7.8 (p.949) */
	val = 1./(4.*n);
	return pnorm(x*(1. - val)/sqrt(1. + x*x*2.*val), 0.0, 1.0,
		     lower_tail, log_p);
    }
#endif

    nx = 1 + (x/n)*x;
    /* FIXME: This test is probably losing rather than gaining precision,
     * now that pbeta(*, log_p = TRUE) is much better.
     * Note however that a version of this test *is* needed for x*x > D_MAX */
    if(nx > 1e100) { /* <==>  x*x > 1e100 * n  */
	/* Danger of underflow. So use Abramowitz & Stegun 26.5.4
	   pbeta(z, a, b) ~ z^a(1-z)^b / aB(a,b) ~ z^a / aB(a,b),
	   with z = 1/nx,  a = n/2,  b= 1/2 :
	*/
	double lval;
	lval = -0.5*n*(2*log(fabs(x)) - log(n))
	        - lbeta(0.5*n, 0.5) - log(0.5*n);
	val = log_p ? lval : exp(lval);
    } else {
	val = (n > x * x)
	    ? pbeta (x * x / (n + x * x), 0.5, n / 2., /*lower_tail*/0, log_p)
	    : pbeta (1. / nx,             n / 2., 0.5, /*lower_tail*/1, log_p);
    }

    /* Use "1 - v"  if	lower_tail  and	 x > 0 (but not both):*/
    if(x <= 0.)
	lower_tail = !lower_tail;

    if(log_p) {
	if(lower_tail) return log1p(-0.5*exp(val));
	else return val - M_LN2; /* = log(.5* pbeta(....)) */
    }
    else {
	val /= 2.;
	return R_D_Cval(val);
    }
}
