/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
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
 *
 * DESCRIPTION
 *
 *    dpois() checks argument validity and calls dpois_raw().
 *
 *    dpois_raw() computes the Poisson probability  lb^x exp(-lb) / x!.
 *      This does not check that x is an integer, since dgamma() may
 *      call this with a fractional x argument. Any necessary argument
 *      checks should be done in the calling function.
 *
 */

#include "nmath.h"
#include "dpq.h"

double attribute_hidden dpois_raw(double x, double lambda, int give_log)
{
    /*       x >= 0 ; integer for dpois(), but not e.g. for pgamma()!
        lambda >= 0
    */
    if (lambda == 0) return( (x == 0) ? R_D__1 : R_D__0 );
    if (!R_FINITE(lambda)) return R_D__0;
    if (x < 0) return( R_D__0 );
    if (x <= lambda * DBL_MIN) return(R_D_exp(-lambda) );
    if (lambda < x * DBL_MIN) return(R_D_exp(-lambda + x*log(lambda) -lgammafn(x+1)));
    return(R_D_fexp( M_2PI*x, -stirlerr(x)-bd0(x,lambda) ));
}

double dpois(double x, double lambda, int give_log)
{
#ifdef IEEE_754
    if(ISNAN(x) || ISNAN(lambda))
        return x + lambda;
#endif

    if (lambda < 0) ML_ERR_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x))
	return R_D__0;

    x = R_D_forceint(x);

    return( dpois_raw(x,lambda,give_log) );
}
