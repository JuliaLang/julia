/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, 2001 The R Core Development Team
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
 *    Given a sequence of r successes and b failures, we sample n (\le b+r)
 *    items without replacement. The hypergeometric probability is the
 *    probability of x successes:
 *
 *		       choose(r, x) * choose(b, n-x)
 *	p(x; r,b,n) =  -----------------------------  =
 *			       choose(r+b, n)
 *
 *		      dbinom(x,r,p) * dbinom(n-x,b,p)
 *		    = --------------------------------
 *			       dbinom(n,r+b,p)
 *
 *    for any p. For numerical stability, we take p=n/(r+b); with this choice,
 *    the denominator is not exponentially small.
 */

#include "nmath.h"
#include "dpq.h"

double dhyper(double x, double r, double b, double n, int give_log)
{
    double p, q, p1, p2, p3;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(r) || ISNAN(b) || ISNAN(n))
	return x + r + b + n;
#endif

    if (R_D_negInonint(r) || R_D_negInonint(b) || R_D_negInonint(n) || n > r+b)
	ML_ERR_return_NAN;
    if (R_D_negInonint(x))
	return(R_D__0);

    x = R_D_forceint(x);
    r = R_D_forceint(r);
    b = R_D_forceint(b);
    n = R_D_forceint(n);

    if (n < x || r < x || n - x > b) return(R_D__0);
    if (n == 0) return((x == 0) ? R_D__1 : R_D__0);

    p = ((double)n)/((double)(r+b));
    q = ((double)(r+b-n))/((double)(r+b));

    p1 = dbinom_raw(x,	r, p,q,give_log);
    p2 = dbinom_raw(n-x,b, p,q,give_log);
    p3 = dbinom_raw(n,r+b, p,q,give_log);

    return( (give_log) ? p1 + p2 - p3 : p1*p2/p3 );
}
