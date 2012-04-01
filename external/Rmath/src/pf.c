/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-8 The R Development Core Team
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
 *  DESCRIPTION
 *
 *    The distribution function of the F distribution.
 */

#include "nmath.h"
#include "dpq.h"

double pf(double x, double df1, double df2, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df1) || ISNAN(df2))
	return x + df2 + df1;
#endif
    if (df1 <= 0. || df2 <= 0.) ML_ERR_return_NAN;

    R_P_bounds_01(x, 0., ML_POSINF);

    /* move to pchisq for very large values - was 'df1 > 4e5' in 2.0.x,
       now only needed for df1 = Inf or df2 = Inf {since pbeta(0,*)=0} : */
    if (df2 == ML_POSINF) {
	if (df1 == ML_POSINF) {
	    if(x <  1.) return R_DT_0;
	    if(x == 1.) return (log_p ? -M_LN2 : 0.5);
	    if(x >  1.) return R_DT_1;
	}

	return pchisq(x * df1, df1, lower_tail, log_p);
    }

    if (df1 == ML_POSINF)/* was "fudge"	'df1 > 4e5' in 2.0.x */
	return pchisq(df2 / x , df2, !lower_tail, log_p);

    /* Avoid squeezing pbeta's first parameter against 1 :  */
    if (df1 * x > df2)
	x = pbeta(df2 / (df2 + df1 * x), df2 / 2., df1 / 2., 
		  !lower_tail, log_p);
    else
	x = pbeta(df1 * x / (df2 + df1 * x), df1 / 2., df2 / 2.,
		  lower_tail, log_p);

    return ML_VALID(x) ? x : ML_NAN;
}
