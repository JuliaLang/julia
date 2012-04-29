/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998	Ross Ihaka
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
 *	The distribution function of the non-central F distribution.
 */

#include "nmath.h"
#include "dpq.h"

double pnf(double x, double df1, double df2, double ncp,
	   int lower_tail, int log_p)
{
    double y;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df1) || ISNAN(df2) || ISNAN(ncp))
	return x + df2 + df1 + ncp;
#endif
    if (df1 <= 0. || df2 <= 0. || ncp < 0) ML_ERR_return_NAN;
    if (!R_FINITE(ncp)) ML_ERR_return_NAN;
    if (!R_FINITE(df1) && !R_FINITE(df2)) /* both +Inf */
	ML_ERR_return_NAN;

    R_P_bounds_01(x, 0., ML_POSINF);

    if (df2 > 1e8) /* avoid problems with +Inf and loss of accuracy */
	return pnchisq(x * df1, df1, ncp, lower_tail, log_p);

    y = (df1 / df2) * x;
    return pnbeta2(y/(1. + y), 1./(1. + y), df1 / 2., df2 / 2.,
		   ncp, lower_tail, log_p);
}
