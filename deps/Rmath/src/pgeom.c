/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2006 The R Development Core Team
 *  Copyright (C) 2004	    The R Foundation
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
 *    The distribution function of the geometric distribution.
 */

#include "nmath.h"
#include "dpq.h"

double pgeom(double x, double p, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(p))
	return x + p;
#endif
    if(p <= 0 || p > 1) ML_ERR_return_NAN;

    if (x < 0.) return R_DT_0;
    if (!R_FINITE(x)) return R_DT_1;
    x = floor(x +1e-7);

    if(p == 1.) { /* we cannot assume IEEE */
	x = lower_tail ? 1: 0;
	return log_p ? log(x) : x;
    }
    x = log1p(-p) * (x + 1);
    if (log_p)
	return R_DT_Clog(x);
    else
	return lower_tail ? -expm1(x) : exp(x);
}
