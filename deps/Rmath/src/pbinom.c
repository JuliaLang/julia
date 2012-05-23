/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-6     The R Development Core Team
 *  Copyright (C) 2004       The R Foundation
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
 *    The distribution function of the binomial distribution.
 */
#include "nmath.h"
#include "dpq.h"

double pbinom(double x, double n, double p, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n) || ISNAN(p))
	return x + n + p;
    if (!R_FINITE(n) || !R_FINITE(p)) ML_ERR_return_NAN;

#endif
    if(R_D_nonint(n)) ML_ERR_return_NAN;
    n = R_D_forceint(n);
    /* PR#8560: n=0 is a valid value */
    if(n < 0 || p < 0 || p > 1) ML_ERR_return_NAN;

    if (x < 0) return R_DT_0;
    x = floor(x + 1e-7);
    if (n <= x) return R_DT_1;
    return pbeta(p, x + 1, n - x, !lower_tail, log_p);
}
