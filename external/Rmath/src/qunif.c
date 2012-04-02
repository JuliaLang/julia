/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2006 The R Development Core Team
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
 *    The quantile function of the uniform distribution.
 */

#include "nmath.h"
#include "dpq.h"

double qunif(double p, double a, double b, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(a) || ISNAN(b))
	return p + a + b;
#endif
    R_Q_P01_check(p);
    if (!R_FINITE(a) || !R_FINITE(b)) ML_ERR_return_NAN;
    if (b < a) ML_ERR_return_NAN;
    if (b == a) return a;

    return a + R_DT_qIv(p) * (b - a);
}




