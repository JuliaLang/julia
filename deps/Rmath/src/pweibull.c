/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2002 The R Development Core Team
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
 *    The distribution function of the Weibull distribution.
 */

#include "nmath.h"
#include "dpq.h"

double pweibull(double x, double shape, double scale, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(shape) || ISNAN(scale))
	return x + shape + scale;
#endif
    if(shape <= 0 || scale <= 0) ML_ERR_return_NAN;

    if (x <= 0)
	return R_DT_0;
    x = -pow(x / scale, shape);
    if (lower_tail)
	return (log_p
		/* log(1 - exp(x))  for x < 0 : */
		? R_Log1_Exp(x) : -expm1(x));
    /* else:  !lower_tail */
    return R_D_exp(x);
}
