/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000	    The R Development Core Team
 *  Copyright (C) 2003	    The R Foundation
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
 *	double dnorm4(double x, double mu, double sigma, int give_log)
 *	      {dnorm (..) is synonymous and preferred inside R}
 *
 *  DESCRIPTION
 *
 *	Compute the density of the normal distribution.
 */

#include "nmath.h"
#include "dpq.h"

double dnorm4(double x, double mu, double sigma, int give_log)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma))
	return x + mu + sigma;
#endif
    if(!R_FINITE(sigma)) return R_D__0;
    if(!R_FINITE(x) && mu == x) return ML_NAN;/* x-mu is NaN */
    if (sigma <= 0) {
	if (sigma < 0) ML_ERR_return_NAN;
	/* sigma == 0 */
	return (x == mu) ? ML_POSINF : R_D__0;
    }
    x = (x - mu) / sigma;

    if(!R_FINITE(x)) return R_D__0;
    return (give_log ?
	    -(M_LN_SQRT_2PI  +	0.5 * x * x + log(sigma)) :
	    M_1_SQRT_2PI * exp(-0.5 * x * x)  /	  sigma);
    /* M_1_SQRT_2PI = 1 / sqrt(2 * pi) */
}
