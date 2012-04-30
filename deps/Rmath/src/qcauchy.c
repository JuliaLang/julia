/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998   Ross Ihaka
 *  Copyright (C) 2000   The R Development Core Team
 *  Copyright (C) 2005-6 The R Foundation
 *
 *  This version is based on a suggestion by Morten Welinder.
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
 *  http://www.gnu.org/copyleft/gpl.html.
 *
 *  DESCRIPTION
 *
 *	The quantile function of the Cauchy distribution.
 */

#include "nmath.h"
#include "dpq.h"

double qcauchy(double p, double location, double scale,
	       int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(location) || ISNAN(scale))
	return p + location + scale;
#endif
    R_Q_P01_check(p);
    if (scale <= 0 || !R_FINITE(scale)) {
	if (scale == 0) return location;
	/* else */ ML_ERR_return_NAN;
    }

#define my_INF location + (lower_tail ? scale : -scale) * ML_POSINF
    if (log_p) {
	if (p > -1) {
	    /* when ep := exp(p),
	     * tan(pi*ep)= -tan(pi*(-ep))= -tan(pi*(-ep)+pi) = -tan(pi*(1-ep)) =
	     *		 = -tan(pi*(-expm1(p))
	     * for p ~ 0, exp(p) ~ 1, tan(~0) may be better than tan(~pi).
	     */
	    if (p == 0.) /* needed, since 1/tan(-0) = -Inf  for some arch. */
		return my_INF;
	    lower_tail = !lower_tail;
	    p = -expm1(p);
	} else
	    p = exp(p);
    } else if (p == 1.)
	return my_INF;

    return location + (lower_tail ? -scale : scale) / tan(M_PI * p);
    /*	-1/tan(pi * p) = -cot(pi * p) = tan(pi * (p - 1/2))  */
}
