/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2006 The R Development Core Team
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
 * #include <Rmath.h>
 *
 * double pbeta_raw(double x, double pin, double qin, int lower_tail, int log_p)
 * double pbeta	   (double x, double pin, double qin, int lower_tail, int log_p)
 *
 *  DESCRIPTION
 *
 *	Returns distribution function of the beta distribution.
 *	( = The incomplete beta ratio I_x(p,q) ).
 *
 *  NOTES
 *
 *      As from R 2.3.0, a wrapper for TOMS708
 *      as from R 2.6.0, 'log_p' partially improved over log(p..)
 */

#include "nmath.h"
#include "dpq.h"

attribute_hidden
double pbeta_raw(double x, double pin, double qin, int lower_tail, int log_p)
{
    double x1 = 0.5 - x + 0.5, w, wc;
    int ierr;
    bratio(pin, qin, x, x1, &w, &wc, &ierr, log_p); /* -> ./toms708.c */
    /* ierr = 8 is about inaccuracy in extreme cases */
    if(ierr && (ierr != 8 || log_p) )
	MATHLIB_WARNING(_("pbeta_raw() -> bratio() gave error code %d"), ierr);
    return lower_tail ? w : wc;
} /* pbeta_raw() */

double pbeta(double x, double pin, double qin, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(pin) || ISNAN(qin)) return x + pin + qin;
#endif

    if (pin <= 0 || qin <= 0) ML_ERR_return_NAN;

    if (x <= 0)
	return R_DT_0;
    if (x >= 1)
	return R_DT_1;
    return pbeta_raw(x, pin, qin, lower_tail, log_p);
}
