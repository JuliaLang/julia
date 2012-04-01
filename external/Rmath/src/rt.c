/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2008 The R Development Core Team
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
 *    Pseudo-random variates from a t distribution.
 *
 *  NOTES
 *
 *    This function calls rchisq and rnorm to do the real work.
 */

#include "nmath.h"

double rt(double df)
{
    if (ISNAN(df) || df <= 0.0)	ML_ERR_return_NAN;

    if(!R_FINITE(df))
	return norm_rand();
    else {
/* Some compilers (including MW6) evaluated this from right to left
	return norm_rand() / sqrt(rchisq(df) / df); */
	double num = norm_rand();
	return num / sqrt(rchisq(df) / df);
    }
}
