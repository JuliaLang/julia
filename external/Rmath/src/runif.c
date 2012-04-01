/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
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
 *    Random variates from the uniform distribution.
 */
#include "nmath.h"

double runif(double a, double b)
{
    if (!R_FINITE(a) || !R_FINITE(b) || b < a)	ML_ERR_return_NAN;

    if (a == b)
	return a;
    else {
	double u;
	/* This is true of all builtin generators, but protect against
	   user-supplied ones */
	do {u = unif_rand();} while (u <= 0 || u >= 1);
	return a + (b - a) * u;
    }
}
