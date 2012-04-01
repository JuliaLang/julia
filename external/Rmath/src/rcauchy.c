/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000--2008 The R Development Core Team
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
 *    #include <Rmath.h>
 *    double rcauchy(double location, double scale);
 *
 *  DESCRIPTION
 *
 *    Random variates from the Cauchy distribution.
 */

#include "nmath.h"

double rcauchy(double location, double scale)
{
    if (ISNAN(location) || !R_FINITE(scale) || scale < 0)
	ML_ERR_return_NAN;
    if (scale == 0. || !R_FINITE(location))
	return location;
    else
	return location + scale * tan(M_PI * unif_rand());
}
