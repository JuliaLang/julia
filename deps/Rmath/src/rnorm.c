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
 *  SYNOPSIS
 *
 *    #include "Rnorm.h"
 *    double rnorm(double mu, double sigma);
 *
 *  DESCRIPTION
 *
 *    Random variates from the normal distribution.
 *
 */

#include "nmath.h"

double rnorm(double mu, double sigma)
{
    if (ISNAN(mu) || !R_FINITE(sigma) || sigma < 0.)
	ML_ERR_return_NAN;
    if (sigma == 0. || !R_FINITE(mu))
	return mu; /* includes mu = +/- Inf with finite sigma */
    else
	return mu + sigma * norm_rand();
}
