/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000--2001  The R Development Core Team
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
 *    double rlnorm(double logmean, double logsd);
 *
 *  DESCRIPTION
 *
 *    Random variates from the lognormal distribution.
 */

#include "nmath.h"

double rlnorm(double meanlog, double sdlog)
{
    if(ISNAN(meanlog) || !R_FINITE(sdlog) || sdlog < 0.)
	ML_ERR_return_NAN;

    return exp(rnorm(meanlog, sdlog));
}
