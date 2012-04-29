/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2003 The R Foundation
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
 *    double rnchisq(double df, double lambda);
 *
 *  DESCRIPTION
 *
 *    Random variates from the NON CENTRAL chi-squared distribution.
 *
 *  NOTES
 *
 According to Hans R. Kuensch's suggestion (30 sep 2002):

  It should be easy to do the general case (ncp > 0) by decomposing it
  as the sum of a central chisquare with df degrees of freedom plus a
  noncentral chisquare with zero degrees of freedom (which is a Poisson
  mixture of central chisquares with integer degrees of freedom),
  see Formula (29.5b-c) in Johnson, Kotz, Balakrishnan (1995).

  The noncentral chisquare with arbitary degrees of freedom is of interest
  for simulating the Cox-Ingersoll-Ross model for interest rates in
  finance.

  R code that works is

    rchisq0 <- function(n, ncp) {
	p <- 0 < (K <- rpois(n, lambda = ncp / 2))
	r <- numeric(n)
	r[p] <- rchisq(sum(p), df = 2*K[p])
	r
    }

    rchisq <- function(n, df, ncp=0) {
	if(missing(ncp)) .Internal(rchisq(n, df))
	else rchisq0(n, ncp) + .Internal(rchisq(n, df))
    }
 */

#include "nmath.h"

double rnchisq(double df, double lambda)
{
    if (!R_FINITE(df) || !R_FINITE(lambda) || df < 0. || lambda < 0.)
	ML_ERR_return_NAN;

    if(lambda == 0.) {
	if (df == 0.) ML_ERR_return_NAN;
	return rgamma(df / 2., 2.);
    }
    else {
	double r = rpois( lambda / 2.);
	if (r > 0.)  r = rchisq(2. * r);
	if (df > 0.) r += rgamma(df / 2., 2.);
	return r;
    }
}
