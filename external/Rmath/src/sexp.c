/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2002 the R Development Core Team
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
 *    double exp_rand(void);
 *
 *  DESCRIPTION
 *
 *    Random variates from the standard exponential distribution.
 *
 *  REFERENCE
 *
 *    Ahrens, J.H. and Dieter, U. (1972).
 *    Computer methods for sampling from the exponential and
 *    normal distributions.
 *    Comm. ACM, 15, 873-882.
 */

#include "nmath.h"

double exp_rand(void)
{
    /* q[k-1] = sum(log(2)^k / k!)  k=1,..,n, */
    /* The highest n (here 16) is determined by q[n-1] = 1.0 */
    /* within standard precision */
    const static double q[] =
    {
	0.6931471805599453,
	0.9333736875190459,
	0.9888777961838675,
	0.9984959252914960,
	0.9998292811061389,
	0.9999833164100727,
	0.9999985691438767,
	0.9999998906925558,
	0.9999999924734159,
	0.9999999995283275,
	0.9999999999728814,
	0.9999999999985598,
	0.9999999999999289,
	0.9999999999999968,
	0.9999999999999999,
	1.0000000000000000
    };

    double a = 0.;
    double u = unif_rand();    /* precaution if u = 0 is ever returned */
    while(u <= 0. || u >= 1.) u = unif_rand();
    for (;;) {
	u += u;
	if (u > 1.)
	    break;
	a += q[0];
    }
    u -= 1.;

    if (u <= q[0])
	return a + u;

    int i = 0;
    double ustar = unif_rand(), umin = ustar;
    do {
	ustar = unif_rand();
	if (umin > ustar)
	    umin = ustar;
	i++;
    } while (u > q[i]);
    return a + umin * q[0];
}
