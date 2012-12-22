/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 1999-2000  The R Development Core Team
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
 *    void gammalims(double *xmin, double *xmax);
 *
 *  DESCRIPTION
 *
 *    This function calculates the minimum and maximum legal bounds
 *    for x in gammafn(x).  These are not the only bounds, but they
 *    are the only non-trivial ones to calculate.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 */

#include "nmath.h"

void attribute_hidden gammalims(double *xmin, double *xmax)
{
/* FIXME: Even better: If IEEE, #define these in nmath.h
	  and don't call gammalims() at all
*/
#ifdef IEEE_754
    *xmin = -170.5674972726612;
    *xmax =  171.61447887182298;/*(3 Intel/Sparc architectures)*/
#else
    double alnbig, alnsml, xln, xold;
    int i;

    alnsml = log(d1mach(1));
    *xmin = -alnsml;
    for (i=1; i<=10; ++i) {
	xold = *xmin;
	xln = log(*xmin);
	*xmin -= *xmin * ((*xmin + .5) * xln - *xmin - .2258 + alnsml) /
		(*xmin * xln + .5);
	if (fabs(*xmin - xold) < .005) {
	    *xmin = -(*xmin) + .01;
	    goto find_xmax;
	}
    }

    /* unable to find xmin */

    ML_ERROR(ME_NOCONV, "gammalims");
    *xmin = *xmax = ML_NAN;

find_xmax:

    alnbig = log(d1mach(2));
    *xmax = alnbig;
    for (i=1; i<=10; ++i) {
	xold = *xmax;
	xln = log(*xmax);
	*xmax -= *xmax * ((*xmax - .5) * xln - *xmax + .9189 - alnbig) /
		(*xmax * xln - .5);
	if (fabs(*xmax - xold) < .005) {
	    *xmax += -.01;
	    goto done;
	}
    }

    /* unable to find xmax */

    ML_ERROR(ME_NOCONV, "gammalims");
    *xmin = *xmax = ML_NAN;

done:
    *xmin = fmax2(*xmin, -(*xmax) + 1);
#endif
}

