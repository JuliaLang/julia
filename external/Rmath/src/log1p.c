/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000, 2003, 2011 The R Development Core Team
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
 *	#include <Rmath.h>
 *	double log1p(double x);
 *
 *  DESCRIPTION
 *
 *	Compute the relative error logarithm.
 *
 *			log(1 + x)
 *
 *  NOTES
 *
 *	This code is a translation of the Fortran subroutine `dlnrel'
 *	written by W. Fullerton of Los Alamos Scientific Laboratory.
 */

/* Every currently known platform has log1p (which is C99), 
   but NetBSD/OpenBSD were at least at one time inaccurate */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include "nmath.h"

/* want to compile log1p as Rlog1p if HAVE_LOG1P && !HAVE_WORKING_LOG1P */
#if defined(HAVE_LOG1P) && !defined(HAVE_WORKING_LOG1P)
#undef HAVE_LOG1P
#endif

#ifndef HAVE_LOG1P
double log1p(double x)
{
    /* series for log1p on the interval -.375 to .375
     *				     with weighted error   6.35e-32
     *				      log weighted error  31.20
     *			    significant figures required  30.93
     *				 decimal places required  32.01
     */
    const static double alnrcs[43] = {
	+.10378693562743769800686267719098e+1,
	-.13364301504908918098766041553133e+0,
	+.19408249135520563357926199374750e-1,
	-.30107551127535777690376537776592e-2,
	+.48694614797154850090456366509137e-3,
	-.81054881893175356066809943008622e-4,
	+.13778847799559524782938251496059e-4,
	-.23802210894358970251369992914935e-5,
	+.41640416213865183476391859901989e-6,
	-.73595828378075994984266837031998e-7,
	+.13117611876241674949152294345011e-7,
	-.23546709317742425136696092330175e-8,
	+.42522773276034997775638052962567e-9,
	-.77190894134840796826108107493300e-10,
	+.14075746481359069909215356472191e-10,
	-.25769072058024680627537078627584e-11,
	+.47342406666294421849154395005938e-12,
	-.87249012674742641745301263292675e-13,
	+.16124614902740551465739833119115e-13,
	-.29875652015665773006710792416815e-14,
	+.55480701209082887983041321697279e-15,
	-.10324619158271569595141333961932e-15,
	+.19250239203049851177878503244868e-16,
	-.35955073465265150011189707844266e-17,
	+.67264542537876857892194574226773e-18,
	-.12602624168735219252082425637546e-18,
	+.23644884408606210044916158955519e-19,
	-.44419377050807936898878389179733e-20,
	+.83546594464034259016241293994666e-21,
	-.15731559416479562574899253521066e-21,
	+.29653128740247422686154369706666e-22,
	-.55949583481815947292156013226666e-23,
	+.10566354268835681048187284138666e-23,
	-.19972483680670204548314999466666e-24,
	+.37782977818839361421049855999999e-25,
	-.71531586889081740345038165333333e-26,
	+.13552488463674213646502024533333e-26,
	-.25694673048487567430079829333333e-27,
	+.48747756066216949076459519999999e-28,
	-.92542112530849715321132373333333e-29,
	+.17578597841760239233269760000000e-29,
	-.33410026677731010351377066666666e-30,
	+.63533936180236187354180266666666e-31,
    };

#ifdef NOMORE_FOR_THREADS
    static int nlnrel = 0;
    static double xmin = 0.0;

    if (xmin == 0.0) xmin = -1 + sqrt(DBL_EPSILON);/*was sqrt(d1mach(4)); */
    if (nlnrel == 0) /* initialize chebychev coefficients */
	nlnrel = chebyshev_init(alnrcs, 43, DBL_EPSILON/20);/*was .1*d1mach(3)*/
#else
# define nlnrel 22
    const static double xmin = -0.999999985;
/* 22: for IEEE double precision where DBL_EPSILON =  2.22044604925031e-16 */
#endif

    if (x == 0.) return 0.;/* speed */
    if (x == -1) return(ML_NEGINF);
    if (x  < -1) ML_ERR_return_NAN;

    if (fabs(x) <= .375) {
        /* Improve on speed (only);
	   again give result accurate to IEEE double precision: */
	if(fabs(x) < .5 * DBL_EPSILON)
	    return x;

	if( (0 < x && x < 1e-8) || (-1e-9 < x && x < 0))
	    return x * (1 - .5 * x);
	/* else */
	return x * (1 - x * chebyshev_eval(x / .375, alnrcs, nlnrel));
    }
    /* else */
    if (x < xmin) {
	/* answer less than half precision because x too near -1 */
	ML_ERROR(ME_PRECISION, "log1p");
    }
    return log(1 + x);
}
#endif



#ifndef HAVE_HYPOT
/* Used as a substitute for the C99 function hypot, which all currently
   known platforms have */

/* hypot(a,b)	finds sqrt(a^2 + b^2)
 *		without overflow or destructive underflow.
 */

double hypot(double a, double b)
{
    double p, r, s, t, tmp, u;

    if(ISNAN(a) || ISNAN(b)) /* propagate Na(N)s: */
        return
#ifdef IEEE_754
	  a + b;
#else
          ML_NAN;
#endif
    if (!R_FINITE(a) || !R_FINITE(b)) {
        return ML_POSINF;
    }
    p = fmax2(fabs(a), fabs(b));
    if (p != 0.0) {

	/* r = (min(|a|,|b|) / p) ^2 */
	tmp = fmin2(fabs(a), fabs(b))/p;
	r = tmp * tmp;
	for(;;) {
	    t = 4.0 + r;
	    /* This was a test of 4.0 + r == 4.0, but optimizing
		compilers nowadays infinite loop on that. */
	    if(fabs(r) < 2*DBL_EPSILON) break;
	    s = r / t;
	    u = 1. + 2. * s;
	    p *= u ;

	    /* r = (s / u)^2 * r */
	    tmp = s / u;
	    r *= tmp * tmp;
	}
    }
    return p;
}
#endif
