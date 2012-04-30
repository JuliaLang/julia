/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2008 The R Development Core Team
 *  Copyright (C) 2005 The R Foundation
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
 *	double qnbinom(double p, double size, double prob,
 *                     int lower_tail, int log_p)
 *
 *  DESCRIPTION
 *
 *	The quantile function of the negative binomial distribution.
 *
 *  NOTES
 *
 *	x = the number of failures before the n-th success
 *
 *  METHOD
 *
 *	Uses the Cornish-Fisher Expansion to include a skewness
 *	correction to a normal approximation.  This gives an
 *	initial value which never seems to be off by more than
 *	1 or 2.	 A search is then conducted of values close to
 *	this initial start point.
 */

#include "nmath.h"
#include "dpq.h"

static double
do_search(double y, double *z, double p, double n, double pr, double incr)
{
    if(*z >= p) {
			/* search to the left */
	for(;;) {
	    if(y == 0 ||
	       (*z = pnbinom(y - incr, n, pr, /*l._t.*/TRUE, /*log_p*/FALSE)) < p)
		return y;
	    y = fmax2(0, y - incr);
	}
    }
    else {		/* search to the right */

	for(;;) {
	    y = y + incr;
	    if((*z = pnbinom(y, n, pr, /*l._t.*/TRUE, /*log_p*/FALSE)) >= p)
		return y;
	}
    }
}


double qnbinom(double p, double size, double prob, int lower_tail, int log_p)
{
    double P, Q, mu, sigma, gamma, z, y;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(size) || ISNAN(prob))
	return p + size + prob;
#endif
    if (prob <= 0 || prob > 1 || size <= 0) ML_ERR_return_NAN;
    /* FIXME: size = 0 is well defined ! */
    if (prob == 1) return 0;

    R_Q_P01_boundaries(p, 0, ML_POSINF);

    Q = 1.0 / prob;
    P = (1.0 - prob) * Q;
    mu = size * P;
    sigma = sqrt(size * P * Q);
    gamma = (Q + P)/sigma;

    /* Note : "same" code in qpois.c, qbinom.c, qnbinom.c --
     * FIXME: This is far from optimal [cancellation for p ~= 1, etc]: */
    if(!lower_tail || log_p) {
	p = R_DT_qIv(p); /* need check again (cancellation!): */
	if (p == R_DT_0) return 0;
	if (p == R_DT_1) return ML_POSINF;
    }
    /* temporary hack --- FIXME --- */
    if (p + 1.01*DBL_EPSILON >= 1.) return ML_POSINF;

    /* y := approx.value (Cornish-Fisher expansion) :  */
    z = qnorm(p, 0., 1., /*lower_tail*/TRUE, /*log_p*/FALSE);
    y = floor(mu + sigma * (z + gamma * (z*z - 1) / 6) + 0.5);

    z = pnbinom(y, size, prob, /*lower_tail*/TRUE, /*log_p*/FALSE);

    /* fuzz to ensure left continuity: */
    p *= 1 - 64*DBL_EPSILON;

    /* If the C-F value is not too large a simple search is OK */
    if(y < 1e5) return do_search(y, &z, p, size, prob, 1);
    /* Otherwise be a bit cleverer in the search */
    {
	double incr = floor(y * 0.001), oldincr;
	do {
	    oldincr = incr;
	    y = do_search(y, &z, p, size, prob, incr);
	    incr = fmax2(1, floor(incr/100));
	} while(oldincr > 1 && incr > y*1e-15);
	return y;
    }
}

double qnbinom_mu(double p, double size, double mu, int lower_tail, int log_p)
{
/* FIXME!  Implement properly!! (not losing accuracy for very large size (prob ~= 1)*/
    return qnbinom(p, size, /* prob = */ size/(size+mu), lower_tail, log_p);
}
