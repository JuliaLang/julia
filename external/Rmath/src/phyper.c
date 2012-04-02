/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 1999-2000  The R Development Core Team
 *  Copyright (C) 2004	     Morten Welinder
 *  Copyright (C) 2004	     The R Foundation
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
 *	The distribution function of the hypergeometric distribution.
 *
 * Current implementation based on posting
 * From: Morten Welinder <terra@gnome.org>
 * Cc: R-bugs@biostat.ku.dk
 * Subject: [Rd] phyper accuracy and efficiency (PR#6772)
 * Date: Thu, 15 Apr 2004 18:06:37 +0200 (CEST)
 ......

 The current version has very serious cancellation issues.  For example,
 if you ask for a small right-tail you are likely to get total cancellation.
 For example,  phyper(59, 150, 150, 60, FALSE, FALSE) gives 6.372680161e-14.
 The right answer is dhyper(0, 150, 150, 60, FALSE) which is 5.111204798e-22.

 phyper is also really slow for large arguments.

 Therefore, I suggest using the code below. This is a sniplet from Gnumeric ...
 The code isn't perfect.  In fact, if  x*(NR+NB)  is close to	n*NR,
 then this code can take a while. Not longer than the old code, though.

 -- Thanks to Ian Smith for ideas.
*/

#include "nmath.h"
#include "dpq.h"

static double pdhyper (double x, double NR, double NB, double n, int log_p)
{
/*
 * Calculate
 *
 *	    phyper (x, NR, NB, n, TRUE, FALSE)
 *   [log]  ----------------------------------
 *	       dhyper (x, NR, NB, n, FALSE)
 *
 * without actually calling phyper.  This assumes that
 *
 *     x * (NR + NB) <= n * NR
 *
 */
    long double sum = 0;
    long double term = 1;

    while (x > 0 && term >= DBL_EPSILON * sum) {
	term *= x * (NB - n + x) / (n + 1 - x) / (NR + 1 - x);
	sum += term;
	x--;
    }

    return log_p ? log1p(sum) : 1 + sum;
}


/* FIXME: The old phyper() code was basically used in ./qhyper.c as well
 * -----  We need to sync this again!
*/
double phyper (double x, double NR, double NB, double n,
	       int lower_tail, int log_p)
{
/* Sample of  n balls from  NR red  and	 NB black ones;	 x are red */

    double d, pd;

#ifdef IEEE_754
    if(ISNAN(x) || ISNAN(NR) || ISNAN(NB) || ISNAN(n))
	return x + NR + NB + n;
#endif

    x = floor (x + 1e-7);
    NR = R_D_forceint(NR);
    NB = R_D_forceint(NB);
    n  = R_D_forceint(n);

    if (NR < 0 || NB < 0 || !R_FINITE(NR + NB) || n < 0 || n > NR + NB)
	ML_ERR_return_NAN;

    if (x * (NR + NB) > n * NR) {
	/* Swap tails.	*/
	double oldNB = NB;
	NB = NR;
	NR = oldNB;
	x = n - x - 1;
	lower_tail = !lower_tail;
    }

    if (x < 0)
	return R_DT_0;
    if (x >= NR || x >= n)
	return R_DT_1;

    d  = dhyper (x, NR, NB, n, log_p);
    pd = pdhyper(x, NR, NB, n, log_p);

    return log_p ? R_DT_Log(d + pd) : R_D_Lval(d * pd);
}
