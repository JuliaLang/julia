/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2002 The R Development Core Team
 *  Copyright (C) 2007 The R Foundation
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
 *	double rbinom(double nin, double pp)
 *
 *  DESCRIPTION
 *
 *	Random variates from the binomial distribution.
 *
 *  REFERENCE
 *
 *	Kachitvichyanukul, V. and Schmeiser, B. W. (1988).
 *	Binomial random variate generation.
 *	Communications of the ACM 31, 216-222.
 *	(Algorithm BTPEC).
 */

#include "nmath.h"
#include <stdlib.h>
#include <limits.h>

#define repeat for(;;)

double rbinom(double nin, double pp)
{
    /* FIXME: These should become THREAD_specific globals : */

    static double c, fm, npq, p1, p2, p3, p4, qn;
    static double xl, xll, xlr, xm, xr;

    static double psave = -1.0;
    static int nsave = -1;
    static int m;

    double f, f1, f2, u, v, w, w2, x, x1, x2, z, z2;
    double p, q, np, g, r, al, alv, amaxp, ffm, ynorm;
    int i,ix,k, n;

    if (!R_FINITE(nin)) ML_ERR_return_NAN;
    r = floor(nin + 0.5);
    if (r != nin) ML_ERR_return_NAN;
    if (!R_FINITE(pp) ||
	/* n=0, p=0, p=1 are not errors <TSL>*/
	r < 0 || pp < 0. || pp > 1.)	ML_ERR_return_NAN;

    if (r == 0 || pp == 0.) return 0;
    if (pp == 1.) return r;

    if (r >= INT_MAX)/* evade integer overflow,
			and r == INT_MAX gave only even values */
	return qbinom(unif_rand(), r, pp, /*lower_tail*/ 0, /*log_p*/ 0);
    /* else */
    n = r;

    p = fmin2(pp, 1. - pp);
    q = 1. - p;
    np = n * p;
    r = p / q;
    g = r * (n + 1);

    /* Setup, perform only when parameters change [using static (globals): */

    /* FIXING: Want this thread safe
       -- use as little (thread globals) as possible
    */
    if (pp != psave || n != nsave) {
	psave = pp;
	nsave = n;
	if (np < 30.0) {
	    /* inverse cdf logic for mean less than 30 */
	    qn = pow(q, (double) n);
	    goto L_np_small;
	} else {
	    ffm = np + p;
	    m = ffm;
	    fm = m;
	    npq = np * q;
	    p1 = (int)(2.195 * sqrt(npq) - 4.6 * q) + 0.5;
	    xm = fm + 0.5;
	    xl = xm - p1;
	    xr = xm + p1;
	    c = 0.134 + 20.5 / (15.3 + fm);
	    al = (ffm - xl) / (ffm - xl * p);
	    xll = al * (1.0 + 0.5 * al);
	    al = (xr - ffm) / (xr * q);
	    xlr = al * (1.0 + 0.5 * al);
	    p2 = p1 * (1.0 + c + c);
	    p3 = p2 + c / xll;
	    p4 = p3 + c / xlr;
	}
    } else if (n == nsave) {
	if (np < 30.0)
	    goto L_np_small;
    }

    /*-------------------------- np = n*p >= 30 : ------------------- */
    repeat {
      u = unif_rand() * p4;
      v = unif_rand();
      /* triangular region */
      if (u <= p1) {
	  ix = xm - p1 * v + u;
	  goto finis;
      }
      /* parallelogram region */
      if (u <= p2) {
	  x = xl + (u - p1) / c;
	  v = v * c + 1.0 - fabs(xm - x) / p1;
	  if (v > 1.0 || v <= 0.)
	      continue;
	  ix = x;
      } else {
	  if (u > p3) {	/* right tail */
	      ix = xr - log(v) / xlr;
	      if (ix > n)
		  continue;
	      v = v * (u - p3) * xlr;
	  } else {/* left tail */
	      ix = xl + log(v) / xll;
	      if (ix < 0)
		  continue;
	      v = v * (u - p2) * xll;
	  }
      }
      /* determine appropriate way to perform accept/reject test */
      k = abs(ix - m);
      if (k <= 20 || k >= npq / 2 - 1) {
	  /* explicit evaluation */
	  f = 1.0;
	  if (m < ix) {
	      for (i = m + 1; i <= ix; i++)
		  f *= (g / i - r);
	  } else if (m != ix) {
	      for (i = ix + 1; i <= m; i++)
		  f /= (g / i - r);
	  }
	  if (v <= f)
	      goto finis;
      } else {
	  /* squeezing using upper and lower bounds on log(f(x)) */
	  amaxp = (k / npq) * ((k * (k / 3. + 0.625) + 0.1666666666666) / npq + 0.5);
	  ynorm = -k * k / (2.0 * npq);
	  alv = log(v);
	  if (alv < ynorm - amaxp)
	      goto finis;
	  if (alv <= ynorm + amaxp) {
	      /* stirling's formula to machine accuracy */
	      /* for the final acceptance/rejection test */
	      x1 = ix + 1;
	      f1 = fm + 1.0;
	      z = n + 1 - fm;
	      w = n - ix + 1.0;
	      z2 = z * z;
	      x2 = x1 * x1;
	      f2 = f1 * f1;
	      w2 = w * w;
	      if (alv <= xm * log(f1 / x1) + (n - m + 0.5) * log(z / w) + (ix - m) * log(w * p / (x1 * q)) + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / f2) / f2) / f2) / f2) / f1 / 166320.0 + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / z2) / z2) / z2) / z2) / z / 166320.0 + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / x2) / x2) / x2) / x2) / x1 / 166320.0 + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / w2) / w2) / w2) / w2) / w / 166320.)
		  goto finis;
	  }
      }
  }

 L_np_small:
    /*---------------------- np = n*p < 30 : ------------------------- */

  repeat {
     ix = 0;
     f = qn;
     u = unif_rand();
     repeat {
	 if (u < f)
	     goto finis;
	 if (ix > 110)
	     break;
	 u -= f;
	 ix++;
	 f *= (g / ix - r);
     }
  }
 finis:
    if (psave > 0.5)
	 ix = n - ix;
  return (double)ix;
}
