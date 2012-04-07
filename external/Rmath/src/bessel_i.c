/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2001 Ross Ihaka and the R Development Core team.
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
 */

/*  DESCRIPTION --> see below */


/* From http://www.netlib.org/specfun/ribesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 */
#include "nmath.h"
#include "bessel.h"

#ifndef MATHLIB_STANDALONE
#include <R_ext/Memory.h>
#endif

static void I_bessel(double *x, double *alpha, long *nb,
		     long *ize, double *bi, long *ncalc);

/* .Internal(besselI(*)) : */
double bessel_i(double x, double alpha, double expo)
{
    long nb, ncalc, ize;
    double na, *bi;
#ifndef MATHLIB_STANDALONE
    const void *vmax;
#endif

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_ERROR(ME_RANGE, "bessel_i");
	return ML_NAN;
    }
    ize = (long)expo;
    na = floor(alpha);
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.6.2 & 9.6.6
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(bessel_i(x, -alpha, expo) +
	       ((alpha == na) ? /* sin(pi * alpha) = 0 */ 0 :
		bessel_k(x, -alpha, expo) *
		((ize == 1)? 2. : 2.*exp(-2.*x))/M_PI * sin(-M_PI * alpha)));
    }
    nb = 1 + (long)na;/* nb-1 <= alpha < nb */
    alpha -= (nb-1);
#ifdef MATHLIB_STANDALONE
    bi = (double *) calloc(nb, sizeof(double));
    if (!bi) MATHLIB_ERROR("%s", _("bessel_i allocation error"));
#else
    vmax = vmaxget();
    bi = (double *) R_alloc((size_t) nb, sizeof(double));
#endif
    I_bessel(&x, &alpha, &nb, &ize, bi, &ncalc);
    if(ncalc != nb) {/* error input */
	if(ncalc < 0)
	    MATHLIB_WARNING4(_("bessel_i(%g): ncalc (=%ld) != nb (=%ld); alpha=%g. Arg. out of range?\n"),
			     x, ncalc, nb, alpha);
	else
	    MATHLIB_WARNING2(_("bessel_i(%g,nu=%g): precision lost in result\n"),
			     x, alpha+nb-1);
    }
    x = bi[nb-1];
#ifdef MATHLIB_STANDALONE
    free(bi);
#else
    vmaxset(vmax);
#endif
    return x;
}

/* modified version of bessel_i that accepts a work array instead of
   allocating one. */
double bessel_i_ex(double x, double alpha, double expo, double *bi)
{
    long nb, ncalc, ize;
    double na;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_ERROR(ME_RANGE, "bessel_i");
	return ML_NAN;
    }
    ize = (long)expo;
    na = floor(alpha);
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.6.2 & 9.6.6
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(bessel_i_ex(x, -alpha, expo, bi) +
	       ((alpha == na) ? 0 :
		bessel_k_ex(x, -alpha, expo, bi) *
		((ize == 1)? 2. : 2.*exp(-2.*x))/M_PI * sin(-M_PI * alpha)));
    }
    nb = 1 + (long)na;/* nb-1 <= alpha < nb */
    alpha -= (nb-1);
    I_bessel(&x, &alpha, &nb, &ize, bi, &ncalc);
    if(ncalc != nb) {/* error input */
	if(ncalc < 0)
	    MATHLIB_WARNING4(_("bessel_i(%g): ncalc (=%ld) != nb (=%ld); alpha=%g. Arg. out of range?\n"),
			     x, ncalc, nb, alpha);
	else
	    MATHLIB_WARNING2(_("bessel_i(%g,nu=%g): precision lost in result\n"),
			     x, alpha+nb-1);
    }
    x = bi[nb-1];
    return x;
}

static void I_bessel(double *x, double *alpha, long *nb,
		     long *ize, double *bi, long *ncalc)
{
/* -------------------------------------------------------------------

 This routine calculates Bessel functions I_(N+ALPHA) (X)
 for non-negative argument X, and non-negative order N+ALPHA,
 with or without exponential scaling.


 Explanation of variables in the calling sequence

 X     - Non-negative argument for which
	 I's or exponentially scaled I's (I*EXP(-X))
	 are to be calculated.	If I's are to be calculated,
	 X must be less than exparg_BESS (IZE=1) or xlrg_BESS_IJ (IZE=2),
	 (see bessel.h).
 ALPHA - Fractional part of order for which
	 I's or exponentially scaled I's (I*EXP(-X)) are
	 to be calculated.  0 <= ALPHA < 1.0.
 NB    - Number of functions to be calculated, NB > 0.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 IZE   - Type.	IZE = 1 if unscaled I's are to be calculated,
		    = 2 if exponentially scaled I's are to be calculated.
 BI    - Output vector of length NB.	If the routine
	 terminates normally (NCALC=NB), the vector BI contains the
	 functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
	 corresponding exponentially scaled functions.
 NCALC - Output variable indicating possible errors.
	 Before using the vector BI, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See error returns below.


 *******************************************************************
 *******************************************************************

 Error returns

  In case of an error,	NCALC != NB, and not all I's are
  calculated to the desired accuracy.

  NCALC < 0:  An argument is out of range. For example,
     NB <= 0, IZE is not 1 or 2, or IZE=1 and ABS(X) >= EXPARG_BESS.
     In this case, the BI-vector is not calculated, and NCALC is
     set to MIN0(NB,0)-1 so that NCALC != NB.

  NB > NCALC > 0: Not all requested function values could
     be calculated accurately.	This usually occurs because NB is
     much larger than ABS(X).  In this case, BI[N] is calculated
     to the desired accuracy for N <= NCALC, but precision
     is lost for NCALC < N <= NB.  If BI[N] does not vanish
     for N > NCALC (because it is too small to be represented),
     and BI[N]/BI[NCALC] = 10**(-K), then only the first NSIG-K
     significant figures of BI[N] can be trusted.


 Intrinsic functions required are:

     DBLE, EXP, gamma_cody, INT, MAX, MIN, REAL, SQRT


 Acknowledgement

  This program is based on a program written by David J.
  Sookne (2) that computes values of the Bessel functions J or
  I of float argument and long order.  Modifications include
  the restriction of the computation to the I Bessel function
  of non-negative float argument, the extension of the computation
  to arbitrary positive order, the inclusion of optional
  exponential scaling, and the elimination of most underflow.
  An earlier version was published in (3).

 References: "A Note on Backward Recurrence Algorithms," Olver,
	      F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
	      pp 941-947.

	     "Bessel Functions of Real Argument and Integer Order,"
	      Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
	      125-132.

	     "ALGORITHM 597, Sequence of Modified Bessel Functions
	      of the First Kind," Cody, W. J., Trans. Math. Soft.,
	      1983, pp. 242-245.

  Latest modification: May 30, 1989

  Modified by: W. J. Cody and L. Stoltz
	       Applied Mathematics Division
	       Argonne National Laboratory
	       Argonne, IL  60439
*/

    /*-------------------------------------------------------------------
      Mathematical constants
      -------------------------------------------------------------------*/
    const static double const__ = 1.585;

    /* Local variables */
    long nend, intx, nbmx, k, l, n, nstart;
    double pold, test,	p, em, en, empal, emp2al, halfx,
	aa, bb, cc, psave, plast, tover, psavel, sum, nu, twonu;

    /*Parameter adjustments */
    --bi;
    nu = *alpha;
    twonu = nu + nu;

    /*-------------------------------------------------------------------
      Check for X, NB, OR IZE out of range.
      ------------------------------------------------------------------- */
    if (*nb > 0 && *x >= 0. &&	(0. <= nu && nu < 1.) &&
	(1 <= *ize && *ize <= 2) ) {

	*ncalc = *nb;
	if(*ize == 1 && *x > exparg_BESS) {
	    for(k=1; k <= *nb; k++)
		bi[k]=ML_POSINF; /* the limit *is* = Inf */
	    return;
	}
	if(*ize == 2 && *x > xlrg_BESS_IJ) {
	    for(k=1; k <= *nb; k++)
		bi[k]= 0.; /* The limit exp(-x) * I_nu(x) --> 0 : */
	    return;
	}
	intx = (long) (*x);/* fine, since *x <= xlrg_BESS_IJ <<< LONG_MAX */
	if (*x >= rtnsig_BESS) { /* "non-small" x ( >= 1e-4 ) */
/* -------------------------------------------------------------------
   Initialize the forward sweep, the P-sequence of Olver
   ------------------------------------------------------------------- */
	    nbmx = *nb - intx;
	    n = intx + 1;
	    en = (double) (n + n) + twonu;
	    plast = 1.;
	    p = en / *x;
	    /* ------------------------------------------------
	       Calculate general significance test
	       ------------------------------------------------ */
	    test = ensig_BESS + ensig_BESS;
	    if (intx << 1 > nsig_BESS * 5) {
		test = sqrt(test * p);
	    } else {
		test /= pow(const__, (double)intx);
	    }
	    if (nbmx >= 3) {
		/* --------------------------------------------------
		   Calculate P-sequence until N = NB-1
		   Check for possible overflow.
		   ------------------------------------------------ */
		tover = enten_BESS / ensig_BESS;
		nstart = intx + 2;
		nend = *nb - 1;
		for (k = nstart; k <= nend; ++k) {
		    n = k;
		    en += 2.;
		    pold = plast;
		    plast = p;
		    p = en * plast / *x + pold;
		    if (p > tover) {
			/* ------------------------------------------------
			   To avoid overflow, divide P-sequence by TOVER.
			   Calculate P-sequence until ABS(P) > 1.
			   ---------------------------------------------- */
			tover = enten_BESS;
			p /= tover;
			plast /= tover;
			psave = p;
			psavel = plast;
			nstart = n + 1;
			do {
			    ++n;
			    en += 2.;
			    pold = plast;
			    plast = p;
			    p = en * plast / *x + pold;
			}
			while (p <= 1.);

			bb = en / *x;
			/* ------------------------------------------------
			   Calculate backward test, and find NCALC,
			   the highest N such that the test is passed.
			   ------------------------------------------------ */
			test = pold * plast / ensig_BESS;
			test *= .5 - .5 / (bb * bb);
			p = plast * tover;
			--n;
			en -= 2.;
			nend = imin2(*nb,n);
			for (l = nstart; l <= nend; ++l) {
			    *ncalc = l;
			    pold = psavel;
			    psavel = psave;
			    psave = en * psavel / *x + pold;
			    if (psave * psavel > test) {
				goto L90;
			    }
			}
			*ncalc = nend + 1;
L90:
			--(*ncalc);
			goto L120;
		    }
		}
		n = nend;
		en = (double)(n + n) + twonu;
		/*---------------------------------------------------
		  Calculate special significance test for NBMX > 2.
		  --------------------------------------------------- */
		test = fmax2(test,sqrt(plast * ensig_BESS) * sqrt(p + p));
	    }
	    /* --------------------------------------------------------
	       Calculate P-sequence until significance test passed.
	       -------------------------------------------------------- */
	    do {
		++n;
		en += 2.;
		pold = plast;
		plast = p;
		p = en * plast / *x + pold;
	    } while (p < test);

L120:
/* -------------------------------------------------------------------
 Initialize the backward recursion and the normalization sum.
 ------------------------------------------------------------------- */
	    ++n;
	    en += 2.;
	    bb = 0.;
	    aa = 1. / p;
	    em = (double) n - 1.;
	    empal = em + nu;
	    emp2al = em - 1. + twonu;
	    sum = aa * empal * emp2al / em;
	    nend = n - *nb;
	    if (nend < 0) {
		/* -----------------------------------------------------
		   N < NB, so store BI[N] and set higher orders to 0..
		   ----------------------------------------------------- */
		bi[n] = aa;
		nend = -nend;
		for (l = 1; l <= nend; ++l) {
		    bi[n + l] = 0.;
		}
	    } else {
		if (nend > 0) {
		    /* -----------------------------------------------------
		       Recur backward via difference equation,
		       calculating (but not storing) BI[N], until N = NB.
		       --------------------------------------------------- */

		    for (l = 1; l <= nend; ++l) {
			--n;
			en -= 2.;
			cc = bb;
			bb = aa;
			/* for x ~= 1500,  sum would overflow to 'inf' here,
			 * and the final bi[] /= sum would give 0 wrongly;
			 * RE-normalize (aa, sum) here -- no need to undo */
			if(nend > 100 && aa > 1e200) {
			    /* multiply by  2^-900 = 1.18e-271 */
			    cc	= ldexp(cc, -900);
			    bb	= ldexp(bb, -900);
			    sum = ldexp(sum,-900);
			}
			aa = en * bb / *x + cc;
			em -= 1.;
			emp2al -= 1.;
			if (n == 1) {
			    break;
			}
			if (n == 2) {
			    emp2al = 1.;
			}
			empal -= 1.;
			sum = (sum + aa * empal) * emp2al / em;
		    }
		}
		/* ---------------------------------------------------
		   Store BI[NB]
		   --------------------------------------------------- */
		bi[n] = aa;
		if (*nb <= 1) {
		    sum = sum + sum + aa;
		    goto L230;
		}
		/* -------------------------------------------------
		   Calculate and Store BI[NB-1]
		   ------------------------------------------------- */
		--n;
		en -= 2.;
		bi[n] = en * aa / *x + bb;
		if (n == 1) {
		    goto L220;
		}
		em -= 1.;
		if (n == 2)
		    emp2al = 1.;
		else
		    emp2al -= 1.;

		empal -= 1.;
		sum = (sum + bi[n] * empal) * emp2al / em;
	    }
	    nend = n - 2;
	    if (nend > 0) {
		/* --------------------------------------------
		   Calculate via difference equation
		   and store BI[N], until N = 2.
		   ------------------------------------------ */
		for (l = 1; l <= nend; ++l) {
		    --n;
		    en -= 2.;
		    bi[n] = en * bi[n + 1] / *x + bi[n + 2];
		    em -= 1.;
		    if (n == 2)
			emp2al = 1.;
		    else
			emp2al -= 1.;
		    empal -= 1.;
		    sum = (sum + bi[n] * empal) * emp2al / em;
		}
	    }
	    /* ----------------------------------------------
	       Calculate BI[1]
	       -------------------------------------------- */
	    bi[1] = 2. * empal * bi[2] / *x + bi[3];
L220:
	    sum = sum + sum + bi[1];

L230:
	    /* ---------------------------------------------------------
	       Normalize.  Divide all BI[N] by sum.
	       --------------------------------------------------------- */
	    if (nu != 0.)
		sum *= (gamma_cody(1. + nu) * pow(*x * .5, -nu));
	    if (*ize == 1)
		sum *= exp(-(*x));
	    aa = enmten_BESS;
	    if (sum > 1.)
		aa *= sum;
	    for (n = 1; n <= *nb; ++n) {
		if (bi[n] < aa)
		    bi[n] = 0.;
		else
		    bi[n] /= sum;
	    }
	    return;
	} else { /* small x  < 1e-4 */
	    /* -----------------------------------------------------------
	       Two-term ascending series for small X.
	       -----------------------------------------------------------*/
	    aa = 1.;
	    empal = 1. + nu;
#ifdef IEEE_754
	    /* No need to check for underflow */
	    halfx = .5 * *x;
#else
	    if (*x > enmten_BESS) */
		halfx = .5 * *x;
	    else
	    	halfx = 0.;
#endif
	    if (nu != 0.)
		aa = pow(halfx, nu) / gamma_cody(empal);
	    if (*ize == 2)
		aa *= exp(-(*x));
	    bb = halfx * halfx;
	    bi[1] = aa + aa * bb / empal;
	    if (*x != 0. && bi[1] == 0.)
		*ncalc = 0;
	    if (*nb > 1) {
		if (*x == 0.) {
		    for (n = 2; n <= *nb; ++n)
			bi[n] = 0.;
		} else {
		    /* -------------------------------------------------
		       Calculate higher-order functions.
		       ------------------------------------------------- */
		    cc = halfx;
		    tover = (enmten_BESS + enmten_BESS) / *x;
		    if (bb != 0.)
			tover = enmten_BESS / bb;
		    for (n = 2; n <= *nb; ++n) {
			aa /= empal;
			empal += 1.;
			aa *= cc;
			if (aa <= tover * empal)
			    bi[n] = aa = 0.;
			else
			    bi[n] = aa + aa * bb / empal;
			if (bi[n] == 0. && *ncalc > n)
			    *ncalc = n - 1;
		    }
		}
	    }
	}
    } else { /* argument out of range */
	*ncalc = imin2(*nb,0) - 1;
    }
}
