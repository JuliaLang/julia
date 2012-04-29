/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2005 Ross Ihaka and the R Development Core team.
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


/* From http://www.netlib.org/specfun/rjbesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 * Additional code for nu == alpha < 0  MM
 */
#include "nmath.h"
#include "bessel.h"

#ifndef MATHLIB_STANDALONE
#include <R_ext/Memory.h>
#endif

static void J_bessel(double *x, double *alpha, long *nb,
		     double *b, long *ncalc);

double bessel_j(double x, double alpha)
{
    long nb, ncalc;
    double na, *bj;
#ifndef MATHLIB_STANDALONE
    const void *vmax;
#endif

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_ERROR(ME_RANGE, "bessel_j");
	return ML_NAN;
    }
    na = floor(alpha);
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.1.2
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(bessel_j(x, -alpha) * cos(M_PI * alpha) +
	       ((alpha == na) ? 0 :
	       bessel_y(x, -alpha) * sin(M_PI * alpha)));
    }
    nb = 1 + (long)na; /* nb-1 <= alpha < nb */
    alpha -= (nb-1);
#ifdef MATHLIB_STANDALONE
    bj = (double *) calloc(nb, sizeof(double));
    if (!bj) MATHLIB_ERROR("%s", _("bessel_j allocation error"));
#else
    vmax = vmaxget();
    bj = (double *) R_alloc((size_t) nb, sizeof(double));
#endif
    J_bessel(&x, &alpha, &nb, bj, &ncalc);
    if(ncalc != nb) {/* error input */
      if(ncalc < 0)
	MATHLIB_WARNING4(_("bessel_j(%g): ncalc (=%ld) != nb (=%ld); alpha=%g. Arg. out of range?\n"),
			 x, ncalc, nb, alpha);
      else
	MATHLIB_WARNING2(_("bessel_j(%g,nu=%g): precision lost in result\n"),
			 x, alpha+nb-1);
    }
    x = bj[nb-1];
#ifdef MATHLIB_STANDALONE
    free(bj);
#else
    vmaxset(vmax);
#endif
    return x;
}

/* modified version of bessel_j that accepts a work array instead of
   allocating one. */
double bessel_j_ex(double x, double alpha, double *bj)
{
    long nb, ncalc;
    double na;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_ERROR(ME_RANGE, "bessel_j");
	return ML_NAN;
    }
    na = floor(alpha);
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.1.2
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(bessel_j_ex(x, -alpha, bj) * cos(M_PI * alpha) +
	       ((alpha == na) ? 0 :
		bessel_y_ex(x, -alpha, bj) * sin(M_PI * alpha)));
    }
    nb = 1 + (long)na; /* nb-1 <= alpha < nb */
    alpha -= (nb-1);
    J_bessel(&x, &alpha, &nb, bj, &ncalc);
    if(ncalc != nb) {/* error input */
      if(ncalc < 0)
	MATHLIB_WARNING4(_("bessel_j(%g): ncalc (=%ld) != nb (=%ld); alpha=%g. Arg. out of range?\n"),
			 x, ncalc, nb, alpha);
      else
	MATHLIB_WARNING2(_("bessel_j(%g,nu=%g): precision lost in result\n"),
			 x, alpha+nb-1);
    }
    x = bj[nb-1];
    return x;
}

static void J_bessel(double *x, double *alpha, long *nb,
		     double *b, long *ncalc)
{
/*
 Calculates Bessel functions J_{n+alpha} (x)
 for non-negative argument x, and non-negative order n+alpha, n = 0,1,..,nb-1.

  Explanation of variables in the calling sequence.

 X     - Non-negative argument for which J's are to be calculated.
 ALPHA - Fractional part of order for which
	 J's are to be calculated.  0 <= ALPHA < 1.
 NB    - Number of functions to be calculated, NB >= 1.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 B     - Output vector of length NB.  If RJBESL
	 terminates normally (NCALC=NB), the vector B contains the
	 functions J/ALPHA/(X) through J/NB-1+ALPHA/(X).
 NCALC - Output variable indicating possible errors.
	 Before using the vector B, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See the following

	 ****************************************************************

 Error return codes

    In case of an error,  NCALC != NB, and not all J's are
    calculated to the desired accuracy.

    NCALC < 0:	An argument is out of range. For example,
       NBES <= 0, ALPHA < 0 or > 1, or X is too large.
       In this case, b[1] is set to zero, the remainder of the
       B-vector is not calculated, and NCALC is set to
       MIN(NB,0)-1 so that NCALC != NB.

    NB > NCALC > 0: Not all requested function values could
       be calculated accurately.  This usually occurs because NB is
       much larger than ABS(X).	 In this case, b[N] is calculated
       to the desired accuracy for N <= NCALC, but precision
       is lost for NCALC < N <= NB.  If b[N] does not vanish
       for N > NCALC (because it is too small to be represented),
       and b[N]/b[NCALC] = 10^(-K), then only the first NSIG - K
       significant figures of b[N] can be trusted.


  Acknowledgement

	This program is based on a program written by David J. Sookne
	(2) that computes values of the Bessel functions J or I of float
	argument and long order.  Modifications include the restriction
	of the computation to the J Bessel function of non-negative float
	argument, the extension of the computation to arbitrary positive
	order, and the elimination of most underflow.

  References:

	Olver, F.W.J., and Sookne, D.J. (1972)
	"A Note on Backward Recurrence Algorithms";
	Math. Comp. 26, 941-947.

	Sookne, D.J. (1973)
	"Bessel Functions of Real Argument and Integer Order";
	NBS Jour. of Res. B. 77B, 125-132.

  Latest modification: March 19, 1990

  Author: W. J. Cody
	  Applied Mathematics Division
	  Argonne National Laboratory
	  Argonne, IL  60439
 *******************************************************************
 */

/* ---------------------------------------------------------------------
  Mathematical constants

   PI2	  = 2 / PI
   TWOPI1 = first few significant digits of 2 * PI
   TWOPI2 = (2*PI - TWOPI1) to working precision, i.e.,
	    TWOPI1 + TWOPI2 = 2 * PI to extra precision.
 --------------------------------------------------------------------- */
    const static double pi2 = .636619772367581343075535;
    const static double twopi1 = 6.28125;
    const static double twopi2 =  .001935307179586476925286767;

/*---------------------------------------------------------------------
 *  Factorial(N)
 *--------------------------------------------------------------------- */
    const static double fact[25] = { 1.,1.,2.,6.,24.,120.,720.,5040.,40320.,
	    362880.,3628800.,39916800.,479001600.,6227020800.,87178291200.,
	    1.307674368e12,2.0922789888e13,3.55687428096e14,6.402373705728e15,
	    1.21645100408832e17,2.43290200817664e18,5.109094217170944e19,
	    1.12400072777760768e21,2.585201673888497664e22,
	    6.2044840173323943936e23 };

    /* Local variables */
    long nend, intx, nbmx, i, j, k, l, m, n, nstart;

    double nu, twonu, capp, capq, pold, vcos, test, vsin;
    double p, s, t, z, alpem, halfx, aa, bb, cc, psave, plast;
    double tover, t1, alp2em, em, en, xc, xk, xm, psavel, gnu, xin, sum;


    /* Parameter adjustment */
    --b;

    nu = *alpha;
    twonu = nu + nu;

    /*-------------------------------------------------------------------
      Check for out of range arguments.
      -------------------------------------------------------------------*/
    if (*nb > 0 && *x >= 0. && 0. <= nu && nu < 1.) {

	*ncalc = *nb;
	if(*x > xlrg_BESS_IJ) {
	    ML_ERROR(ME_RANGE, "J_bessel");
	    /* indeed, the limit is 0,
	     * but the cutoff happens too early */
	    for(i=1; i <= *nb; i++)
		b[i] = 0.; /*was ML_POSINF (really nonsense) */
	    return;
	}
	intx = (long) (*x);
	/* Initialize result array to zero. */
	for (i = 1; i <= *nb; ++i)
	    b[i] = 0.;

	/*===================================================================
	  Branch into  3 cases :
	  1) use 2-term ascending series for small X
	  2) use asymptotic form for large X when NB is not too large
	  3) use recursion otherwise
	  ===================================================================*/

	if (*x < rtnsig_BESS) {
	  /* ---------------------------------------------------------------
	     Two-term ascending series for small X.
	     --------------------------------------------------------------- */
	    alpem = 1. + nu;

	    halfx = (*x > enmten_BESS) ? .5 * *x :  0.;
	    aa	  = (nu != 0.)	  ? pow(halfx, nu) / (nu * gamma_cody(nu)) : 1.;
	    bb	  = (*x + 1. > 1.)? -halfx * halfx : 0.;
	    b[1] = aa + aa * bb / alpem;
	    if (*x != 0. && b[1] == 0.)
		*ncalc = 0;

	    if (*nb != 1) {
		if (*x <= 0.) {
		    for (n = 2; n <= *nb; ++n)
			b[n] = 0.;
		}
		else {
		    /* ----------------------------------------------
		       Calculate higher order functions.
		       ---------------------------------------------- */
		    if (bb == 0.)
			tover = (enmten_BESS + enmten_BESS) / *x;
		    else
			tover = enmten_BESS / bb;
		    cc = halfx;
		    for (n = 2; n <= *nb; ++n) {
			aa /= alpem;
			alpem += 1.;
			aa *= cc;
			if (aa <= tover * alpem)
			    aa = 0.;

			b[n] = aa + aa * bb / alpem;
			if (b[n] == 0. && *ncalc > n)
			    *ncalc = n - 1;
		    }
		}
	    }
	} else if (*x > 25. && *nb <= intx + 1) {
	    /* ------------------------------------------------------------
	       Asymptotic series for X > 25 (and not too large nb)
	       ------------------------------------------------------------ */
	    xc = sqrt(pi2 / *x);
	    xin = 1 / (64 * *x * *x);
	    if (*x >= 130.)	m = 4;
	    else if (*x >= 35.) m = 8;
	    else		m = 11;
	    xm = 4. * (double) m;
	    /* ------------------------------------------------
	       Argument reduction for SIN and COS routines.
	       ------------------------------------------------ */
	    t = ftrunc(*x / (twopi1 + twopi2) + .5);
	    z = (*x - t * twopi1) - t * twopi2 - (nu + .5) / pi2;
	    vsin = sin(z);
	    vcos = cos(z);
	    gnu = twonu;
	    for (i = 1; i <= 2; ++i) {
		s = (xm - 1. - gnu) * (xm - 1. + gnu) * xin * .5;
		t = (gnu - (xm - 3.)) * (gnu + (xm - 3.));
		t1= (gnu - (xm + 1.)) * (gnu + (xm + 1.));
		k = m + m;
		capp = s * t / fact[k];
		capq = s * t1/ fact[k + 1];
		xk = xm;
		for (; k >= 4; k -= 2) {/* k + 2(j-2) == 2m */
		    xk -= 4.;
		    s = (xk - 1. - gnu) * (xk - 1. + gnu);
		    t1 = t;
		    t = (gnu - (xk - 3.)) * (gnu + (xk - 3.));
		    capp = (capp + 1. / fact[k - 2]) * s * t  * xin;
		    capq = (capq + 1. / fact[k - 1]) * s * t1 * xin;

		}
		capp += 1.;
		capq = (capq + 1.) * (gnu * gnu - 1.) * (.125 / *x);
		b[i] = xc * (capp * vcos - capq * vsin);
		if (*nb == 1)
		    return;

		/* vsin <--> vcos */ t = vsin; vsin = -vcos; vcos = t;
		gnu += 2.;
	    }
	    /* -----------------------------------------------
	       If  NB > 2, compute J(X,ORDER+I)	for I = 2, NB-1
	       ----------------------------------------------- */
	    if (*nb > 2)
		for (gnu = twonu + 2., j = 3; j <= *nb; j++, gnu += 2.)
		    b[j] = gnu * b[j - 1] / *x - b[j - 2];
	}
	else {
	    /* rtnsig_BESS <= x && ( x <= 25 || intx+1 < *nb ) :
	       --------------------------------------------------------
	       Use recurrence to generate results.
	       First initialize the calculation of P*S.
	       -------------------------------------------------------- */
	    nbmx = *nb - intx;
	    n = intx + 1;
	    en = (double)(n + n) + twonu;
	    plast = 1.;
	    p = en / *x;
	    /* ---------------------------------------------------
	       Calculate general significance test.
	       --------------------------------------------------- */
	    test = ensig_BESS + ensig_BESS;
	    if (nbmx >= 3) {
		/* ------------------------------------------------------------
		   Calculate P*S until N = NB-1.  Check for possible overflow.
		   ---------------------------------------------------------- */
		tover = enten_BESS / ensig_BESS;
		nstart = intx + 2;
		nend = *nb - 1;
		en = (double) (nstart + nstart) - 2. + twonu;
		for (k = nstart; k <= nend; ++k) {
		    n = k;
		    en += 2.;
		    pold = plast;
		    plast = p;
		    p = en * plast / *x - pold;
		    if (p > tover) {
			/* -------------------------------------------
			   To avoid overflow, divide P*S by TOVER.
			   Calculate P*S until ABS(P) > 1.
			   -------------------------------------------*/
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
			    p = en * plast / *x - pold;
			} while (p <= 1.);

			bb = en / *x;
			/* -----------------------------------------------
			   Calculate backward test and find NCALC,
			   the highest N such that the test is passed.
			   ----------------------------------------------- */
			test = pold * plast * (.5 - .5 / (bb * bb));
			test /= ensig_BESS;
			p = plast * tover;
			--n;
			en -= 2.;
			nend = imin2(*nb,n);
			for (l = nstart; l <= nend; ++l) {
			    pold = psavel;
			    psavel = psave;
			    psave = en * psavel / *x - pold;
			    if (psave * psavel > test) {
				*ncalc = l - 1;
				goto L190;
			    }
			}
			*ncalc = nend;
			goto L190;
		    }
		}
		n = nend;
		en = (double) (n + n) + twonu;
		/* -----------------------------------------------------
		   Calculate special significance test for NBMX > 2.
		   -----------------------------------------------------*/
		test = fmax2(test, sqrt(plast * ensig_BESS) * sqrt(p + p));
	    }
	    /* ------------------------------------------------
	       Calculate P*S until significance test passes. */
	    do {
		++n;
		en += 2.;
		pold = plast;
		plast = p;
		p = en * plast / *x - pold;
	    } while (p < test);

L190:
	    /*---------------------------------------------------------------
	      Initialize the backward recursion and the normalization sum.
	      --------------------------------------------------------------- */
	    ++n;
	    en += 2.;
	    bb = 0.;
	    aa = 1. / p;
	    m = n / 2;
	    em = (double)m;
	    m = (n << 1) - (m << 2);/* = 2 n - 4 (n/2)
				       = 0 for even, 2 for odd n */
	    if (m == 0)
		sum = 0.;
	    else {
		alpem = em - 1. + nu;
		alp2em = em + em + nu;
		sum = aa * alpem * alp2em / em;
	    }
	    nend = n - *nb;
	    /* if (nend > 0) */
	    /* --------------------------------------------------------
	       Recur backward via difference equation, calculating
	       (but not storing) b[N], until N = NB.
	       -------------------------------------------------------- */
	    for (l = 1; l <= nend; ++l) {
		--n;
		en -= 2.;
		cc = bb;
		bb = aa;
		aa = en * bb / *x - cc;
		m = m ? 0 : 2; /* m = 2 - m failed on gcc4-20041019 */
		if (m != 0) {
		    em -= 1.;
		    alp2em = em + em + nu;
		    if (n == 1)
			break;

		    alpem = em - 1. + nu;
		    if (alpem == 0.)
			alpem = 1.;
		    sum = (sum + aa * alp2em) * alpem / em;
		}
	    }
	    /*--------------------------------------------------
	      Store b[NB].
	      --------------------------------------------------*/
	    b[n] = aa;
	    if (nend >= 0) {
		if (*nb <= 1) {
		    if (nu + 1. == 1.)
			alp2em = 1.;
		    else
			alp2em = nu;
		    sum += b[1] * alp2em;
		    goto L250;
		}
		else {/*-- nb >= 2 : ---------------------------
			Calculate and store b[NB-1].
			----------------------------------------*/
		    --n;
		    en -= 2.;
		    b[n] = en * aa / *x - bb;
		    if (n == 1)
			goto L240;

		    m = m ? 0 : 2; /* m = 2 - m failed on gcc4-20041019 */
		    if (m != 0) {
			em -= 1.;
			alp2em = em + em + nu;
			alpem = em - 1. + nu;
			if (alpem == 0.)
			    alpem = 1.;
			sum = (sum + b[n] * alp2em) * alpem / em;
		    }
		}
	    }

	    /* if (n - 2 != 0) */
	    /* --------------------------------------------------------
	       Calculate via difference equation and store b[N],
	       until N = 2.
	       -------------------------------------------------------- */
	    for (n = n-1; n >= 2; n--) {
		en -= 2.;
		b[n] = en * b[n + 1] / *x - b[n + 2];
		m = m ? 0 : 2; /* m = 2 - m failed on gcc4-20041019 */
		if (m != 0) {
		    em -= 1.;
		    alp2em = em + em + nu;
		    alpem = em - 1. + nu;
		    if (alpem == 0.)
			alpem = 1.;
		    sum = (sum + b[n] * alp2em) * alpem / em;
		}
	    }
	    /* ---------------------------------------
	       Calculate b[1].
	       -----------------------------------------*/
	    b[1] = 2. * (nu + 1.) * b[2] / *x - b[3];

L240:
	    em -= 1.;
	    alp2em = em + em + nu;
	    if (alp2em == 0.)
		alp2em = 1.;
	    sum += b[1] * alp2em;

L250:
	    /* ---------------------------------------------------
	       Normalize.  Divide all b[N] by sum.
	       ---------------------------------------------------*/
/*	    if (nu + 1. != 1.) poor test */
	    if(fabs(nu) > 1e-15)
		sum *= (gamma_cody(nu) * pow(.5* *x, -nu));

	    aa = enmten_BESS;
	    if (sum > 1.)
		aa *= sum;
	    for (n = 1; n <= *nb; ++n) {
		if (fabs(b[n]) < aa)
		    b[n] = 0.;
		else
		    b[n] /= sum;
	    }
	}

    }
    else {
      /* Error return -- X, NB, or ALPHA is out of range : */
	b[1] = 0.;
	*ncalc = imin2(*nb,0) - 1;
    }
}
