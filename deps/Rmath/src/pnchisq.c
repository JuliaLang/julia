/*
 *  Algorithm AS 275 Appl.Statist. (1992), vol.41, no.2
 *  original  (C) 1992	     Royal Statistical Society
 *
 *  Computes the noncentral chi-squared distribution function with
 *  positive real degrees of freedom df and nonnegative noncentrality
 *  parameter ncp.  pnchisq_raw is based on
 *
 *    Ding, C. G. (1992)
 *    Algorithm AS275: Computing the non-central chi-squared
 *    distribution function. Appl.Statist., 41, 478-482.

 *  Other parts
 *  Copyright (C) 2000-2009  The R Development Core Team
 *  Copyright (C) 2003-2009  The R Foundation
 */

#include "nmath.h"
#include "dpq.h"

/*----------- DEBUGGING -------------
 *
 *	make CFLAGS='-DDEBUG_pnch ....'

 * -- Feb.6, 2000 (R pre0.99); M.Maechler:  still have
 * bad precision & non-convergence in some cases (x ~= f, both LARGE)
 */


double pnchisq(double x, double df, double ncp, int lower_tail, int log_p)
{
    double ans;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df) || ISNAN(ncp))
	return x + df + ncp;
    if (!R_FINITE(df) || !R_FINITE(ncp))
	ML_ERR_return_NAN;
#endif

    if (df < 0. || ncp < 0.) ML_ERR_return_NAN;

    ans = pnchisq_raw(x, df, ncp, 1e-12, 8*DBL_EPSILON, 1000000, lower_tail);
    if(ncp >= 80) {
	if(lower_tail) {
	    ans = fmin2(ans, 1.0);  /* e.g., pchisq(555, 1.01, ncp = 80) */
	} else { /* !lower_tail */
	    /* since we computed the other tail cancellation is likely */
	    if(ans < 1e-10) ML_ERROR(ME_PRECISION, "pnchisq");
	    ans = fmax2(ans, 0.0);  /* Precaution PR#7099 */
	}
    }
    if (!log_p) return ans;
    /* if ans is near one, we can do better using the other tail */
    if (ncp >= 80 || ans < 1 - 1e-8) return log(ans);
    ans = pnchisq_raw(x, df, ncp, 1e-12, 8*DBL_EPSILON, 1000000, !lower_tail);
    return log1p(-ans);
}

double attribute_hidden
pnchisq_raw(double x, double f, double theta,
	    double errmax, double reltol, int itrmax, Rboolean lower_tail)
{
    double lam, x2, f2, term, bound, f_x_2n, f_2n;
    double l_lam = -1., l_x = -1.; /* initialized for -Wall */
    int n;
    Rboolean lamSml, tSml, is_r, is_b, is_it;
    long double ans, u, v, t, lt, lu =-1;

    static const double _dbl_min_exp = M_LN2 * DBL_MIN_EXP;
    /*= -708.3964 for IEEE double precision */

    if (x <= 0.) {
	if(x == 0. && f == 0.)
	    return lower_tail ? exp(-0.5*theta) : -expm1(-0.5*theta);
	/* x < 0  or {x==0, f > 0} */
	return lower_tail ? 0. : 1.;
    }
    if(!R_FINITE(x))	return lower_tail ? 1. : 0.;

    /* This is principally for use from qnchisq */
#ifndef MATHLIB_STANDALONE
    R_CheckUserInterrupt();
#endif

    if(theta < 80) { /* use 110 for Inf, as ppois(110, 80/2, lower.tail=FALSE) is 2e-20 */
	long double sum = 0, sum2 = 0, lambda = 0.5*theta, pr = exp(-lambda);
	double ans;
	int i;
	/* we need to renormalize here: the result could be very close to 1 */
	for(i = 0; i < 110;  pr *= lambda/++i) {
	    sum2 += pr;
	    sum += pr * pchisq(x, f+2*i, lower_tail, FALSE);
	    if (sum2 >= 1-1e-15) break;
	}
	ans = sum/sum2;
	return ans;
    }


#ifdef DEBUG_pnch
    REprintf("pnchisq(x=%g, f=%g, theta=%g): ",x,f,theta);
#endif
    lam = .5 * theta;
    lamSml = (-lam < _dbl_min_exp);
    if(lamSml) {
	/* MATHLIB_ERROR(
	   "non centrality parameter (= %g) too large for current algorithm",
	   theta) */
        u = 0;
        lu = -lam;/* == ln(u) */
        l_lam = log(lam);
    } else {
	u = exp(-lam);
    }

    /* evaluate the first term */
    v = u;
    x2 = .5 * x;
    f2 = .5 * f;
    f_x_2n = f - x;

#ifdef DEBUG_pnch
    REprintf("-- v=exp(-th/2)=%g, x/2= %g, f/2= %g\n",v,x2,f2);
#endif

    if(f2 * DBL_EPSILON > 0.125 && /* very large f and x ~= f: probably needs */
       fabs(t = x2 - f2) <         /* another algorithm anyway */
       sqrt(DBL_EPSILON) * f2) {
	/* evade cancellation error */
	/* t = exp((1 - t)*(2 - t/(f2 + 1))) / sqrt(2*M_PI*(f2 + 1));*/
        lt = (1 - t)*(2 - t/(f2 + 1)) - 0.5 * log(2*M_PI*(f2 + 1));
#ifdef DEBUG_pnch
	REprintf(" (case I) ==> ");
#endif
    }
    else {
	/* Usual case 2: careful not to overflow .. : */
	lt = f2*log(x2) -x2 - lgammafn(f2 + 1);
    }
#ifdef DEBUG_pnch
    REprintf(" lt= %g", lt);
#endif

    tSml = (lt < _dbl_min_exp);
    if(tSml) {
	if (x > f + theta +  5* sqrt( 2*(f + 2*theta))) {
	    /* x > E[X] + 5* sigma(X) */
	    return lower_tail ? 1. : 0.; /* FIXME: We could be more accurate than 0. */
	} /* else */
	l_x = log(x);
	ans = term = t = 0.;
    }
    else {
	t = exp(lt);
#ifdef DEBUG_pnch
 	REprintf(", t=exp(lt)= %g\n", t);
#endif
	ans = term = v * t;
    }

    for (n = 1, f_2n = f + 2., f_x_2n += 2.;  ; n++, f_2n += 2, f_x_2n += 2) {
#ifdef DEBUG_pnch
	REprintf("\n _OL_: n=%d",n);
#endif
#ifndef MATHLIB_STANDALONE
	if(n % 1000) R_CheckUserInterrupt();
#endif
	/* f_2n    === f + 2*n
	 * f_x_2n  === f - x + 2*n   > 0  <==> (f+2n)  >   x */
	if (f_x_2n > 0) {
	    /* find the error bound and check for convergence */

	    bound = t * x / f_x_2n;
#ifdef DEBUG_pnch
	    REprintf("\n L10: n=%d; term= %g; bound= %g",n,term,bound);
#endif
	    is_r = is_it = FALSE;
	    /* convergence only if BOTH absolute and relative error < 'bnd' */
	    if (((is_b = (bound <= errmax)) &&
                 (is_r = (term <= reltol * ans))) || (is_it = (n > itrmax)))
            {
#ifdef DEBUG_pnch
                REprintf("BREAK n=%d %s; bound= %g %s, rel.err= %g %s\n",
			 n, (is_it ? "> itrmax" : ""),
			 bound, (is_b ? "<= errmax" : ""),
			 term/ans, (is_r ? "<= reltol" : ""));
#endif
		break; /* out completely */
            }

	}

	/* evaluate the next term of the */
	/* expansion and then the partial sum */

        if(lamSml) {
            lu += l_lam - log(n); /* u = u* lam / n */
            if(lu >= _dbl_min_exp) {
		/* no underflow anymore ==> change regime */
#ifdef DEBUG_pnch
                REprintf(" n=%d; nomore underflow in u = exp(lu) ==> change\n",
			 n);
#endif
                v = u = exp(lu); /* the first non-0 'u' */
                lamSml = FALSE;
            }
        } else {
	    u *= lam / n;
	    v += u;
	}
	if(tSml) {
            lt += l_x - log(f_2n);/* t <- t * (x / f2n) */
            if(lt >= _dbl_min_exp) {
		/* no underflow anymore ==> change regime */
#ifdef DEBUG_pnch
                REprintf("  n=%d; nomore underflow in t = exp(lt) ==> change\n",
			 n);
#endif
                t = exp(lt); /* the first non-0 't' */
                tSml = FALSE;
            }
        } else {
	    t *= x / f_2n;
	}
        if(!lamSml && !tSml) {
	    term = v * t;
	    ans += term;
	}

    } /* for(n ...) */

    if (is_it) {
	MATHLIB_WARNING2(_("pnchisq(x=%g, ..): not converged in %d iter."),
			 x, itrmax);
    }
#ifdef DEBUG_pnch
    REprintf("\n == L_End: n=%d; term= %g; bound=%g\n",n,term,bound);
#endif
    return lower_tail ? ans : 1 - ans;
}
