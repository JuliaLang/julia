/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000--2011 The R Development Core Team
 *  Copyright (C) 2004--2009 The R Foundation
 *  based on AS 91 (C) 1979 Royal Statistical Society
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *  DESCRIPTION
 *
 *	Compute the quantile function of the gamma distribution.
 *
 *  NOTES
 *
 *	This function is based on the Applied Statistics
 *	Algorithm AS 91 ("ppchi2") and via pgamma(.) AS 239.
 *
 *	R core improvements:
 *	o  lower_tail, log_p
 *      o  non-trivial result for p outside [0.000002, 0.999998]
 *	o  p ~ 1 no longer gives +Inf; final Newton step(s)
 *
 *  REFERENCES
 *
 *	Best, D. J. and D. E. Roberts (1975).
 *	Percentage Points of the Chi-Squared Distribution.
 *	Applied Statistics 24, page 385.  */

#include "nmath.h"
#include "dpq.h"

#ifdef DEBUG_qgamma
# define DEBUG_q
#endif

attribute_hidden
double qchisq_appr(double p, double nu, double g /* = log Gamma(nu/2) */,
		   int lower_tail, int log_p, double tol /* EPS1 */)
{
#define C7	4.67
#define C8	6.66
#define C9	6.73
#define C10	13.32

    double alpha, a, c, ch, p1;
    double p2, q, t, x;

    /* test arguments and initialise */

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(nu))
	return p + nu;
#endif
    R_Q_P01_check(p);
    if (nu <= 0) ML_ERR_return_NAN;

    alpha = 0.5 * nu;/* = [pq]gamma() shape */
    c = alpha-1;

    if(nu < (-1.24)*(p1 = R_DT_log(p))) {	/* for small chi-squared */
	/* log(alpha) + g = log(alpha) + log(gamma(alpha)) =
	 *        = log(alpha*gamma(alpha)) = lgamma(alpha+1) suffers from
	 *  catastrophic cancellation when alpha << 1
	 */
	double lgam1pa = (alpha < 0.5) ? lgamma1p(alpha) : (log(alpha) + g);
	ch = exp((lgam1pa + p1)/alpha + M_LN2);
#ifdef DEBUG_qgamma
	REprintf(" small chi-sq., ch0 = %g\n", ch);
#endif

    } else if(nu > 0.32) {	/*  using Wilson and Hilferty estimate */

	x = qnorm(p, 0, 1, lower_tail, log_p);
	p1 = 2./(9*nu);
	ch = nu*pow(x*sqrt(p1) + 1-p1, 3);

#ifdef DEBUG_qgamma
	REprintf(" nu > .32: Wilson-Hilferty; x = %7g\n", x);
#endif
	/* approximation for p tending to 1: */
	if( ch > 2.2*nu + 6 )
	    ch = -2*(R_DT_Clog(p) - c*log(0.5*ch) + g);

    } else { /* "small nu" : 1.24*(-log(p)) <= nu <= 0.32 */

	ch = 0.4;
	a = R_DT_Clog(p) + g + c*M_LN2;
#ifdef DEBUG_qgamma
	REprintf(" nu <= .32: a = %7g\n", a);
#endif
	do {
	    q = ch;
	    p1 = 1. / (1+ch*(C7+ch));
	    p2 = ch*(C9+ch*(C8+ch));
	    t = -0.5 +(C7+2*ch)*p1 - (C9+ch*(C10+3*ch))/p2;
	    ch -= (1- exp(a+0.5*ch)*p2*p1)/t;
	} while(fabs(q - ch) > tol * fabs(ch));
    }

    return ch;
}

double qgamma(double p, double alpha, double scale, int lower_tail, int log_p)
/*			shape = alpha */
{
#define EPS1 1e-2
#define EPS2 5e-7/* final precision of AS 91 */
#define EPS_N 1e-15/* precision of Newton step / iterations */
#define LN_EPS -36.043653389117156 /* = log(.Machine$double.eps) iff IEEE_754 */

#define MAXIT 1000/* was 20 */

#define pMIN 1e-100   /* was 0.000002 = 2e-6 */
#define pMAX (1-1e-14)/* was (1-1e-12) and 0.999998 = 1 - 2e-6 */

    const static double
	i420  = 1./ 420.,
	i2520 = 1./ 2520.,
	i5040 = 1./ 5040;

    double p_, a, b, c, g, ch, ch0, p1;
    double p2, q, s1, s2, s3, s4, s5, s6, t, x;
    int i, max_it_Newton = 1;

    /* test arguments and initialise */

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(alpha) || ISNAN(scale))
	return p + alpha + scale;
#endif
    R_Q_P01_boundaries(p, 0., ML_POSINF);

    if (alpha < 0 || scale <= 0) ML_ERR_return_NAN;

    if (alpha == 0) /* all mass at 0 : */ return 0.;

    if (alpha < 1e-10) {
    /* Warning seems unnecessary now: */
#ifdef _DO_WARN_qgamma_
	MATHLIB_WARNING("value of shape (%g) is extremely small: results may be unreliable", alpha);
#endif
	max_it_Newton = 7;/* may still be increased below */
    }

    p_ = R_DT_qIv(p);/* lower_tail prob (in any case) */

#ifdef DEBUG_qgamma
    REprintf("qgamma(p=%7g, alpha=%7g, scale=%7g, l.t.=%2d, log_p=%2d): ",
	     p,alpha,scale, lower_tail, log_p);
#endif
    g = lgammafn(alpha);/* log Gamma(v/2) */

    /*----- Phase I : Starting Approximation */
    ch = qchisq_appr(p, /* nu= 'df' =  */ 2*alpha, /* lgamma(nu/2)= */ g,
		     lower_tail, log_p, /* tol= */ EPS1);
    if(!R_FINITE(ch)) {
	/* forget about all iterations! */
	max_it_Newton = 0; goto END;
    }
    if(ch < EPS2) {/* Corrected according to AS 91; MM, May 25, 1999 */
	max_it_Newton = 20;
	goto END;/* and do Newton steps */
    }

    /* FIXME: This (cutoff to {0, +Inf}) is far from optimal
     * -----  when log_p or !lower_tail, but NOT doing it can be even worse */
    if(p_ > pMAX || p_ < pMIN) {
	/* did return ML_POSINF or 0.;	much better: */
	max_it_Newton = 20;
	goto END;/* and do Newton steps */
    }

#ifdef DEBUG_qgamma
    REprintf("\t==> ch = %10g:", ch);
#endif

/*----- Phase II: Iteration
 *	Call pgamma() [AS 239]	and calculate seven term taylor series
 */
    c = alpha-1;
    s6 = (120+c*(346+127*c)) * i5040; /* used below, is "const" */

    ch0 = ch;/* save initial approx. */
    for(i=1; i <= MAXIT; i++ ) {
	q = ch;
	p1 = 0.5*ch;
	p2 = p_ - pgamma_raw(p1, alpha, /*lower_tail*/TRUE, /*log_p*/FALSE);
#ifdef DEBUG_qgamma
	if(i == 1) REprintf(" Ph.II iter; ch=%g, p2=%g\n", ch, p2);
	if(i >= 2) REprintf("     it=%d,  ch=%g, p2=%g\n", i, ch, p2);
#endif
#ifdef IEEE_754
	if(!R_FINITE(p2) || ch <= 0)
#else
	if(errno != 0 || ch <= 0)
#endif
	    { ch = ch0; max_it_Newton = 27; goto END; }/*was  return ML_NAN;*/

	t = p2*exp(alpha*M_LN2+g+p1-c*log(ch));
	b = t/ch;
	a = 0.5*t - b*c;
	s1 = (210+ a*(140+a*(105+a*(84+a*(70+60*a))))) * i420;
	s2 = (420+ a*(735+a*(966+a*(1141+1278*a)))) * i2520;
	s3 = (210+ a*(462+a*(707+932*a))) * i2520;
	s4 = (252+ a*(672+1182*a) + c*(294+a*(889+1740*a))) * i5040;
	s5 = (84+2264*a + c*(1175+606*a)) * i2520;

	ch += t*(1+0.5*t*s1-b*c*(s1-b*(s2-b*(s3-b*(s4-b*(s5-b*s6))))));
	if(fabs(q - ch) < EPS2*ch)
	    goto END;
	if(fabs(q - ch) > 0.1*ch) {/* diverging? -- also forces ch > 0 */
	    if(ch < q) ch = 0.9 * q; else ch = 1.1 * q;
	}
    }
/* no convergence in MAXIT iterations -- but we add Newton now... */
#ifdef DEBUG_q
    MATHLIB_WARNING3("qgamma(%g) not converged in %d iterations; rel.ch=%g\n",
		     p, MAXIT, ch/fabs(q - ch));
#endif
/* was
 *    ML_ERROR(ME_PRECISION, "qgamma");
 * does nothing in R !*/

END:
/* PR# 2214 :	 From: Morten Welinder <terra@diku.dk>, Fri, 25 Oct 2002 16:50
   --------	 To: R-bugs@biostat.ku.dk     Subject: qgamma precision

   * With a final Newton step, double accuracy, e.g. for (p= 7e-4; nu= 0.9)
   *
   * Improved (MM): - only if rel.Err > EPS_N (= 1e-15);
   *		    - also for lower_tail = FALSE	 or log_p = TRUE
   * 		    - optionally *iterate* Newton
   */
    x = 0.5*scale*ch;
    if(max_it_Newton) {
	/* always use log scale */
	if (!log_p) {
	    p = log(p);
	    log_p = TRUE;
	}
	if(x == 0) {
	    const double _1_p = 1. + 1e-7;
	    const double _1_m = 1. - 1e-7;
	    x = DBL_MIN;
	    p_ = pgamma(x, alpha, scale, lower_tail, log_p);
	    if(( lower_tail && p_ > p * _1_p) ||
	       (!lower_tail && p_ < p * _1_m))
		return(0.);
	    /* else:  continue, using x = DBL_MIN instead of  0  */
	}
	else
	    p_ = pgamma(x, alpha, scale, lower_tail, log_p);
	if(p_ == ML_NEGINF) return 0; /* PR#14710 */
	for(i = 1; i <= max_it_Newton; i++) {
	    p1 = p_ - p;
#ifdef DEBUG_qgamma
	    if(i == 1) REprintf("\n it=%d: p=%g, x = %g, p.=%g; p1=d{p}=%g\n",
				i, p, x, p_, p1);
	    if(i >= 2) REprintf("          x{it= %d} = %g, p.=%g, p1=d{p}=%g\n",
				i,    x, p_, p1);
#endif
	    if(fabs(p1) < fabs(EPS_N * p))
		break;
	    /* else */
	    if((g = dgamma(x, alpha, scale, log_p)) == R_D__0) {
#ifdef DEBUG_q
		if(i == 1) REprintf("no final Newton step because dgamma(*)== 0!\n");
#endif
		break;
	    }
	    /* else :
	     * delta x = f(x)/f'(x);
	     * if(log_p) f(x) := log P(x) - p; f'(x) = d/dx log P(x) = P' / P
	     * ==> f(x)/f'(x) = f*P / P' = f*exp(p_) / P' (since p_ = log P(x))
	     */
	    t = log_p ? p1*exp(p_ - g) : p1/g ;/* = "delta x" */
	    t = lower_tail ? x - t : x + t;
	    p_ = pgamma (t, alpha, scale, lower_tail, log_p);
	    if (fabs(p_ - p) > fabs(p1) ||
		(i > 1 && fabs(p_ - p) == fabs(p1)) /* <- against flip-flop */) {
		/* no improvement */
#ifdef DEBUG_qgamma
		if(i == 1 && max_it_Newton > 1)
		    REprintf("no Newton step done since delta{p} >= last delta\n");
#endif
		break;
	    } /* else : */
#ifdef Harmful_notably_if_max_it_Newton_is_1
	    /* control step length: this could have started at
	       the initial approximation */
	    if(t > 1.1*x) t = 1.1*x;
	    else if(t < 0.9*x) t = 0.9*x;
#endif
	    x = t;
	}
    }

    return x;
}
