/* Based on C translation of ACM TOMS 708
   Please do not change this, e.g. to use R's versions of the
   ancillary routines, without investigating the error analysis as we
   do need very high relative accuracy.  This version has about
   14 digits accuracy.
*/

#undef min
#define min(a,b) ((a < b)?a:b)
#undef max
#define max(a,b) ((a > b)?a:b)

#include "nmath.h"
#include "dpq.h"
/* after config.h to avoid warning on Solaris */
#include <limits.h>
/* <math.h> is included by above, with suitable defines in glibc systems
   to make log1p and expm1 declared */

/**----------- DEBUGGING -------------
 *
 *	make CFLAGS='-DDEBUG_bratio  ...'
 *MM: (cd `R-devel RHOME`/src/nmath ; gcc -std=gnu99 -I. -I../../src/include -I../../../R/src/include -I/usr/local/include -DHAVE_CONFIG_H -DDEBUG_bratio -g  -c ../../../R/src/nmath/toms708.c -o toms708.o; cd ../..; make R)
*/
#ifdef DEBUG_bratio
# include <R_ext/PrtUtil.h>
#endif

/* MM added R_D_LExp, so redefine here in terms of rexpm1 */
#undef R_Log1_Exp
#define R_Log1_Exp(x)   ((x) > -M_LN2 ? log(-rexpm1(x)) : log1p(-exp(x)))


static double bfrac(double, double, double, double, double, double, int log_p);
static void bgrat(double, double, double, double, double *, double, int *);
static void grat1(double, double, double, double *, double *, double);
static double apser(double, double, double, double);
static double bpser(double, double, double, double, int log_p);
static double basym(double, double, double, double, int log_p);
static double fpser(double, double, double, double, int log_p);
static double bup(double, double, double, double, int, double);
static double exparg(int);
static double psi(double);
static double gam1(double);
static double gamln1(double);
static double betaln(double, double);
static double algdiv(double, double);
static double brcmp1(int, double, double, double, double);
static double brcomp(double, double, double, double, int log_p);
static double rlog1(double);
static double bcorr(double, double);
static double gamln(double);
static double alnrel(double);
static double esum(int, double);
static double erf__(double);
static double rexpm1(double);
static double erfc1(int, double);
static double gsumln(double, double);

/*      ALGORITHM 708, COLLECTED ALGORITHMS FROM ACM.
 *      This work published in  Transactions On Mathematical Software,
 *      vol. 18, no. 3, September 1992, pp. 360-373z.
 */

/* Changes by R Core Team :
 * add log_p  and work towards gaining precision in that case
 */

void attribute_hidden
bratio(double a, double b, double x, double y, double *w, double *w1,
       int *ierr, int log_p)
{
/* -----------------------------------------------------------------------

 *	      Evaluation of the Incomplete Beta function I_x(a,b)

 *		       --------------------

 *     It is assumed that a and b are nonnegative, and that x <= 1
 *     and y = 1 - x.  Bratio assigns w and w1 the values

 *			w  = I_x(a,b)
 *			w1 = 1 - I_x(a,b)

 *     ierr is a variable that reports the status of the results.
 *     If no input errors are detected then ierr is set to 0 and
 *     w and w1 are computed. otherwise, if an error is detected,
 *     then w and w1 are assigned the value 0 and ierr is set to
 *     one of the following values ...

 *	  ierr = 1  if a or b is negative
 *	  ierr = 2  if a = b = 0
 *	  ierr = 3  if x < 0 or x > 1
 *	  ierr = 4  if y < 0 or y > 1
 *	  ierr = 5  if x + y != 1
 *	  ierr = 6  if x = a = 0
 *	  ierr = 7  if y = b = 0
 *	  ierr = 8  "error" in bgrat()

 * --------------------
 *     Written by Alfred H. Morris, Jr.
 *	  Naval Surface Warfare Center
 *	  Dahlgren, Virginia
 *     Revised ... Nov 1991
* ----------------------------------------------------------------------- */

    Rboolean do_swap;
    int n, ierr1 = 0;
    double z, a0, b0, x0, y0, eps, lambda;

/*  eps is a machine dependent constant: the smallest
 *      floating point number for which   1.0 + eps > 1.0 */
    eps = 2.0 * Rf_d1mach(3); /* == DBL_EPSILON (in R, Rmath) */

/* ----------------------------------------------------------------------- */
    *w  = R_D__0;
    *w1 = R_D__0;

    if (a < 0.0 || b < 0.0)   { *ierr = 1; return; }
    if (a == 0.0 && b == 0.0) { *ierr = 2; return; }
    if (x < 0.0 || x > 1.0)   {	*ierr = 3; return; }
    if (y < 0.0 || y > 1.0)   { *ierr = 4; return; }

    /* check that  'y == 1 - x' : */
    z = x + y - 0.5 - 0.5;

    if (fabs(z) > eps * 3.0) { *ierr = 5; return; }

#ifdef DEBUG_bratio
    REprintf("bratio(a=%g, b=%g, x=%9g, y=%9g, .., log_p=%d): ", a,b,x,y, log_p);
#endif

    *ierr = 0;
    if (x == 0.0) goto L200;
    if (y == 0.0) goto L210;

    if (a == 0.0) goto L211;
    if (b == 0.0) goto L201;

    eps = max(eps, 1e-15);
    if (max(a,b) < eps * .001) { /* procedure for a and b < 0.001 * eps */
	/* L230: */
	if(log_p) {
	    z = log(a + b);
	    *w	= log(b) - z;
	    *w1 = log(a) - z;
	} else {
	    *w	= b / (a + b);
	    *w1 = a / (a + b);
	}
	return;
    }

#define SET_0_noswap \
    a0 = a;  x0 = x; \
    b0 = b;  y0 = y;

#define SET_0_swap   \
    a0 = b;  x0 = y; \
    b0 = a;  y0 = x;

    if (min(a,b) <= 1.) { /*------------------------ a <= 1  or  b <= 1 ---- */

	do_swap = (x > 0.5);
	if (do_swap) {
	    SET_0_swap;
	} else {
	    SET_0_noswap;
	}
	/* now have  x0 <= 1/2 <= y0  (still  x0+y0 == 1) */

#ifdef DEBUG_bratio
	REprintf("  min(a,b) <= 1 : do_swap = %d; ", do_swap);
#endif

	if (b0 < min(eps, eps * a0)) { /* L80: */
	    *w = fpser(a0, b0, x0, eps, log_p);
	    *w1 = log_p ? R_Log1_Exp(*w) : 0.5 - *w + 0.5;
#ifdef DEBUG_bratio
	    REprintf("  b0 small -> w := fpser(*) = %15g\n", *w);
#endif
	    goto L_end_after_log;
	}

	if (a0 < min(eps, eps * b0) && b0 * x0 <= 1.0) { /* L90: */
	    *w1 = apser(a0, b0, x0, eps);
#ifdef DEBUG_bratio
	    REprintf("  a0 small -> w1 := apser(*) = %15g\n", *w1);
#endif
	    goto L_end_from_w1;
	}

	if (max(a0,b0) > 1.0) { /* L20:  min(a,b) <= 1 < max(a,b)  */
#ifdef DEBUG_bratio
	    REprintf("  L20:  min(a,b) <= 1 < max(a,b); ");
#endif
	    if (b0 <= 1.0) goto L100;

	    if (x0 >= 0.29) /* was 0.3, PR#13786 */	goto L110;

	    if (x0 < 0.1) {
		if (pow(x0*b0, a0) <= 0.7) {
		    goto L100;
		}
	    }
	    if (b0 > 15.0) {
		*w1 = 0.;
		goto L131;
	    }
	} else { /*  a, b <= 1 */
#ifdef DEBUG_bratio
	    REprintf("  both a,b <= 1; ");
#endif
	    if (a0 >= min(0.2, b0))	goto L100;

	    if (pow(x0, a0) <= 0.9) goto L100;

	    if (x0 >= 0.3)		goto L110;
	}
	n = 20; /* goto L130; */
	*w1 = bup(b0, a0, y0, x0, n, eps);
#ifdef DEBUG_bratio
	REprintf("  ... n=20 and *w1 := bup(*) = %15g; ");
#endif
	b0 += n;
    L131:
	bgrat(b0, a0, y0, x0, w1, 15*eps, &ierr1);

#ifdef DEBUG_bratio
	REprintf(" L131: bgrat(*, w1) ==> w1 = %15g\n", *w1);
#endif
	goto L_end_from_w1;
    }
    else { /* L30: -------------------- both  a, b > 1  {a0 > 1  &  b0 > 1} ---*/

	if (a > b)
	    lambda = (a + b) * y - b;
	else
	    lambda = a - (a + b) * x;

	do_swap = (lambda < 0.0);
	if (do_swap) {
	    lambda = -lambda;
	    SET_0_swap;
	} else {
	    SET_0_noswap;
	}

#ifdef DEBUG_bratio
	REprintf("  L30:  both  a, b > 1; |lambda| = %#g, do_swap = %d\n",
		 lambda, do_swap);
#endif

	if (b0 < 40.0) {
#ifdef DEBUG_bratio
	    REprintf("  b0 < 40; ");
#endif
	    if (b0 * x0 <= 0.7
		|| (log_p && lambda > 650.)) /* << added 2010-03-18 */
		goto L100;
	    else
		goto L140;
	}
	else if (a0 > b0) { /* ----  a0 > b0 >= 40  ---- */
#ifdef DEBUG_bratio
	    REprintf("  a0 > b0 >= 40; ");
#endif
	    if (b0 <= 100.0 || lambda > b0 * 0.03)
		goto L120;

	} else if (a0 <= 100.0) {
#ifdef DEBUG_bratio
	    REprintf("  a0 <= 100; a0 <= b0 >= 40; ");
#endif
	    goto L120;
	}
	else if (lambda > a0 * 0.03) {
#ifdef DEBUG_bratio
	    REprintf("  b0 >= a0 > 100; lambda > a0 * 0.03 ");
#endif
	    goto L120;
	}

	/* else if none of the above    L180: */
	*w = basym(a0, b0, lambda, eps * 100.0, log_p);
	*w1 = log_p ? R_Log1_Exp(*w) : 0.5 - *w + 0.5;
#ifdef DEBUG_bratio
	REprintf("  b0 >= a0 > 100; lambda <= a0 * 0.03: *w := basym(*) = %15g\n",
		 *w);
#endif
	goto L_end_after_log;

    } /* else: a, b > 1 */

/*            EVALUATION OF THE APPROPRIATE ALGORITHM */

L100:
    *w = bpser(a0, b0, x0, eps, log_p);
    *w1 = log_p ? R_Log1_Exp(*w) : 0.5 - *w + 0.5;
#ifdef DEBUG_bratio
    REprintf(" L100: *w := bpser(*) = %15g\n", *w);
#endif
    goto L_end_after_log;

L110:
    *w1 = bpser(b0, a0, y0, eps, log_p);
    *w  = log_p ? R_Log1_Exp(*w1) : 0.5 - *w1 + 0.5;
#ifdef DEBUG_bratio
    REprintf(" L110: *w1 := bpser(*) = %15g\n", *w1);
#endif
    goto L_end_after_log;

L120:
    *w = bfrac(a0, b0, x0, y0, lambda, eps * 15.0, log_p);
    *w1 = log_p ? R_Log1_Exp(*w) : 0.5 - *w + 0.5;
#ifdef DEBUG_bratio
    REprintf(" L120: *w := bfrac(*) = %g\n", *w);
#endif
    goto L_end_after_log;

L140:
    /* b0 := fractional_part( b0 )  in (0, 1]  */
    n = (int) b0;
    b0 -= n;
    if (b0 == 0.) {
	--n; b0 = 1.;
    }

    *w = bup(b0, a0, y0, x0, n, eps);

#ifdef DEBUG_bratio
    REprintf(" L140: *w := bup(b0=%g, *) = %15g; ", b0, *w);
#endif
    if(*w < DBL_MIN && log_p) { /* do not believe it; try bpser() : */
	/*revert: */ b0 += n;
	/* which is only valid if b0 <= 1 || b0*x0 <= 0.7 */
	goto L100;
    }
    if (x0 <= 0.7) {
	/* log_p :  TODO:  w = bup(.) + bpser(.)  -- not so easy to use log-scale */
	*w += bpser(a0, b0, x0, eps, /* log_p = */ FALSE);
#ifdef DEBUG_bratio
	REprintf(" x0 <= 0.7: *w := *w + bpser(*) = %15g\n", *w);
#endif
	goto L_end_from_w;
    }
    /* L150: */
    if (a0 <= 15.0) {
	n = 20;
	*w += bup(a0, b0, x0, y0, n, eps);
#ifdef DEBUG_bratio
	REprintf("\n a0 <= 15: *w := *w + bup(*) = %15g;", *w);
#endif
	a0 += n;
    }
    bgrat(a0, b0, x0, y0, w, 15*eps, &ierr1);
#ifdef DEBUG_bratio
    REprintf(" bgrat(*) ==> *w = %g\n", *w);
#endif
    goto L_end_from_w;


/* TERMINATION OF THE PROCEDURE */

L200:
    if (a == 0.0) { *ierr = 6;    return; }
L201:
    *w  = R_D__0;
    *w1 = R_D__1;
    return;

L210:
    if (b == 0.0) { *ierr = 7;    return; }
L211:
    *w  = R_D__1;
    *w1 = R_D__0;
    return;

L_end_from_w:
    if(log_p) {
	*w1 = log1p(-*w);
	*w  = log(*w);
    } else {
	*w1 = 0.5 - *w + 0.5;
    }
    goto L_end_after_log;

L_end_from_w1:
    if(log_p) {
	*w  = log1p(-*w1);
	*w1 = log(*w1);
    } else {
	*w = 0.5 - *w1 + 0.5;
    }

L_end_after_log:
    if (do_swap) { /* swap */
	double t = *w; *w = *w1; *w1 = t;
    }
    return;

} /* bratio */

#undef SET_0_noswap
#undef SET_0_swap

double fpser(double a, double b, double x, double eps, int log_p)
{
/* ----------------------------------------------------------------------- *

 *                 EVALUATION OF I (A,B)
 *                                X

 *          FOR B < MIN(EPS, EPS*A) AND X <= 0.5

 * ----------------------------------------------------------------------- */

    double ans, c, s, t, an, tol;

    /* SET  ans := x^a : */
    if (log_p) {
	ans = a * log(x);
    } else if (a > eps * 0.001) {
	t = a * log(x);
	if (t < exparg(1)) { /* exp(t) would underflow */
	    return 0.0;
	}
	ans = exp(t);
    } else
	ans = 1.;

/*                NOTE THAT 1/B(A,B) = B */

    if (log_p)
	ans += log(b) - log(a);
    else
	ans *= b / a;

    tol = eps / a;
    an = a + 1.0;
    t = x;
    s = t / an;
    do {
	an += 1.0;
	t = x * t;
	c = t / an;
	s += c;
    } while (fabs(c) > tol);

    if (log_p)
	ans += log1p(a * s);
    else
	ans *= a * s + 1.0;
    return ans;
} /* fpser */

static double apser(double a, double b, double x, double eps)
{
/* -----------------------------------------------------------------------
 *     apser() yields the incomplete beta ratio  I_{1-x}(b,a)  for
 *     a <= min(eps,eps*b), b*x <= 1, and x <= 0.5,  i.e., a is very small.
 *     Use only if above inequalities are satisfied.
 * ----------------------------------------------------------------------- */

    static double const g = .577215664901533;

    double tol, c, j, s, t, aj;
    double bx = b * x;

    t = x - bx;
    if (b * eps <= 0.02)
	c = log(x) + psi(b) + g + t;
    else
	c = log(bx) + g + t;

    tol = eps * 5.0 * fabs(c);
    j = 1.;
    s = 0.;
    do {
	j += 1.0;
	t *= x - bx / j;
	aj = t / j;
	s += aj;
    } while (fabs(aj) > tol);

    return -a * (c + s);
} /* apser */

static double bpser(double a, double b, double x, double eps, int log_p)
{
/* -----------------------------------------------------------------------
 * Power SERies expansion for evaluating I_x(a,b) when
 *	       b <= 1 or b*x <= 0.7.   eps is the tolerance used.
 * ----------------------------------------------------------------------- */

    int i, m;
    double ans, c, n, t, u, w, z, a0, b0, apb, tol, sum;

    if (x == 0.) {
	return R_D__0;
    }
/* ----------------------------------------------------------------------- */
/*	      compute the factor  x^a/(a*Beta(a,b)) */
/* ----------------------------------------------------------------------- */
    a0 = min(a,b);
    if (a0 >= 1.0) { /*		 ------	 1 <= a0 <= b0  ------ */
	z = a * log(x) - betaln(a, b);
	ans = log_p ? z - log(a) : exp(z) / a;
    }
    else {
	b0 = max(a,b);

	if (b0 < 8.0) {

	    if (b0 <= 1.0) { /*	 ------	 a0 < 1	 and  b0 <= 1  ------ */

		if(log_p) {
		    ans = a * log(x);
		} else {
		    ans = pow(x, a);
		    if (ans == 0.) /* once underflow, always underflow .. */
			return ans;
		}
		apb = a + b;
		if (apb > 1.0) {
		    u = a + b - 1.;
		    z = (gam1(u) + 1.0) / apb;
		} else {
		    z = gam1(apb) + 1.0;
		}
		c = (gam1(a) + 1.0) * (gam1(b) + 1.0) / z;

		if(log_p) /* FIXME ? -- improve quite a bit for c ~= 1 */
		    ans += log(c * (b / apb));
		else
		    ans *=  c * (b / apb);

	    } else { /* 	------	a0 < 1 < b0 < 8	 ------ */

		u = gamln1(a0);
		m = b0 - 1.0;
		if (m >= 1) {
		    c = 1.0;
		    for (i = 1; i <= m; ++i) {
			b0 += -1.0;
			c *= b0 / (a0 + b0);
		    }
		    u += log(c);
		}

		z = a * log(x) - u;
		b0 += -1.0;
		apb = a0 + b0;
		if (apb > 1.0) {
		    u = a0 + b0 - 1.;
		    t = (gam1(u) + 1.0) / apb;
		} else {
		    t = gam1(apb) + 1.0;
		}

		if(log_p) /* FIXME? potential for improving log(t) */
		    ans = z + log(a0 / a) + log1p(gam1(b0)) - log(t);
		else
		    ans = exp(z) * (a0 / a) * (gam1(b0) + 1.0) / t;
	    }

	} else { /* 		------  a0 < 1 < 8 <= b0  ------ */

	    u = gamln1(a0) + algdiv(a0, b0);
	    z = a * log(x) - u;

	    if(log_p)
		ans = z + log(a0 / a);
	    else
		ans = a0 / a * exp(z);
	}
    }

    if (!log_p && (ans == 0.0 || a <= eps * 0.1)) {
	return ans;
    }

/* ----------------------------------------------------------------------- */
/*		       COMPUTE THE SERIES */
/* ----------------------------------------------------------------------- */
    sum = 0.;
    n = 0.;
    c = 1.;
    tol = eps / a;

    do {
	n += 1.;
	c *= (0.5 - b / n + 0.5) * x;
	w = c / (a + n);
	sum += w;
    } while (fabs(w) > tol);

    if(log_p) {
	if (a*sum > -1.0) ans += log1p(a * sum);
	else ans = ML_NEGINF;
    } else
	ans *= a * sum + 1.0;
    return ans;
} /* bpser */

static double bup(double a, double b, double x, double y, int n, double eps)
{
/* ----------------------------------------------------------------------- */
/*     EVALUATION OF I_x(A,B) - I_x(A+N,B) WHERE N IS A POSITIVE INT. */
/*     EPS IS THE TOLERANCE USED. */
/* ----------------------------------------------------------------------- */

    /* System generated locals */
    double ret_val;

    /* Local variables */
    int i, k, mu, nm1;
    double d, l, r, t, w;
    double ap1, apb;

/*          OBTAIN THE SCALING FACTOR EXP(-MU) AND */
/*             EXP(MU)*(X^A * Y^B / BETA(A,B))/A */

    apb = a + b;
    ap1 = a + 1.0;
    if (n > 1 && a >= 1. && apb >= ap1 * 1.1) {
	mu = fabs(exparg(1));
	k = (int) exparg(0);
	if (k < mu) {
	    mu = k;
	}
	t = (double) mu;
	d = exp(-t);
    }
    else {
	mu = 0;
	d = 1.0;
    }

    /* L10: */
    ret_val = brcmp1(mu, a, b, x, y) / a;
    if (n == 1 || ret_val == 0.0) {
	return ret_val;
    }
    nm1 = n - 1;
    w = d;

/*          LET K BE THE INDEX OF THE MAXIMUM TERM */

    k = 0;
    if (b <= 1.0) {
	goto L40;
    }
    if (y > 1e-4) {
	r = (b - 1.0) * x / y - a;
	if (r < 1.0) {
	    goto L40;
	}
	k = nm1;
	t = (double) nm1;
	if (r < t) {
	    k = (int) r;
	}
    } else {
	k = nm1;
    }

/*          ADD THE INCREASING TERMS OF THE SERIES */

/* L30: */
    for (i = 1; i <= k; ++i) {
	l = (double) (i - 1);
	d = (apb + l) / (ap1 + l) * x * d;
	w += d;
/* L31: */
    }
    if (k == nm1) {
	goto L50;
    }

/*          ADD THE REMAINING TERMS OF THE SERIES */

L40:
    for (i = k+1; i <= nm1; ++i) {
	l = (double) (i - 1);
	d = (apb + l) / (ap1 + l) * x * d;
	w += d;
	if (d <= eps * w) /* relativ convergence (eps) */
	    break;
    }

/*               TERMINATE THE PROCEDURE */

L50:
    ret_val *= w;
    return ret_val;
} /* bup */

static double bfrac(double a, double b, double x, double y, double lambda,
		    double eps, int log_p)
{
/* -----------------------------------------------------------------------
       Continued fraction expansion for I_x(a,b) when a, b > 1.
       It is assumed that  lambda = (a + b)*y - b.
   -----------------------------------------------------------------------*/

    double c, e, n, p, r, s, t, w, c0, c1, r0, an, bn, yp1, anp1, bnp1,
	beta, alpha;

    double brc = brcomp(a, b, x, y, log_p);

    if (!log_p && brc == 0.) /* already underflowed to 0 */
	return 0.;

    c = lambda + 1.0;
    c0 = b / a;
    c1 = 1.0 / a + 1.0;
    yp1 = y + 1.0;

    n = 0.0;
    p = 1.0;
    s = a + 1.0;
    an = 0.0;
    bn = 1.0;
    anp1 = 1.0;
    bnp1 = c / c1;
    r = c1 / c;

/*        CONTINUED FRACTION CALCULATION */

    do {
	n += 1.0;
	t = n / a;
	w = n * (b - n) * x;
	e = a / s;
	alpha = p * (p + c0) * e * e * (w * x);
	e = (t + 1.0) / (c1 + t + t);
	beta = n + w / s + e * (c + n * yp1);
	p = t + 1.0;
	s += 2.0;

	/* update an, bn, anp1, and bnp1 */

	t = alpha * an + beta * anp1;
	an = anp1;
	anp1 = t;
	t = alpha * bn + beta * bnp1;
	bn = bnp1;
	bnp1 = t;

	r0 = r;
	r = anp1 / bnp1;
	if (fabs(r - r0) <= eps * r) {
	    break;
	}

	/* rescale an, bn, anp1, and bnp1 */

	an /= bnp1;
	bn /= bnp1;
	anp1 = r;
	bnp1 = 1.0;
    } while (1);

    return (log_p ? brc + log(r) : brc * r);
} /* bfrac */

static double brcomp(double a, double b, double x, double y, int log_p)
{
/* -----------------------------------------------------------------------
 *		 Evaluation of x^a * y^b / Beta(a,b)
 * ----------------------------------------------------------------------- */

    static double const__ = .398942280401433; /* == 1/sqrt(2*pi); */
    /* R has  M_1_SQRT_2PI , and M_LN_SQRT_2PI = ln(sqrt(2*pi)) = 0.918938.. */

    int i, n;
    double c, e, h, t, u, v, z, a0, b0, x0, y0, apb, lnx, lny;
    double lambda;


    if (x == 0.0 || y == 0.0) {
	return R_D__0;
    }
    a0 = min(a, b);
    if (a0 >= 8.0) {
	goto L100;
    }

    if (x <= .375) {
	lnx = log(x);
	lny = alnrel(-x);
    }
    else {
	if (y > .375) {
	    lnx = log(x);
	    lny = log(y);
	} else {
	    lnx = alnrel(-y);
	    lny = log(y);
	}
    }

    z = a * lnx + b * lny;
    if (a0 >= 1.) {
	z -= betaln(a, b);
	return R_D_exp(z);
    }

/* ----------------------------------------------------------------------- */
/*		PROCEDURE FOR a < 1 OR b < 1 */
/* ----------------------------------------------------------------------- */

    b0 = max(a, b);
    if (b0 >= 8.0) { /* L80: */
	u = gamln1(a0) + algdiv(a0, b0);

	return (log_p ? log(a0) + (z - u)  : a0 * exp(z - u));
    }
    /* else : */

    if (b0 <= 1.0) { /*		algorithm for max(a,b) = b0 <= 1 */

	double e_z = R_D_exp(z);

	if (!log_p && e_z == 0.0) /* exp() underflow */
	    return 0.;

	apb = a + b;
	if (apb > 1.0) {
	    u = a + b - 1.;
	    z = (gam1(u) + 1.0) / apb;
	} else {
	    z = gam1(apb) + 1.0;
	}

	c = (gam1(a) + 1.0) * (gam1(b) + 1.0) / z;
	/* FIXME? log(a0*c)= log(a0)+ log(c) and that is improvable */
	return (log_p
		? e_z + log(a0 * c) - log1p(a0/b0)
		: e_z * (a0 * c) / (a0 / b0 + 1.0));
    }
    /* else : */

/*		  ALGORITHM FOR 1 < b0 < 8 */

    u = gamln1(a0);
    n = b0 - 1.0;
    if (n >= 1) {
	c = 1.0;
	for (i = 1; i <= n; ++i) {
	    b0 += -1.0;
	    c *= b0 / (a0 + b0);
	}
	u = log(c) + u;
    }
    z -= u;
    b0 += -1.0;
    apb = a0 + b0;
    if (apb > 1.0) {
	u = a0 + b0 - 1.;
	t = (gam1(u) + 1.0) / apb;
    } else {
	t = gam1(apb) + 1.0;
    }

    return (log_p
	    ? log(a0) + z + log1p(gam1(b0))  - log(t)
	    : a0 * exp(z) * (gam1(b0) + 1.0) / t);



/* ----------------------------------------------------------------------- */
/*		PROCEDURE FOR A >= 8 AND B >= 8 */
/* ----------------------------------------------------------------------- */
L100:
    if (a <= b) {
	h = a / b;
	x0 = h / (h + 1.0);
	y0 = 1.0 / (h + 1.0);
	lambda = a - (a + b) * x;
    } else {
	h = b / a;
	x0 = 1.0 / (h + 1.0);
	y0 = h / (h + 1.0);
	lambda = (a + b) * y - b;
    }

    e = -lambda / a;
    if (fabs(e) > .6)
	u = e - log(x / x0);
    else
	u = rlog1(e);

    e = lambda / b;
    if (fabs(e) <= .6)
	v = rlog1(e);
    else
	v = e - log(y / y0);

    z = log_p ? -(a * u + b * v) : exp(-(a * u + b * v));

    return(log_p
	   ? -M_LN_SQRT_2PI + .5*log(b * x0) + z - bcorr(a,b)
	   : const__ * sqrt(b * x0) * z * exp(-bcorr(a, b)));
} /* brcomp */

static double brcmp1(int mu, double a, double b, double x, double y)
{
/* -----------------------------------------------------------------------
 *          EVALUATION OF  EXP(MU) * (X^A * Y^B / BETA(A,B))
 * ----------------------------------------------------------------------- */

    static double const__ = .398942280401433; /* == 1/sqrt(2*pi); */
    /* R has  M_1_SQRT_2PI */

    /* System generated locals */
    double ret_val, r1;

    /* Local variables */
    double c, e, h;
    int i, n;
    double t, u, v, z, a0, b0, x0, y0, apb, lnx, lny;
    double lambda;

    a0 = min(a,b);
    if (a0 >= 8.0) {
	goto L100;
    }

    if (x > .375) {
	goto L10;
    }
    lnx = log(x);
    lny = alnrel(-x);
    goto L20;
L10:
    if (y > .375) {
	goto L11;
    }
    lnx = alnrel(-y);
    lny = log(y);
    goto L20;
L11:
    lnx = log(x);
    lny = log(y);

L20:
    z = a * lnx + b * lny;
    if (a0 < 1.0) {
	goto L30;
    }
    z -= betaln(a, b);
    ret_val = esum(mu, z);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A < 1 OR B < 1 */
/* ----------------------------------------------------------------------- */
L30:
    b0 = max(a,b);
    if (b0 >= 8.0) {
	goto L80;
    }
    if (b0 > 1.0) {
	goto L60;
    }

/*                   ALGORITHM FOR b0 <= 1 */

    ret_val = esum(mu, z);
    if (ret_val == 0.0) {
	return ret_val;
    }

    apb = a + b;
    if (apb > 1.0) {
	goto L40;
    }
    z = gam1(apb) + 1.0;
    goto L50;
L40:
    u = a + b - 1.;
    z = (gam1(u) + 1.0) / apb;

L50:
    c = (gam1(a) + 1.0) * (gam1(b) + 1.0) / z;
    ret_val = ret_val * (a0 * c) / (a0 / b0 + 1.0);
    return ret_val;

/*                ALGORITHM FOR 1 < b0 < 8 */

L60:
    u = gamln1(a0);
    n = b0 - 1.0;
    if (n < 1) {
	goto L70;
    }
    c = 1.0;
    for (i = 1; i <= n; ++i) {
	b0 += -1.0;
	c *= b0 / (a0 + b0);
/* L61: */
    }
    u = log(c) + u;

L70:
    z -= u;
    b0 += -1.0;
    apb = a0 + b0;
    if (apb > 1.0) {
	goto L71;
    }
    t = gam1(apb) + 1.0;
    goto L72;
L71:
    u = a0 + b0 - 1.;
    t = (gam1(u) + 1.0) / apb;
L72:
    ret_val = a0 * esum(mu, z) * (gam1(b0) + 1.0) / t;
    return ret_val;

/*                   ALGORITHM FOR b0 >= 8 */

L80:
    u = gamln1(a0) + algdiv(a0, b0);
    ret_val = a0 * esum(mu, z - u);
    return ret_val;
/* ----------------------------------------------------------------------- */
/*              PROCEDURE FOR A >= 8 AND B >= 8 */
/* ----------------------------------------------------------------------- */
L100:
    if (a > b) {
	goto L101;
    }
    h = a / b;
    x0 = h / (h + 1.0);
    y0 = 1.0 / (h + 1.0);
    lambda = a - (a + b) * x;
    goto L110;
L101:
    h = b / a;
    x0 = 1.0 / (h + 1.0);
    y0 = h / (h + 1.0);
    lambda = (a + b) * y - b;

L110:
    e = -lambda / a;
    if (fabs(e) > 0.6) {
	goto L111;
    }
    u = rlog1(e);
    goto L120;
L111:
    u = e - log(x / x0);

L120:
    e = lambda / b;
    if (fabs(e) > 0.6) {
	goto L121;
    }
    v = rlog1(e);
    goto L130;
L121:
    v = e - log(y / y0);

L130:
    r1 = -(a * u + b * v);
    z = esum(mu, r1);

    return const__ * sqrt(b * x0) * z * exp(-bcorr(a, b));

} /* brcmp1 */

static void bgrat(double a, double b, double x, double y, double *w,
		  double eps, int *ierr)
{
/* -----------------------------------------------------------------------
*     Asymptotic Expansion for I_x(a,b)  when a is larger than b.
*     The result of the expansion is added to w.
*     It is assumed a >= 15 and b <= 1.
*     eps is the tolerance used.
*     ierr is a variable that reports the status of the results.
* ----------------------------------------------------------------------- */

    double c[30], d[30];
    int i, n, nm1;
    double j, l, p, q, r, s, t, u, v, z, n2, t2, dj, cn, nu, bm1;
    double lnx, sum, bp2n, coef;

    bm1 = b - 0.5 - 0.5;
    nu = a + bm1 * 0.5;
    if (y > 0.375)
	lnx = log(x);
    else
	lnx = alnrel(-y);

    z = -nu * lnx;
    if (b * z == 0.0) goto L_Error; /* should *never* happen */

/*                 COMPUTATION OF THE EXPANSION */

/* set r := exp(-z) * z^b / Gamma(b) */
    r = b * (gam1(b) + 1.0) * exp(b * log(z));

    r = r * exp(a * lnx) * exp(bm1 * 0.5 * lnx);
    u = algdiv(b, a) + b * log(nu);
    u = r * exp(-u);
    if (u == 0.0) {
#ifdef DEBUG_bratio
	REprintf(" bgrat(*) *underflow* r = %g ", r);
#endif
	goto L_Error;
    }
    grat1(b, z, r, &p, &q, eps); /* -> (p,q)  {p + q = 1} */

    v = 0.25 / (nu * nu);
    t2 = lnx * 0.25 * lnx;
    l = *w / u;
    j = q / r;
    sum = j;
    t = 1.0;
    cn = 1.0;
    n2 = 0.0;
    for (n = 1; n <= 30; ++n) {
	bp2n = b + n2;
	j = (bp2n * (bp2n + 1.0) * j + (z + bp2n + 1.0) * t) * v;
	n2 += 2.0;
	t *= t2;
	cn /= n2 * (n2 + 1.0);
	nm1 = n - 1;
	c[nm1] = cn;
	s = 0.0;
	if (n > 1) {
	    coef = b - n;
	    for (i = 1; i <= nm1; ++i) {
		s += coef * c[i - 1] * d[nm1 - i];
		coef += b;
	    }
	}
	d[nm1] = bm1 * cn + s / n;
	dj = d[nm1] * j;
	sum += dj;
	if (sum <= 0.0) {
	    goto L_Error;
	}
	if (fabs(dj) <= eps * (sum + l)) {
	    break;
	}
    }

/*                    ADD THE RESULTS TO W */
    *ierr = 0;
    *w += u * sum;
    return;

/*               THE EXPANSION CANNOT BE COMPUTED */

L_Error:
    *ierr = 1;
    return;
} /* bgrat */

static void grat1(double a, double x, double r, double *p, double *q,
		  double eps)
{
/* -----------------------------------------------------------------------
*        Evaluation of the incomplete gamma ratio functions
*                      P(a,x) and Q(a,x)

*     It is assumed that a <= 1.  eps is the tolerance to be used.
*     the input argument r has the value  r = e^(-x)* x^a / Gamma(a).
* ----------------------------------------------------------------------- */

    double c, g, h, j, l, t, w, z, an, am0, an0, a2n, b2n, cma;
    double tol, sum, a2nm1, b2nm1;

    if (a * x == 0.0) { /* L130: */
	if (x <= a)
	    goto L100;
	else
	    goto L110;
    }
    else if (a == 0.5) {
	goto L120;
    }

    if (x < 1.1) { /* L10:  Taylor series for  P(a,x)/x^a */

	an = 3.0;
	c = x;
	sum = x / (a + 3.0);
	tol = eps * 0.1 / (a + 1.0);
	do {
	    an += 1.0;
	    c = -c * (x / an);
	    t = c / (a + an);
	    sum += t;
	} while (fabs(t) > tol);

	j = a * x * ((sum / 6.0 - 0.5 / (a + 2.0)) * x + 1.0 / (a + 1.0));

	z = a * log(x);
	h = gam1(a);
	g = h + 1.0;
	if (x >= 0.25) {
	    if (a < x / 2.59) {
		goto L40;
	    }
	}
	else {
	    if (z > -0.13394) {
		goto L40;
	    }
	}

	w = exp(z);
	*p = w * g * (0.5 - j + 0.5);
	*q = 0.5 - *p + 0.5;
	return;

    L40:
	l = rexpm1(z);
	w = l + 0.5 + 0.5;
	*q = (w * j - l) * g - h;
	if (*q < 0.0) {
	    goto L110;
	}
	*p = 0.5 - *q + 0.5;
	return;

    }

/* L50: ----  (x >= 1.1)  ---- Continued Fraction Expansion */

    a2nm1 = 1.0;
    a2n = 1.0;
    b2nm1 = x;
    b2n = x + (1.0 - a);
    c = 1.0;

    do {
	a2nm1 = x * a2n + c * a2nm1;
	b2nm1 = x * b2n + c * b2nm1;
	am0 = a2nm1 / b2nm1;
	c += 1.0;
	cma = c - a;
	a2n = a2nm1 + cma * a2n;
	b2n = b2nm1 + cma * b2n;
	an0 = a2n / b2n;
    } while (fabs(an0 - am0) >= eps * an0);

    *q = r * an0;
    *p = 0.5 - *q + 0.5;
    return;

/*                SPECIAL CASES */

L100:
    *p = 0.0;
    *q = 1.0;
    return;

L110:
    *p = 1.0;
    *q = 0.0;
    return;

L120:
    if (x < 0.25) {
	*p = erf__(sqrt(x));
	*q = 0.5 - *p + 0.5;
    } else {
	*q = erfc1(0, sqrt(x));
	*p = 0.5 - *q + 0.5;
    }
    return;

} /* grat1 */

static double basym(double a, double b, double lambda, double eps, int log_p)
{
/* ----------------------------------------------------------------------- */
/*     ASYMPTOTIC EXPANSION FOR I_x(A,B) FOR LARGE A AND B. */
/*     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED. */
/*     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT */
/*     A AND B ARE GREATER THAN OR EQUAL TO 15. */
/* ----------------------------------------------------------------------- */


/* ------------------------ */
/*     ****** NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP */
/*            ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN. */
#define num_IT 20
/*            THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1. */

    static double const e0 = 1.12837916709551;/* e0 == 2/sqrt(pi) */
    static double const e1 = .353553390593274;/* e1 == 2^(-3/2)   */
    static double const ln_e0 = 0.120782237635245; /* == ln(e0) */

    double a0[num_IT + 1], b0[num_IT + 1], c[num_IT + 1], d[num_IT + 1];
    double f, h, r, s, t, u, w, z, j0, j1, h2, r0, r1, t0, t1, w0, z0, z2, hn, zn;
    double sum, znm1, bsum, dsum;

    int i, j, m, n, im1, mm1, np1, imj, mmj;

/* ------------------------ */

    f = a * rlog1(-lambda/a) + b * rlog1(lambda/b);
    if(log_p)
	t = -f;
    else {
	t = exp(-f);
	if (t == 0.0) {
	    return 0; /* once underflow, always underflow .. */
	}
    }
    z0 = sqrt(f);
    z = z0 / e1 * 0.5;
    z2 = f + f;

    if (a < b) {
	h = a / b;
	r0 = 1.0 / (h + 1.0);
	r1 = (b - a) / b;
	w0 = 1.0 / sqrt(a * (h + 1.0));
    } else {
	h = b / a;
	r0 = 1.0 / (h + 1.0);
	r1 = (b - a) / a;
	w0 = 1.0 / sqrt(b * (h + 1.0));
    }

    a0[0] = r1 * .66666666666666663;
    c[0] = a0[0] * -0.5;
    d[0] = -c[0];
    j0 = 0.5 / e0 * erfc1(1, z0);
    j1 = e1;
    sum = j0 + d[0] * w0 * j1;

    s = 1.0;
    h2 = h * h;
    hn = 1.0;
    w = w0;
    znm1 = z;
    zn = z2;
    for (n = 2; n <= num_IT; n += 2) {
	hn = h2 * hn;
	a0[n - 1] = r0 * 2.0 * (h * hn + 1.0) / (n + 2.0);
	np1 = n + 1;
	s += hn;
	a0[np1 - 1] = r1 * 2.0 * s / (n + 3.0);

	for (i = n; i <= np1; ++i) {
	    r = (i + 1.0) * -0.5;
	    b0[0] = r * a0[0];
	    for (m = 2; m <= i; ++m) {
		bsum = 0.0;
		mm1 = m - 1;
		for (j = 1; j <= mm1; ++j) {
		    mmj = m - j;
		    bsum += (j * r - mmj) * a0[j - 1] * b0[mmj - 1];
		}
		b0[m - 1] = r * a0[m - 1] + bsum / m;
	    }
	    c[i - 1] = b0[i - 1] / (i + 1.0);

	    dsum = 0.0;
	    im1 = i - 1;
	    for (j = 1; j <= im1; ++j) {
		imj = i - j;
		dsum += d[imj - 1] * c[j - 1];
	    }
	    d[i - 1] = -(dsum + c[i - 1]);
	}

	j0 = e1 * znm1 + (n - 1.0) * j0;
	j1 = e1 * zn + n * j1;
	znm1 = z2 * znm1;
	zn = z2 * zn;
	w = w0 * w;
	t0 = d[n - 1] * w * j0;
	w = w0 * w;
	t1 = d[np1 - 1] * w * j1;
	sum += t0 + t1;
	if (fabs(t0) + fabs(t1) <= eps * sum) {
	    break;
	}
    }

    if(log_p)
	return ln_e0 + t - bcorr(a, b) + log(sum);
    else {
	u = exp(-bcorr(a, b));
	return e0 * t * u * sum;
    }

} /* basym_ */


static double exparg(int l)
{
/* -------------------------------------------------------------------- */
/*     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
 *     EXP(W) CAN BE COMPUTED.  ==>  exparg(0) = 709.7827  nowadays. */

/*     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
 *     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
 *       ==> exparg(1) = -708.3964   nowadays. */

/*     Note... only an approximate value for exparg(L) is needed. */
/* -------------------------------------------------------------------- */

    static double const lnb = .69314718055995;
    int m;

    if (l == 0) {
	m = Rf_i1mach(16);
	return m * lnb * .99999;
    }
    m = Rf_i1mach(15) - 1;
    return m * lnb * .99999;
} /* exparg */

static double esum(int mu, double x)
{
/* ----------------------------------------------------------------------- */
/*                    EVALUATION OF EXP(MU + X) */
/* ----------------------------------------------------------------------- */
    double w;

    if (x > 0.0) { /* L10: */
	if (mu > 0) goto L20;
	w = mu + x;
	if (w < 0.0) goto L20;
    }
    else { /* x <= 0 */
	if (mu < 0) goto L20;
	w = mu + x;
	if (w > 0.0) goto L20;
    }
    return exp(w);

L20:
    w = (double) (mu);
    return exp(w) * exp(x);
} /* esum */

double rexpm1(double x)
{
/* ----------------------------------------------------------------------- */
/*            EVALUATION OF THE FUNCTION EXP(X) - 1 */
/* ----------------------------------------------------------------------- */

    static double p1 = 9.14041914819518e-10;
    static double p2 = .0238082361044469;
    static double q1 = -.499999999085958;
    static double q2 = .107141568980644;
    static double q3 = -.0119041179760821;
    static double q4 = 5.95130811860248e-4;

    if (fabs(x) <= 0.15) {
	return x * (((p2 * x + p1) * x + 1.0) /
		    ((((q4 * x + q3) * x + q2) * x + q1) * x + 1.0));
    }
    else { /* |x| > 0.15 : */
	double w = exp(x);
	if (x > 0.0)
	    return w * (0.5 - 1.0 / w + 0.5);
	else
	    return w - 0.5 - 0.5;
    }

} /* rexpm1 */

static double alnrel(double a)
{
/* -----------------------------------------------------------------------
 *            Evaluation of the function ln(1 + a)
 * ----------------------------------------------------------------------- */

    static double p1 = -1.29418923021993;
    static double p2 = .405303492862024;
    static double p3 = -.0178874546012214;
    static double q1 = -1.62752256355323;
    static double q2 = .747811014037616;
    static double q3 = -.0845104217945565;

    if (fabs(a) <= 0.375) {
	double t, t2, w;
	t = a / (a + 2.0);
	t2 = t * t;
	w = (((p3 * t2 + p2) * t2 + p1) * t2 + 1.) /
	    (((q3 * t2 + q2) * t2 + q1) * t2 + 1.);
	return t * 2.0 * w;
    } else {
	double x = a + 1.;
	return log(x);
    }
} /* alnrel */

static double rlog1(double x)
{
/* -----------------------------------------------------------------------
 *             Evaluation of the function  x - ln(1 + x)
 * ----------------------------------------------------------------------- */

    static double a = .0566749439387324;
    static double b = .0456512608815524;
    static double p0 = .333333333333333;
    static double p1 = -.224696413112536;
    static double p2 = .00620886815375787;
    static double q1 = -1.27408923933623;
    static double q2 = .354508718369557;

    double h, r, t, w, w1;

    if (x < -0.39 || x > 0.57) { /* direct evaluation */
	w = x + 0.5 + 0.5;
	return x - log(w);
    }
    /* else */
    if (x < -0.18) { /* L10: */
	h = x + .3;
	h /= .7;
	w1 = a - h * .3;
    }
    else if (x > 0.18) { /* L20: */
	h = x * .75 - .25;
	w1 = b + h / 3.0;
    }
    else { /*		Argument Reduction */
	h = x;
	w1 = 0.0;
    }

/* L30:              	Series Expansion */

    r = h / (h + 2.0);
    t = r * r;
    w = ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.0);
    return t * 2.0 * (1.0 / (1.0 - r) - r * w) + w1;

} /* rlog1 */

static double erf__(double x)
{
/* -----------------------------------------------------------------------
 *             EVALUATION OF THE REAL ERROR FUNCTION
 * ----------------------------------------------------------------------- */

    /* Initialized data */

    static double c = .564189583547756;
    static double a[5] = { 7.7105849500132e-5,-.00133733772997339,
	    .0323076579225834,.0479137145607681,.128379167095513 };
    static double b[3] = { .00301048631703895,.0538971687740286,
	    .375795757275549 };
    static double p[8] = { -1.36864857382717e-7,.564195517478974,
	    7.21175825088309,43.1622272220567,152.98928504694,
	    339.320816734344,451.918953711873,300.459261020162 };
    static double q[8] = { 1.,12.7827273196294,77.0001529352295,
	    277.585444743988,638.980264465631,931.35409485061,
	    790.950925327898,300.459260956983 };
    static double r[5] = { 2.10144126479064,26.2370141675169,
	    21.3688200555087,4.6580782871847,.282094791773523 };
    static double s[4] = { 94.153775055546,187.11481179959,
	    99.0191814623914,18.0124575948747 };

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double t, x2, ax, bot, top;

    ax = fabs(x);
    if (ax <= 0.5) {
	t = x * x;
	top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.0;
	bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.0;

	return x * (top / bot);
    }
    /* else: ax > 0.5 */

    if (ax <= 4.) { /*  ax in (0.5, 4] */

	top = ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax
		+ p[5]) * ax + p[6]) * ax + p[7];
	bot = ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax
		+ q[5]) * ax + q[6]) * ax + q[7];
	ret_val = 0.5 - exp(-x * x) * top / bot + 0.5;
	if (x < 0.0) {
	    ret_val = -ret_val;
	}
	return ret_val;
    }

    /* else: ax > 4 */

    if (ax >= 5.8) {
	return x > 0 ? 1 : -1;
    }
    x2 = x * x;
    t = 1.0 / x2;
    top = (((r[0] * t + r[1]) * t + r[2]) * t + r[3]) * t + r[4];
    bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.0;
    t = (c - top / (x2 * bot)) / ax;
    ret_val = 0.5 - exp(-x2) * t + 0.5;
    if (x < 0.0) {
	ret_val = -ret_val;
    }
    return ret_val;

} /* erf */

static double erfc1(int ind, double x)
{
/* ----------------------------------------------------------------------- */
/*         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION */

/*          ERFC1(IND,X) = ERFC(X)            IF IND = 0 */
/*          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE */
/* ----------------------------------------------------------------------- */

    /* Initialized data */

    static double c = .564189583547756;
    static double a[5] = { 7.7105849500132e-5,-.00133733772997339,
	    .0323076579225834,.0479137145607681,.128379167095513 };
    static double b[3] = { .00301048631703895,.0538971687740286,
	    .375795757275549 };
    static double p[8] = { -1.36864857382717e-7,.564195517478974,
	    7.21175825088309,43.1622272220567,152.98928504694,
	    339.320816734344,451.918953711873,300.459261020162 };
    static double q[8] = { 1.,12.7827273196294,77.0001529352295,
	    277.585444743988,638.980264465631,931.35409485061,
	    790.950925327898,300.459260956983 };
    static double r[5] = { 2.10144126479064,26.2370141675169,
	    21.3688200555087,4.6580782871847,.282094791773523 };
    static double s[4] = { 94.153775055546,187.11481179959,
	    99.0191814623914,18.0124575948747 };

    /* System generated locals */
    double ret_val, d1;

    /* Local variables */
    double e, t, w, ax, bot, top;

/*                     ABS(X) <= 0.5 */

    ax = fabs(x);
    if (ax > 0.5) {
	goto L10;
    }
    t = x * x;
    top = (((a[0] * t + a[1]) * t + a[2]) * t + a[3]) * t + a[4] + 1.0;
    bot = ((b[0] * t + b[1]) * t + b[2]) * t + 1.0;
    ret_val = 0.5 - x * (top / bot) + 0.5;
    if (ind != 0) {
	ret_val = exp(t) * ret_val;
    }
    return ret_val;

/*                  0.5 < ABS(X) <= 4 */

L10:
    if (ax > 4.0) {
	goto L20;
    }
    top = ((((((p[0] * ax + p[1]) * ax + p[2]) * ax + p[3]) * ax + p[4]) * ax
	    + p[5]) * ax + p[6]) * ax + p[7];
    bot = ((((((q[0] * ax + q[1]) * ax + q[2]) * ax + q[3]) * ax + q[4]) * ax
	    + q[5]) * ax + q[6]) * ax + q[7];
    ret_val = top / bot;
    goto L40;

/*                      ABS(X) > 4 */

L20:
    if (x <= -5.6) {
	goto L50;
    }
    if (ind != 0) {
	goto L30;
    }
    if (x > 100.0) {
	goto L60;
    }
    if (x * x > -exparg(1)) {
	goto L60;
    }

L30:
/* Computing 2nd power */
    d1 = 1.0 / x;
    t = d1 * d1;
    top = (((r[0] * t + r[1]) * t + r[2]) * t + r[3]) * t + r[4];
    bot = (((s[0] * t + s[1]) * t + s[2]) * t + s[3]) * t + 1.0;
    ret_val = (c - t * top / bot) / ax;

/*                      FINAL ASSEMBLY */

L40:
    if (ind == 0) {
	goto L41;
    }
    if (x < 0.0) {
	ret_val = exp(x * x) * 2.0 - ret_val;
    }
    return ret_val;
L41:
    w = x * x;
    t = w;
    e = w - t;
    ret_val = (0.5 - e + 0.5) * exp(-t) * ret_val;
    if (x < 0.0) {
	ret_val = 2.0 - ret_val;
    }
    return ret_val;

/*             LIMIT VALUE FOR LARGE NEGATIVE X */

L50:
    ret_val = 2.0;
    if (ind != 0) {
	ret_val = exp(x * x) * 2.0;
    }
    return ret_val;

/*             LIMIT VALUE FOR LARGE POSITIVE X */
/*                       WHEN IND = 0 */

L60:
    ret_val = 0.0;
    return ret_val;
} /* erfc1 */

static double gam1(double a)
{
/*     ------------------------------------------------------------------ */
/*     COMPUTATION OF 1/GAMMA(A+1) - 1  FOR -0.5 <= A <= 1.5 */
/*     ------------------------------------------------------------------ */

    double d, t, w, bot, top;

    t = a;
    d = a - 0.5;
    if (d > 0.0) {
	t = d - 0.5;
    }
    if (t < 0.0) { /* L30: */
	static double
	    r[9] = { -.422784335098468,-.771330383816272,
		     -.244757765222226,.118378989872749,9.30357293360349e-4,
		     -.0118290993445146,.00223047661158249,2.66505979058923e-4,
		     -1.32674909766242e-4 },
	    s1 = .273076135303957,
	    s2 = .0559398236957378;

	top = (((((((r[8] * t + r[7]) * t + r[6]) * t + r[5]) * t + r[4]
		     ) * t + r[3]) * t + r[2]) * t + r[1]) * t + r[0];
	bot = (s2 * t + s1) * t + 1.0;
	w = top / bot;
	if (d > 0.0)
	    return t * w / a;
	else
	    return a * (w + 0.5 + 0.5);

    } else if (t == 0) { /* L10: */
	return 0.;

    } else { /* t > 0;  L20: */
	static double
	    p[7] = { .577215664901533,-.409078193005776,
		     -.230975380857675,.0597275330452234,.0076696818164949,
		     -.00514889771323592,5.89597428611429e-4 },
	    q[5] = { 1.,.427569613095214,.158451672430138,
		     .0261132021441447,.00423244297896961 };

	top = (((((p[6] * t + p[5]) * t + p[4]) * t + p[3]) * t + p[2]
		   ) * t + p[1]) * t + p[0];
	bot = (((q[4] * t + q[3]) * t + q[2]) * t + q[1]) * t + 1.0;
	w = top / bot;
	if (d > 0.0) /* L21: */
	    return t / a * (w - 0.5 - 0.5);
	else
	    return a * w;
    }
} /* gam1 */

static double gamln1(double a)
{
/* ----------------------------------------------------------------------- */
/*     EVALUATION OF LN(GAMMA(1 + A)) FOR -0.2 <= A <= 1.25 */
/* ----------------------------------------------------------------------- */

    double w;
    if (a < 0.6) {
	static double p0 = .577215664901533;
	static double p1 = .844203922187225;
	static double p2 = -.168860593646662;
	static double p3 = -.780427615533591;
	static double p4 = -.402055799310489;
	static double p5 = -.0673562214325671;
	static double p6 = -.00271935708322958;
	static double q1 = 2.88743195473681;
	static double q2 = 3.12755088914843;
	static double q3 = 1.56875193295039;
	static double q4 = .361951990101499;
	static double q5 = .0325038868253937;
	static double q6 = 6.67465618796164e-4;
	w = ((((((p6 * a + p5)* a + p4)* a + p3)* a + p2)* a + p1)* a + p0) /
	    ((((((q6 * a + q5)* a + q4)* a + q3)* a + q2)* a + q1)* a + 1.);
	return -(a) * w;
    }
    else { /* 0.6 <= a <= 1.25 */
	static double r0 = .422784335098467;
	static double r1 = .848044614534529;
	static double r2 = .565221050691933;
	static double r3 = .156513060486551;
	static double r4 = .017050248402265;
	static double r5 = 4.97958207639485e-4;
	static double s1 = 1.24313399877507;
	static double s2 = .548042109832463;
	static double s3 = .10155218743983;
	static double s4 = .00713309612391;
	static double s5 = 1.16165475989616e-4;
	double x = a - 0.5 - 0.5;
	w = (((((r5 * x + r4) * x + r3) * x + r2) * x + r1) * x + r0) /
	    (((((s5 * x + s4) * x + s3) * x + s2) * x + s1) * x + 1.0);
	return x * w;
    }
} /* gamln1 */

static double psi(double x)
{
/* ---------------------------------------------------------------------

 *                 Evaluation of the Digamma function psi(x)

 *                           -----------

 *     Psi(xx) is assigned the value 0 when the digamma function cannot
 *     be computed.

 *     The main computation involves evaluation of rational Chebyshev
 *     approximations published in Math. Comp. 27, 123-127(1973) by
 *     Cody, Strecok and Thacher. */

/* --------------------------------------------------------------------- */
/*     Psi was written at Argonne National Laboratory for the FUNPACK */
/*     package of special function subroutines. Psi was modified by */
/*     A.H. Morris (NSWC). */
/* --------------------------------------------------------------------- */

    static double piov4 = .785398163397448; /* == pi / 4 */
/*     dx0 = zero of psi() to extended precision : */
    static double dx0 = 1.461632144968362341262659542325721325;

/* --------------------------------------------------------------------- */
/*     COEFFICIENTS FOR RATIONAL APPROXIMATION OF */
/*     PSI(X) / (X - X0),  0.5 <= X <= 3.0 */
    static double p1[7] = { .0089538502298197,4.77762828042627,
	    142.441585084029,1186.45200713425,3633.51846806499,
	    4138.10161269013,1305.60269827897 };
    static double q1[6] = { 44.8452573429826,520.752771467162,
	    2210.0079924783,3641.27349079381,1908.310765963,
	    6.91091682714533e-6 };
/* --------------------------------------------------------------------- */


/* --------------------------------------------------------------------- */
/*     COEFFICIENTS FOR RATIONAL APPROXIMATION OF */
/*     PSI(X) - LN(X) + 1 / (2*X),  X > 3.0 */

    static double p2[4] = { -2.12940445131011,-7.01677227766759,
	    -4.48616543918019,-.648157123766197 };
    static double q2[4] = { 32.2703493791143,89.2920700481861,
	    54.6117738103215,7.77788548522962 };
/* --------------------------------------------------------------------- */

    int i, m, n, nq;
    double d2;
    double w, z;
    double den, aug, sgn, xmx0, xmax1, upper, xsmall;

/* --------------------------------------------------------------------- */


/*     MACHINE DEPENDENT CONSTANTS ... */

/* --------------------------------------------------------------------- */
/*	  XMAX1	 = THE SMALLEST POSITIVE FLOATING POINT CONSTANT
		   WITH ENTIRELY INT REPRESENTATION.  ALSO USED
		   AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE
		   ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH
		   PSI MAY BE REPRESENTED AS LOG(X).
 * Originally:  xmax1 = amin1(ipmpar(3), 1./spmpar(1))  */
    xmax1 = (double) INT_MAX;
    d2 = 0.5 / Rf_d1mach(3); /*= 0.5 / (0.5 * DBL_EPS) = 1/DBL_EPSILON = 2^52 */
    if(xmax1 > d2) xmax1 = d2;

/* --------------------------------------------------------------------- */
/*        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X) */
/*                 MAY BE REPRESENTED BY 1/X. */
    xsmall = 1e-9;
/* --------------------------------------------------------------------- */
    aug = 0.0;
    if (x < 0.5) {
/* --------------------------------------------------------------------- */
/*     X < 0.5,  USE REFLECTION FORMULA */
/*     PSI(1-X) = PSI(X) + PI * COTAN(PI*X) */
/* --------------------------------------------------------------------- */
	if (fabs(x) <= xsmall) {

	    if (x == 0.0) {
		goto L_err;
	    }
/* --------------------------------------------------------------------- */
/*     0 < ABS(X) <= XSMALL.  USE 1/X AS A SUBSTITUTE */
/*     FOR  PI*COTAN(PI*X) */
/* --------------------------------------------------------------------- */
	    aug = -1.0 / x;
	} else { /* |x| > xsmall */
/* --------------------------------------------------------------------- */
/*     REDUCTION OF ARGUMENT FOR COTAN */
/* --------------------------------------------------------------------- */
	    /* L100: */
	    w = -x;
	    sgn = piov4;
	    if (w <= 0.0) {
		w = -w;
		sgn = -sgn;
	    }
/* --------------------------------------------------------------------- */
/*     MAKE AN ERROR EXIT IF |X| >= XMAX1 */
/* --------------------------------------------------------------------- */
	    if (w >= xmax1) {
		goto L_err;
	    }
	    nq = (int) w;
	    w -= (double) nq;
	    nq = (int) (w * 4.0);
	    w = (w - (double) nq * 0.25) * 4.0;
/* --------------------------------------------------------------------- */
/*     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X. */
/*     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST */
/*     QUADRANT AND DETERMINE SIGN */
/* --------------------------------------------------------------------- */
	    n = nq / 2;
	    if (n + n != nq) {
		w = 1.0 - w;
	    }
	    z = piov4 * w;
	    m = n / 2;
	    if (m + m != n) {
		sgn = -sgn;
	    }
/* --------------------------------------------------------------------- */
/*     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X) */
/* --------------------------------------------------------------------- */
	    n = (nq + 1) / 2;
	    m = n / 2;
	    m += m;
	    if (m == n) {
/* --------------------------------------------------------------------- */
/*     CHECK FOR SINGULARITY */
/* --------------------------------------------------------------------- */
		if (z == 0.0) {
		    goto L_err;
		}
/* --------------------------------------------------------------------- */
/*     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND */
/*     SIN/COS AS A SUBSTITUTE FOR TAN */
/* --------------------------------------------------------------------- */
		aug = sgn * (cos(z) / sin(z) * 4.0);

	    } else { /* L140: */
		aug = sgn * (sin(z) / cos(z) * 4.0);
	    }
	}

	x = 1.0 - x;

    }
    /* L200: */
    if (x <= 3.0) {
/* --------------------------------------------------------------------- */
/*     0.5 <= X <= 3.0 */
/* --------------------------------------------------------------------- */
	den = x;
	upper = p1[0] * x;

	for (i = 1; i <= 5; ++i) {
	    den = (den + q1[i - 1]) * x;
	    upper = (upper + p1[i]) * x;
	}

	den = (upper + p1[6]) / (den + q1[5]);
	xmx0 = x - dx0;
	return den * xmx0 + aug;
    }

/* --------------------------------------------------------------------- */
/*     IF X >= XMAX1, PSI = LN(X) */
/* --------------------------------------------------------------------- */
    if (x < xmax1) {
/* --------------------------------------------------------------------- */
/*     3.0 < X < XMAX1 */
/* --------------------------------------------------------------------- */
	w = 1.0 / (x * x);
	den = w;
	upper = p2[0] * w;

	for (i = 1; i <= 3; ++i) {
	    den = (den + q2[i - 1]) * w;
	    upper = (upper + p2[i]) * w;
	}

	aug = upper / (den + q2[3]) - 0.5 / x + aug;
    }
    return aug + log(x);

/* --------------------------------------------------------------------- */
/*     ERROR RETURN */
/* --------------------------------------------------------------------- */
L_err:
    return 0.;
} /* psi */

static double betaln(double a0, double b0)
{
/* -----------------------------------------------------------------------
 *     Evaluation of the logarithm of the beta function  ln(beta(a0,b0))
 * ----------------------------------------------------------------------- */

    static double e = .918938533204673;/* e == 0.5*LN(2*PI) */

    double a, b, c, h, u, v, w, z;
    int i, n;

    a = min(a0 ,b0);
    b = max(a0, b0);
    if (a >= 8.0) {
	goto L60;
    }
    if (a < 1.0) {
/* ----------------------------------------------------------------------- */
/*                   PROCEDURE WHEN A < 1 */
/* ----------------------------------------------------------------------- */
	if (b < 8.0)
	    return gamln(a) + (gamln(b) - gamln(a+b));
	else
	    return gamln(a) + algdiv(a, b);
    }
    /* else */
/* ----------------------------------------------------------------------- */
/*                PROCEDURE WHEN 1 <= A < 8 */
/* ----------------------------------------------------------------------- */
    if (a > 2.0) {
	goto L30;
    }
    if (b <= 2.0) {
	return gamln(a) + gamln(b) - gsumln(a, b);
    }
    /* else */

    w = 0.0;
    if (b < 8.0) {
	goto L40;
    }
    return gamln(a) + algdiv(a, b);

L30:
/*                REDUCTION OF A WHEN B <= 1000 */

    if (b > 1e3) {
	goto L50;
    }
    n = a - 1.0;
    w = 1.0;
    for (i = 1; i <= n; ++i) {
	a += -1.0;
	h = a / b;
	w *= h / (h + 1.0);
    }
    w = log(w);
    if (b < 8.0) {
	goto L40;
    }
    return w + gamln(a) + algdiv(a, b);

L40:
/*                 REDUCTION OF B WHEN B < 8 */

    n = b - 1.0;
    z = 1.0;
    for (i = 1; i <= n; ++i) {
	b += -1.0;
	z *= b / (a + b);
    }
    return w + log(z) + (gamln(a) + (gamln(b) - gsumln(a, b)));

L50:
/*                REDUCTION OF A WHEN B > 1000 */
    n = a - 1.0;
    w = 1.0;
    for (i = 1; i <= n; ++i) {
	a += -1.0;
	w *= a / (a / b + 1.0);
    }
    return log(w) - n * log(b) + (gamln(a) + algdiv(a, b));

L60:
/* ----------------------------------------------------------------------- */
/*                   PROCEDURE WHEN A >= 8 */
/* ----------------------------------------------------------------------- */

    w = bcorr(a, b);
    h = a / b;
    c = h / (h + 1.0);
    u = -(a - 0.5) * log(c);
    v = b * alnrel(h);
    if (u > v)
	return log(b) * -0.5 + e + w - v - u;
    else
	return log(b) * -0.5 + e + w - u - v;

} /* betaln */

static double gsumln(double a, double b)
{
/* ----------------------------------------------------------------------- */
/*          EVALUATION OF THE FUNCTION LN(GAMMA(A + B)) */
/*          FOR 1 <= A <= 2  AND  1 <= B <= 2 */
/* ----------------------------------------------------------------------- */

    double x = a + b - 2.;/* in [0, 2] */

    if (x <= 0.25)
	return gamln1(x + 1.0);

    /* else */
    if (x <= 1.25)
	return gamln1(x) + alnrel(x);
    /* else x > 1.25 : */
    return gamln1(x - 1.0) + log(x * (x + 1.0));

} /* gsumln */

static double bcorr(double a0, double b0)
{
/* ----------------------------------------------------------------------- */

/*     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE */
/*     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A). */
/*     IT IS ASSUMED THAT A0 >= 8 AND B0 >= 8. */

/* ----------------------------------------------------------------------- */
    /* Initialized data */

    static double c0 = .0833333333333333;
    static double c1 = -.00277777777760991;
    static double c2 = 7.9365066682539e-4;
    static double c3 = -5.9520293135187e-4;
    static double c4 = 8.37308034031215e-4;
    static double c5 = -.00165322962780713;

    /* System generated locals */
    double ret_val, r1;

    /* Local variables */
    double a, b, c, h, t, w, x, s3, s5, x2, s7, s9, s11;
/* ------------------------ */
    a = min(a0, b0);
    b = max(a0, b0);

    h = a / b;
    c = h / (h + 1.0);
    x = 1.0 / (h + 1.0);
    x2 = x * x;

/*                SET SN = (1 - X^N)/(1 - X) */

    s3 = x + x2 + 1.0;
    s5 = x + x2 * s3 + 1.0;
    s7 = x + x2 * s5 + 1.0;
    s9 = x + x2 * s7 + 1.0;
    s11 = x + x2 * s9 + 1.0;

/*                SET W = DEL(B) - DEL(A + B) */

/* Computing 2nd power */
    r1 = 1.0 / b;
    t = r1 * r1;
    w = ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t + c1 *
	    s3) * t + c0;
    w *= c / b;

/*                   COMPUTE  DEL(A) + W */

/* Computing 2nd power */
    r1 = 1.0 / a;
    t = r1 * r1;
    ret_val = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a +
	    w;
    return ret_val;
} /* bcorr */

static double algdiv(double a, double b)
{
/* ----------------------------------------------------------------------- */

/*     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B >= 8 */

/*                         -------- */

/*     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY */
/*     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X). */

/* ----------------------------------------------------------------------- */

    /* Initialized data */

    static double c0 = .0833333333333333;
    static double c1 = -.00277777777760991;
    static double c2 = 7.9365066682539e-4;
    static double c3 = -5.9520293135187e-4;
    static double c4 = 8.37308034031215e-4;
    static double c5 = -.00165322962780713;

    double c, d, h, t, u, v, w, x, s3, s5, x2, s7, s9, s11;

/* ------------------------ */
    if (a > b) {
	h = b / a;
	c = 1.0 / (h + 1.0);
	x = h / (h + 1.0);
	d = a + (b - 0.5);
    }
    else {
	h = a / b;
	c = h / (h + 1.0);
	x = 1.0 / (h + 1.0);
	d = b + (a - 0.5);
    }

/* Set s<n> = (1 - x^n)/(1 - x) : */

    x2 = x * x;
    s3 = x + x2 + 1.0;
    s5 = x + x2 * s3 + 1.0;
    s7 = x + x2 * s5 + 1.0;
    s9 = x + x2 * s7 + 1.0;
    s11 = x + x2 * s9 + 1.0;

/* w := Del(b) - Del(a + b) */

    t = 1./ (b * b);
    w = ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t + c1 *
	    s3) * t + c0;
    w *= c / b;

/*                    COMBINE THE RESULTS */

    u = d * alnrel(a / b);
    v = a * (log(b) - 1.0);
    if (u > v)
	return w - v - u;
    else
	return w - u - v;
} /* algdiv */

static double gamln(double a)
{
/* -----------------------------------------------------------------------
 *            Evaluation of  ln(gamma(a))  for positive a
 * ----------------------------------------------------------------------- */
/*     Written by Alfred H. Morris */
/*          Naval Surface Warfare Center */
/*          Dahlgren, Virginia */
/* ----------------------------------------------------------------------- */

    static double d = .418938533204673;/* d == 0.5*(LN(2*PI) - 1) */

    static double c0 = .0833333333333333;
    static double c1 = -.00277777777760991;
    static double c2 = 7.9365066682539e-4;
    static double c3 = -5.9520293135187e-4;
    static double c4 = 8.37308034031215e-4;
    static double c5 = -.00165322962780713;

    if (a <= 0.8)
	return gamln1(a) - log(a); /* ln(G(a+1)) - ln(a) == ln(G(a+1)/a) = ln(G(a)) */
    else if (a <= 2.25)
	return gamln1(a - 0.5 - 0.5);

    else if (a < 10.0) {
	int i, n = a - 1.25;
	double t = a;
	double w = 1.0;
	for (i = 1; i <= n; ++i) {
	    t += -1.0;
	    w *= t;
	}
	return gamln1(t - 1.) + log(w);
    }
    else { /* a >= 10 */
	double t = 1. / (a * a);
	double w = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a;
	return d + w + (a - 0.5) * (log(a) - 1.0);
    }
} /* gamln */
