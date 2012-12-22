/*
  Mathlib : A C Library of Special Functions
  Copyright (C) 1999-2007  The R Development Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, a copy is available at
  http://www.r-project.org/Licenses/

  SYNOPSIS

    #include <Rmath.h>
    double dwilcox(double x, double m, double n, int give_log)
    double pwilcox(double x, double m, double n, int lower_tail, int log_p)
    double qwilcox(double x, double m, double n, int lower_tail, int log_p);
    double rwilcox(double m, double n)

  DESCRIPTION

    dwilcox	The density of the Wilcoxon distribution.
    pwilcox	The distribution function of the Wilcoxon distribution.
    qwilcox	The quantile function of the Wilcoxon distribution.
    rwilcox	Random variates from the Wilcoxon distribution.

 */

/* 
   Note: the checks here for R_CheckInterrupt also do stack checking.

   calloc/free are remapped for use in R, so allocation checks are done there.
   freeing is completed by an on.exit action in the R wrappers.
*/

#include "nmath.h"
#include "dpq.h"

#ifndef MATHLIB_STANDALONE
#include <R_ext/Utils.h>
#endif

static double ***w; /* to store  cwilcox(i,j,k) -> w[i][j][k] */
static int allocated_m, allocated_n;

static void
w_free(int m, int n)
{
    int i, j;

    for (i = m; i >= 0; i--) {
	for (j = n; j >= 0; j--) {
	    if (w[i][j] != 0)
		free((void *) w[i][j]);
	}
	free((void *) w[i]);
    }
    free((void *) w);
    w = 0; allocated_m = allocated_n = 0;
}

static void
w_init_maybe(int m, int n)
{
    int i;

    if (m > n) {
	i = n; n = m; m = i;
    }
    if (w && (m > allocated_m || n > allocated_n))
	w_free(allocated_m, allocated_n); /* zeroes w */

    if (!w) { /* initialize w[][] */
	m = imax2(m, WILCOX_MAX);
	n = imax2(n, WILCOX_MAX);
	w = (double ***) calloc((size_t) m + 1, sizeof(double **));
#ifdef MATHLIB_STANDALONE
	if (!w) MATHLIB_ERROR(_("wilcox allocation error %d"), 1);
#endif
	for (i = 0; i <= m; i++) {
	    w[i] = (double **) calloc((size_t) n + 1, sizeof(double *));
#ifdef MATHLIB_STANDALONE
	    /* the apparent leak here in the in-R case should be
	       swept up by the on.exit action */
	    if (!w[i]) {
		/* first free all earlier allocations */
		w_free(i-1, n);
		MATHLIB_ERROR(_("wilcox allocation error %d"), 2);
	    }
#endif
	}
	allocated_m = m; allocated_n = n;
    }
}

static void
w_free_maybe(int m, int n)
{
    if (m > WILCOX_MAX || n > WILCOX_MAX)
	w_free(m, n);
}


/* This counts the number of choices with statistic = k */
static double
cwilcox(int k, int m, int n)
{
    int c, u, i, j, l;

#ifndef MATHLIB_STANDALONE
    R_CheckUserInterrupt();
#endif

    u = m * n;
    if (k < 0 || k > u)
	return(0);
    c = (int)(u / 2);
    if (k > c)
	k = u - k; /* hence  k <= floor(u / 2) */
    if (m < n) {
	i = m; j = n;
    } else {
	i = n; j = m;
    } /* hence  i <= j */

    if (j == 0) /* and hence i == 0 */
	return (k == 0);


    /* We can simplify things if k is small.  Consider the Mann-Whitney 
       definition, and sort y.  Then if the statistic is k, no more 
       than k of the y's can be <= any x[i], and since they are sorted 
       these can only be in the first k.  So the count is the same as
       if there were just k y's. 
    */
    if (j > 0 && k < j) return cwilcox(k, i, k);    
    
    if (w[i][j] == 0) {
	w[i][j] = (double *) calloc((size_t) c + 1, sizeof(double));
#ifdef MATHLIB_STANDALONE
	if (!w[i][j]) MATHLIB_ERROR(_("wilcox allocation error %d"), 3);
#endif
	for (l = 0; l <= c; l++)
	    w[i][j][l] = -1;
    }
    if (w[i][j][k] < 0) {
	if (j == 0) /* and hence i == 0 */
	    w[i][j][k] = (k == 0);
	else
	    w[i][j][k] = cwilcox(k - j, i - 1, j) + cwilcox(k, i, j - 1);

    }
    return(w[i][j][k]);
}

double dwilcox(double x, double m, double n, int give_log)
{
    double d;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
#endif
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if (m <= 0 || n <= 0)
	ML_ERR_return_NAN;

    if (fabs(x - floor(x + 0.5)) > 1e-7)
	return(R_D__0);
    x = floor(x + 0.5);
    if ((x < 0) || (x > m * n))
	return(R_D__0);

    w_init_maybe(m, n);
    d = give_log ?
	log(cwilcox(x, m, n)) - lchoose(m + n, n) :
	    cwilcox(x, m, n)  /	 choose(m + n, n);

    return(d);
}

/* args have the same meaning as R function pwilcox */
double pwilcox(double q, double m, double n, int lower_tail, int log_p)
{
    int i;
    double c, p;

#ifdef IEEE_754
    if (ISNAN(q) || ISNAN(m) || ISNAN(n))
	return(q + m + n);
#endif
    if (!R_FINITE(m) || !R_FINITE(n))
	ML_ERR_return_NAN;
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if (m <= 0 || n <= 0)
	ML_ERR_return_NAN;

    q = floor(q + 1e-7);

    if (q < 0.0)
	return(R_DT_0);
    if (q >= m * n)
	return(R_DT_1);

    w_init_maybe(m, n);
    c = choose(m + n, n);
    p = 0;
    /* Use summation of probs over the shorter range */
    if (q <= (m * n / 2)) {
	for (i = 0; i <= q; i++)
	    p += cwilcox(i, m, n) / c;
    }
    else {
	q = m * n - q;
	for (i = 0; i < q; i++)
	    p += cwilcox(i, m, n) / c;
	lower_tail = !lower_tail; /* p = 1 - p; */
    }

    return(R_DT_val(p));
} /* pwilcox */

/* x is 'p' in R function qwilcox */

double qwilcox(double x, double m, double n, int lower_tail, int log_p)
{
    double c, p, q;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
#endif
    if(!R_FINITE(x) || !R_FINITE(m) || !R_FINITE(n))
	ML_ERR_return_NAN;
    R_Q_P01_check(x);

    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if (m <= 0 || n <= 0)
	ML_ERR_return_NAN;

    if (x == R_DT_0)
	return(0);
    if (x == R_DT_1)
	return(m * n);

    if(log_p || !lower_tail)
	x = R_DT_qIv(x); /* lower_tail,non-log "p" */

    w_init_maybe(m, n);
    c = choose(m + n, n);
    p = 0;
    q = 0;
    if (x <= 0.5) {
	x = x - 10 * DBL_EPSILON;
	for (;;) {
	    p += cwilcox(q, m, n) / c;
	    if (p >= x)
		break;
	    q++;
	}
    }
    else {
	x = 1 - x + 10 * DBL_EPSILON;
	for (;;) {
	    p += cwilcox(q, m, n) / c;
	    if (p > x) {
		q = m * n - q;
		break;
	    }
	    q++;
	}
    }

    return(q);
}

double rwilcox(double m, double n)
{
    int i, j, k, *x;
    double r;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(m) || ISNAN(n))
	return(m + n);
#endif
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if ((m < 0) || (n < 0))
	ML_ERR_return_NAN;

    if ((m == 0) || (n == 0))
	return(0);

    r = 0.0;
    k = (int) (m + n);
    x = (int *) calloc((size_t) k, sizeof(int));
#ifdef MATHLIB_STANDALONE
    if (!x) MATHLIB_ERROR(_("wilcox allocation error %d"), 4);
#endif
    for (i = 0; i < k; i++)
	x[i] = i;
    for (i = 0; i < n; i++) {
	j = floor(k * unif_rand());
	r += x[j];
	x[j] = x[--k];
    }
    free(x);
    return(r - n * (n - 1) / 2);
}

void wilcox_free(void)
{
    w_free_maybe(allocated_m, allocated_n);
}
