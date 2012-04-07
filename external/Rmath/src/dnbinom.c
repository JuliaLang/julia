/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000 and Feb, 2001.
 *
 *    dnbinom_mu(): Martin Maechler, June 2008
 *
 *  Merge in to R:
 *	Copyright (C) 2000--2008, The R Core Development Team
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
 *
 * DESCRIPTION
 *
 *   Computes the negative binomial distribution. For integer n,
 *   this is probability of x failures before the nth success in a
 *   sequence of Bernoulli trials. We do not enforce integer n, since
 *   the distribution is well defined for non-integers,
 *   and this can be useful for e.g. overdispersed discrete survival times.
 */

#include "nmath.h"
#include "dpq.h"

double dnbinom(double x, double size, double prob, int give_log)
{
    double ans, p;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(size) || ISNAN(prob))
        return x + size + prob;
#endif

    if (prob <= 0 || prob > 1 || size < 0) ML_ERR_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x)) return R_D__0;
    x = R_D_forceint(x);

    ans = dbinom_raw(size, x+size, prob, 1-prob, give_log);
    p = ((double)size)/(size+x);
    return((give_log) ? log(p) + ans : p * ans);
}

double dnbinom_mu(double x, double size, double mu, int give_log)
{
    /* originally, just set  prob :=  size / (size + mu)  and called dbinom_raw(),
     * but that suffers from cancellation when   mu << size  */
    double ans, p;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(size) || ISNAN(mu))
        return x + size + mu;
#endif

    if (mu < 0 || size < 0) ML_ERR_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x)) return R_D__0;
    x = R_D_forceint(x);
    if(x == 0)/* be accurate, both for n << mu, and n >> mu :*/
	return R_D_exp(size * (size < mu ? log(size/(size+mu)) : log1p(- mu/(size+mu))));
    if(x < 1e-10 * size) { /* don't use dbinom_raw() but MM's formula: */
	/* FIXME --- 1e-8 shows problem; rather use algdiv() from ./toms708.c */
	return R_D_exp(x * log(size*mu / (size+mu)) - mu - lgamma(x+1) +
		       log1p(x*(x-1)/(2*size)));
    }
    /* else: no unnecessary cancellation inside dbinom_raw, when
     * x_ = size and n_ = x+size are so close that n_ - x_ loses accuracy
     */
    ans = dbinom_raw(size, x+size, size/(size+mu), mu/(size+mu), give_log);
    p = ((double)size)/(size+x);
    return((give_log) ? log(p) + ans : p * ans);
}
