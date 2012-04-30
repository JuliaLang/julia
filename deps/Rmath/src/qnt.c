/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006-8 The R Development Core Team
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

#include "nmath.h"
#include "dpq.h"

double qnt(double p, double df, double ncp, int lower_tail, int log_p)
{
    const static double accu = 1e-13;
    const static double Eps = 1e-11; /* must be > accu */

    double ux, lx, nx, pp;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(df) || ISNAN(ncp))
	return p + df + ncp;
#endif
    if (!R_FINITE(df)) ML_ERR_return_NAN;

    /* Was
     * df = floor(df + 0.5);
     * if (df < 1 || ncp < 0) ML_ERR_return_NAN;
     */
    if (df <= 0.0) ML_ERR_return_NAN;

    if(ncp == 0.0 && df >= 1.0) return qt(p, df, lower_tail, log_p);

    R_Q_P01_boundaries(p, ML_NEGINF, ML_POSINF);

    p = R_DT_qIv(p);

    /* Invert pnt(.) :
     * 1. finding an upper and lower bound */
    if(p > 1 - DBL_EPSILON) return ML_POSINF;
    pp = fmin2(1 - DBL_EPSILON, p * (1 + Eps));
    for(ux = fmax2(1., ncp);
	ux < DBL_MAX && pnt(ux, df, ncp, TRUE, FALSE) < pp;
	ux *= 2);
    pp = p * (1 - Eps);
    for(lx = fmin2(-1., -ncp);
	lx > -DBL_MAX && pnt(lx, df, ncp, TRUE, FALSE) > pp;
	lx *= 2);

    /* 2. interval (lx,ux)  halving : */
    do {
	nx = 0.5 * (lx + ux);
	if (pnt(nx, df, ncp, TRUE, FALSE) > p) ux = nx; else lx = nx;
    }
    while ((ux - lx) / fabs(nx) > accu);

    return 0.5 * (lx + ux);
}
