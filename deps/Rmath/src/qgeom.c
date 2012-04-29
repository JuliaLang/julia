/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 	     Ross Ihaka
 *  Copyright (C) 2000 	     The R Development Core Team
 *  Copyright (C) 2004--2005 The R Foundation
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
 *    The quantile function of the geometric distribution.
 */

#include "nmath.h"
#include "dpq.h"

double qgeom(double p, double prob, int lower_tail, int log_p)
{
    if (prob <= 0 || prob > 1) ML_ERR_return_NAN;

    R_Q_P01_boundaries(p, 0, ML_POSINF);

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(prob))
	return p + prob;
#endif

    if (prob == 1) return(0);
/* add a fuzz to ensure left continuity */
    return ceil(R_DT_Clog(p) / log1p(- prob) - 1 - 1e-7);
}
