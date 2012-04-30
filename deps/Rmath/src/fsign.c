/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *    #include <Rmath.h>
 *    double fsign(double x, double y);
 *
 *  DESCRIPTION
 *
 *    This function performs transfer of sign.  The result is:
 *
 *                        |x| * signum(y)
 */

#include "nmath.h"

double fsign(double x, double y)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(y))
	return x + y;
#endif
    return ((y >= 0) ? fabs(x) : -fabs(x));
}
