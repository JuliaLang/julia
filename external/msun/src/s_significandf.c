/* s_significandf.c -- float version of s_significand.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD: src/lib/msun/src/s_significandf.c,v 1.8 2008/02/22 02:30:36 das Exp $");

#include "math.h"
#include "math_private.h"

float
significandf(float x)
{
	return __ieee754_scalbf(x,(float) -ilogbf(x));
}
