/* k_sinf.c -- float version of k_sin.c
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Optimized by Bruce D. Evans.
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

#ifndef INLINE_KERNEL_SINDF
#include <sys/cdefs.h>
__FBSDID("$FreeBSD: src/lib/msun/src/k_sinf.c,v 1.16 2009/06/03 08:16:34 ed Exp $");
#endif

#include "math.h"
#include "math_private.h"

/* |sin(x)/x - s(x)| < 2**-37.5 (~[-4.89e-12, 4.824e-12]). */
static const double
S1 = -0x15555554cbac77.0p-55,	/* -0.166666666416265235595 */
S2 =  0x111110896efbb2.0p-59,	/*  0.0083333293858894631756 */
S3 = -0x1a00f9e2cae774.0p-65,	/* -0.000198393348360966317347 */
S4 =  0x16cd878c3b46a7.0p-71;	/*  0.0000027183114939898219064 */

#ifndef INLINE_KERNEL_SINDF
extern
#endif
__inline float
__kernel_sindf(double x)
{
	double r, s, w, z;

	/* Try to optimize for parallel evaluation as in k_tanf.c. */
	z = x*x;
	w = z*z;
	r = S3+z*S4;
	s = z*x;
	return (x + s*(S1+z*S2)) + s*w*r;
}
