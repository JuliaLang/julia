/* w_tgammaf.c -- float version of w_tgamma.c.
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

#include "math.h"
#include "math_private.h"

float lgammaf_r_fdlibm(float x, int *signgamp);

extern int signgam;

float
tgammaf(float x)
{
	return lgammaf_r_fdlibm(x,&signgam);
}
