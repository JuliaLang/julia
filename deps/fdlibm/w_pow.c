

/* @(#)w_pow.c 1.3 95/01/18 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* 
 * wrapper pow(x,y) return x**y
 */

#include "fdlibm.h"


#ifdef __STDC__
	double pow(double x, double y)	/* wrapper pow */
#else
	double pow(x,y)			/* wrapper pow */
	double x,y;
#endif
{
#ifdef _IEEE_LIBM
	return  __ieee754_pow(x,y);
#else
	double z;
	z=__ieee754_pow(x,y);
	if(_LIB_VERSION == _IEEE_|| isnan(y)) return z;
	if(isnan(x)) {
	    if(y==0.0) 
	        return __kernel_standard(x,y,42); /* pow(NaN,0.0) */
	    else 
		return z;
	}
	if(x==0.0){ 
	    if(y==0.0)
	        return __kernel_standard(x,y,20); /* pow(0.0,0.0) */
	    if(finite(y)&&y<0.0)
	        return __kernel_standard(x,y,23); /* pow(0.0,negative) */
	    return z;
	}
	if(!finite(z)) {
	    if(finite(x)&&finite(y)) {
	        if(isnan(z))
	            return __kernel_standard(x,y,24); /* pow neg**non-int */
	        else 
	            return __kernel_standard(x,y,21); /* pow overflow */
	    }
	} 
	if(z==0.0&&finite(x)&&finite(y))
	    return __kernel_standard(x,y,22); /* pow underflow */
	return z;
#endif
}
