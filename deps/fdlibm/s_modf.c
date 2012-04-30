
/* @(#)s_modf.c 1.3 95/01/18 */
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
 * modf(double x, double *iptr) 
 * return fraction part of x, and return x's integral part in *iptr.
 * Method:
 *	Bit twiddling.
 *
 * Exception:
 *	No exception.
 */

#include "fdlibm.h"

#ifdef __STDC__
static const double one = 1.0;
#else
static double one = 1.0;
#endif

#ifdef __STDC__
	double modf(double x, double *iptr)
#else
	double modf(x, iptr)
	double x,*iptr;
#endif
{
	int i0,i1,j0;
	unsigned i;
        GET_HIGH_WORD(i0, x);
        GET_LOW_WORD(i1, x);
	j0 = ((i0>>20)&0x7ff)-0x3ff;	/* exponent of x */
	if(j0<20) {			/* integer part in high x */
	    if(j0<0) {			/* |x|<1 */
                double tmp;
                SET_HIGH_WORD(tmp, i0&0x80000000);
                SET_LOW_WORD(tmp, 0);           /* *iptr = +-0 */
                *iptr = tmp;
		return x;
	    } else {
		i = (0x000fffff)>>j0;
		if(((i0&i)|i1)==0) {		/* x is integral */
		    *iptr = x;
                    GET_HIGH_WORD(i0, x);
                    SET_HIGH_WORD(x, i0&0x80000000);
                    SET_LOW_WORD(x, 0); /* return +-0 */
		    return x;
		} else {
                    double tmp;
                    SET_HIGH_WORD(tmp, i0&(~i));
                    SET_LOW_WORD(tmp, 0);
                    *iptr = tmp;
		    return x - *iptr;
		}
	    }
	} else if (j0>51) {		/* no fraction part */
	    *iptr = x*one;
            GET_HIGH_WORD(i0, x);
            SET_HIGH_WORD(x, i0&0x80000000);
            SET_LOW_WORD(x, 0); /* return +-0 */
	    return x;
	} else {			/* fraction part in low x */
	    i = ((unsigned)(0xffffffff))>>(j0-20);
	    if((i1&i)==0) { 		/* x is integral */
		*iptr = x;

                GET_HIGH_WORD(i0, x);
                SET_HIGH_WORD(x, i0&0x80000000);
                SET_LOW_WORD(x, 0); /* return +-0 */
		return x;
	    } else {
                double tmp;
                SET_HIGH_WORD(tmp, i0);
                SET_LOW_WORD(tmp, i1&(~i));
                *iptr = tmp;
		return x - *iptr;
	    }
	}
}
