/*
 * cabs() wrapper for hypot().
 *
 * Written by J.T. Conklin, <jtc@wimsey.com>
 * Placed into the Public Domain, 1994.
 */

#include <sys/cdefs.h>


#include <complex.h>
#include <float.h>
#include "openlibm.h"

double
cabs(double complex z)
{
	return hypot(creal(z), cimag(z));
}

#if LDBL_MANT_DIG == 53
__weak_reference(cabs, cabsl);
#endif
