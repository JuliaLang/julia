/*
 * cabs() wrapper for hypot().
 *
 * Written by J.T. Conklin, <jtc@wimsey.com>
 * Placed into the Public Domain, 1994.
 *
 * Modified by Steven G. Kargl for the long double type.
 */

#include <sys/cdefs.h>


#include <complex.h>
#include "openlibm.h"

long double
cabsl(long double complex z)
{
	return hypotl(creall(z), cimagl(z));
}
