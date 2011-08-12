/*
 * drem() wrapper for remainder().
 *
 * Written by J.T. Conklin, <jtc@wimsey.com>
 * Placed into the Public Domain, 1994.
 */

#include "openlibm.h"

double
drem(x, y)
	double x, y;
{
	return remainder(x, y);
}
