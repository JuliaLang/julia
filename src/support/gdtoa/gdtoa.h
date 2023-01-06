/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998 by Lucent Technologies
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

****************************************************************/

/* Please send bug reports to David M. Gay (dmg at acm dot org,
 * with " at " changed at "@" and " dot " changed to ".").	*/

/* Modified by Danny Smith for inclusion in libmingwex.a
   Aug 2006  */

#ifndef GDTOA_H_INCLUDED
#define GDTOA_H_INCLUDED

#include "gd_arith.h"
#include <stddef.h> /* for size_t */

#if defined(__MINGW32__) || defined(__MINGW64__)
/* keep the 'Long' definition as 'long' for compatibility
 * with older/other software. long in w64 is 32 bits anyway..
 */
#define Long long	/* Windows long is 32 bit */
#undef  NO_LONG_LONG	/* we have long long type */
#endif	/* MinGW */

#ifndef Long
#define Long int
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef UShort
typedef unsigned short UShort;
#endif

enum {	/* return values from strtodg */
	STRTOG_Zero	= 0,
	STRTOG_Normal	= 1,
	STRTOG_Denormal	= 2,
	STRTOG_Infinite	= 3,
	STRTOG_NaN	= 4,
	STRTOG_NaNbits	= 5,
	STRTOG_NoNumber	= 6,
	STRTOG_Retmask	= 7,

	/* The following may be or-ed into one of the above values. */

	STRTOG_Neg	= 0x08, /* does not affect STRTOG_Inexlo or STRTOG_Inexhi */
	STRTOG_Inexlo	= 0x10,	/* returned result rounded toward zero */
	STRTOG_Inexhi	= 0x20, /* returned result rounded away from zero */
	STRTOG_Inexact	= 0x30,
	STRTOG_Underflow= 0x40,
	STRTOG_Overflow	= 0x80
};

typedef struct
FPI {
	int nbits;
	int emin;
	int emax;
	int rounding;
	int sudden_underflow;
	int int_max;
} FPI;

enum {	/* FPI.rounding values: same as FLT_ROUNDS */
	FPI_Round_zero = 0,
	FPI_Round_near = 1,
	FPI_Round_up = 2,
	FPI_Round_down = 3
};

#ifdef __cplusplus
extern "C" {
#endif

extern char* __dtoa (double d, int mode, int ndigits, int *decpt,
		     int *sign, char **rve);
extern char* __gdtoa (FPI *fpi, int be, ULong *bits, int *kindp,
		     int mode, int ndigits, int *decpt, char **rve);
extern void __freedtoa (char *);

extern float  __strtof (const char *, char **);
extern double __strtod (const char *, char **);
extern long double __strtold (const char *, char **);
extern int __strtodg (const char *, char **, FPI *, Long *, ULong *);

extern char*	__g__fmt   (char*, char*, char*, int, ULong, size_t);
extern char*	__g_dfmt   (char*, double*, int, size_t);
extern char*	__g_ffmt   (char*, float*,  int, size_t);
extern char*	__g_xfmt   (char*, void*,   int, size_t);

#ifdef __cplusplus
}
#endif
#endif /* GDTOA_H_INCLUDED */
