/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998, 2000 by Lucent Technologies
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

#include "gdtoaimp.h"

#undef _0
#undef _1

/* one or the other of IEEE_MC68k or IEEE_8087 should be #defined */

#ifdef IEEE_MC68k
#define _0 0
#define _1 1
#define _2 2
#define _3 3
#define _4 4
#endif
#ifdef IEEE_8087
#define _0 4
#define _1 3
#define _2 2
#define _3 1
#define _4 0
#endif

/* This is specific to the x86 80 bit long doubles. */
#if defined(_AMD64_) || defined(__x86_64__) || \
  defined(_X86_) || defined(__i386__)

typedef union lD {
	UShort L[5];
	long double D;
} lD;

static int __strtopx (const char *s, char **sp, lD *V)
{
	static FPI fpi0 = { 64, 1-16383-64+1, 32766 - 16383 - 64 + 1, 1, SI,
			   Int_max };
	ULong bits[2];
	Long expo;
	int k;
	UShort *L = & (V->L[0]);
#ifdef Honor_FLT_ROUNDS
#include "gdtoa_fltrnds.h"
#else
#define fpi &fpi0
#endif
	V->D = 0.0L;

	k = __strtodg(s, sp, fpi, &expo, bits);
	switch(k & STRTOG_Retmask) {
	  case STRTOG_NoNumber:
	  case STRTOG_Zero:
		L[0] = L[1] = L[2] = L[3] = L[4] = 0;
		break;

	  case STRTOG_Denormal:
		L[_0] = 0;
		goto normal_bits;

	  case STRTOG_Normal:
	  case STRTOG_NaNbits:
		L[_0] = expo + 0x3fff + 63;
 normal_bits:
		L[_4] = (UShort)bits[0];
		L[_3] = (UShort)(bits[0] >> 16);
		L[_2] = (UShort)bits[1];
		L[_1] = (UShort)(bits[1] >> 16);
		break;

	  case STRTOG_Infinite:
		L[_0] = 0x7fff;
		L[_1] = 0x8000;
		L[_2] = L[_3] = L[_4] = 0;
		break;

	  case STRTOG_NaN:
		L[0] = ldus_QNAN0;
		L[1] = ldus_QNAN1;
		L[2] = ldus_QNAN2;
		L[3] = ldus_QNAN3;
		L[4] = ldus_QNAN4;
	}
	if (k & STRTOG_Neg)
		L[_0] |= 0x8000;
	return k;
}

long double __cdecl
__strtold (const char * __restrict__ src, char ** __restrict__ endptr)
{
	lD ret;
	ret.D = 0.0L;
	__strtopx(src, endptr,  &ret);
	return ret.D;
}

long double __cdecl
__mingw_strtold (const char * __restrict__ src, char ** __restrict__ endptr)
  __attribute__((alias("__strtold")));

long double __cdecl
strtold (const char * __restrict__ src, char ** __restrict__ endptr)
  __attribute__((alias("__strtold")));

#elif defined(__arm__) || defined(__aarch64__) || defined(_ARM_) || defined(_ARM64_)
/* For ARM, where long double == double, provide the long double function as
 * an alias for __strtod. Do this in a separate object file from other
 * functions, to avoid linker conflicts if object files import both 'strtold'
 * from libucrt*.a and the object file providing '__strtod'. */
long double __cdecl
strtold (const char * __restrict__ src, char ** __restrict__ endptr)
{
  return __mingw_strtod(src, endptr);
}
#endif
