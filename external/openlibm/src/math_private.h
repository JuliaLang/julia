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

/*
 * from: @(#)fdlibm.h 5.1 93/09/24
 * $FreeBSD: src/lib/msun/src/math_private.h,v 1.31 2010/12/05 22:11:22 das Exp $
 */

#ifndef _MATH_PRIVATE_H_
#define	_MATH_PRIVATE_H_

#include <sys/types.h>
#include <machine/endian.h>

#ifdef __APPLE__
#define uint32_t u_int32_t 
#endif 

/*
 * The original fdlibm code used statements like:
 *	n0 = ((*(int*)&one)>>29)^1;		* index of high word *
 *	ix0 = *(n0+(int*)&x);			* high word of x *
 *	ix1 = *((1-n0)+(int*)&x);		* low word of x *
 * to dig two 32 bit words out of the 64 bit IEEE floating point
 * value.  That is non-ANSI, and, moreover, the gcc instruction
 * scheduler gets it wrong.  We instead use the following macros.
 * Unlike the original code, we determine the endianness at compile
 * time, not at run time; I don't see much benefit to selecting
 * endianness at run time.
 */

/*
 * A union which permits us to convert between a double and two 32 bit
 * ints.
 */

#ifdef __arm__
#if defined(__VFP_FP__)
#define	IEEE_WORD_ORDER	BYTE_ORDER
#else
#define	IEEE_WORD_ORDER	BIG_ENDIAN
#endif
#else /* __arm__ */
#define	IEEE_WORD_ORDER	BYTE_ORDER
#endif

#if IEEE_WORD_ORDER == BIG_ENDIAN

typedef union
{
  double value;
  struct
  {
    u_int32_t msw;
    u_int32_t lsw;
  } parts;
} ieee_double_shape_type;

#endif

#if IEEE_WORD_ORDER == LITTLE_ENDIAN

typedef union
{
  double value;
  struct
  {
    u_int32_t lsw;
    u_int32_t msw;
  } parts;
} ieee_double_shape_type;

#endif

/* Get two 32 bit ints from a double.  */

#define EXTRACT_WORDS(ix0,ix1,d)				\
do {								\
  ieee_double_shape_type ew_u;					\
  ew_u.value = (d);						\
  (ix0) = ew_u.parts.msw;					\
  (ix1) = ew_u.parts.lsw;					\
} while (0)

/* Get the more significant 32 bit int from a double.  */

#define GET_HIGH_WORD(i,d)					\
do {								\
  ieee_double_shape_type gh_u;					\
  gh_u.value = (d);						\
  (i) = gh_u.parts.msw;						\
} while (0)

/* Get the less significant 32 bit int from a double.  */

#define GET_LOW_WORD(i,d)					\
do {								\
  ieee_double_shape_type gl_u;					\
  gl_u.value = (d);						\
  (i) = gl_u.parts.lsw;						\
} while (0)

/* Set a double from two 32 bit ints.  */

#define INSERT_WORDS(d,ix0,ix1)					\
do {								\
  ieee_double_shape_type iw_u;					\
  iw_u.parts.msw = (ix0);					\
  iw_u.parts.lsw = (ix1);					\
  (d) = iw_u.value;						\
} while (0)

/* Set the more significant 32 bits of a double from an int.  */

#define SET_HIGH_WORD(d,v)					\
do {								\
  ieee_double_shape_type sh_u;					\
  sh_u.value = (d);						\
  sh_u.parts.msw = (v);						\
  (d) = sh_u.value;						\
} while (0)

/* Set the less significant 32 bits of a double from an int.  */

#define SET_LOW_WORD(d,v)					\
do {								\
  ieee_double_shape_type sl_u;					\
  sl_u.value = (d);						\
  sl_u.parts.lsw = (v);						\
  (d) = sl_u.value;						\
} while (0)

/*
 * A union which permits us to convert between a float and a 32 bit
 * int.
 */

typedef union
{
  float value;
  /* FIXME: Assumes 32 bit int.  */
  unsigned int word;
} ieee_float_shape_type;

/* Get a 32 bit int from a float.  */

#define GET_FLOAT_WORD(i,d)					\
do {								\
  ieee_float_shape_type gf_u;					\
  gf_u.value = (d);						\
  (i) = gf_u.word;						\
} while (0)

/* Set a float from a 32 bit int.  */

#define SET_FLOAT_WORD(d,i)					\
do {								\
  ieee_float_shape_type sf_u;					\
  sf_u.word = (i);						\
  (d) = sf_u.value;						\
} while (0)

#ifdef FLT_EVAL_METHOD
/*
 * Attempt to get strict C99 semantics for assignment with non-C99 compilers.
 */
#if FLT_EVAL_METHOD == 0 || __GNUC__ == 0
#define	STRICT_ASSIGN(type, lval, rval)	((lval) = (rval))
#else
#define	STRICT_ASSIGN(type, lval, rval) do {	\
	volatile type __lval;			\
						\
	if (sizeof(type) >= sizeof(double))	\
		(lval) = (rval);		\
	else {					\
		__lval = (rval);		\
		(lval) = __lval;		\
	}					\
} while (0)
#endif
#endif

/*
 * Common routine to process the arguments to nan(), nanf(), and nanl().
 */
void _scan_nan(uint32_t *__words, int __num_words, const char *__s);

#ifdef _COMPLEX_H

/*
 * C99 specifies that complex numbers have the same representation as
 * an array of two elements, where the first element is the real part
 * and the second element is the imaginary part.
 */
typedef union {
	float complex f;
	float a[2];
} float_complex;
typedef union {
	double complex f;
	double a[2];
} double_complex;
typedef union {
	long double complex f;
	long double a[2];
} long_double_complex;
#define	REALPART(z)	((z).a[0])
#define	IMAGPART(z)	((z).a[1])

/*
 * Inline functions that can be used to construct complex values.
 *
 * The C99 standard intends x+I*y to be used for this, but x+I*y is
 * currently unusable in general since gcc introduces many overflow,
 * underflow, sign and efficiency bugs by rewriting I*y as
 * (0.0+I)*(y+0.0*I) and laboriously computing the full complex product.
 * In particular, I*Inf is corrupted to NaN+I*Inf, and I*-0 is corrupted
 * to -0.0+I*0.0.
 */
static __inline float complex
cpackf(float x, float y)
{
	float_complex z;

	REALPART(z) = x;
	IMAGPART(z) = y;
	return (z.f);
}

static __inline double complex
cpack(double x, double y)
{
	double_complex z;

	REALPART(z) = x;
	IMAGPART(z) = y;
	return (z.f);
}

static __inline long double complex
cpackl(long double x, long double y)
{
	long_double_complex z;

	REALPART(z) = x;
	IMAGPART(z) = y;
	return (z.f);
}
#endif /* _COMPLEX_H */
 
#ifdef __GNUCLIKE_ASM

/* Asm versions of some functions. */

#ifdef __amd64__
static __inline int
irint(double x)
{
	int n;

	asm("cvtsd2si %1,%0" : "=r" (n) : "x" (x));
	return (n);
}
#define	HAVE_EFFICIENT_IRINT
#endif

#ifdef __i386__
static __inline int
irint(double x)
{
	int n;

	asm("fistl %0" : "=m" (n) : "t" (x));
	return (n);
}
#define	HAVE_EFFICIENT_IRINT
#endif

#endif /* __GNUCLIKE_ASM */

/*
 * ieee style elementary functions
 *
 * We rename functions here to improve other sources' diffability
 * against fdlibm.
 */
#define	__ieee754_sqrt	sqrt
#define	__ieee754_acos	acos
#define	__ieee754_acosh	acosh
#define	__ieee754_log	log
#define	__ieee754_log2	log2
#define	__ieee754_atanh	atanh
#define	__ieee754_asin	asin
#define	__ieee754_atan2	atan2
#define	__ieee754_exp	exp
#define	__ieee754_cosh	cosh
#define	__ieee754_fmod	fmod
#define	__ieee754_pow	pow
#define	__ieee754_lgamma lgamma
#define	__ieee754_gamma	gamma
#define	__ieee754_lgamma_r lgamma_r
#define	__ieee754_gamma_r gamma_r
#define	__ieee754_log10	log10
#define	__ieee754_sinh	sinh
#define	__ieee754_hypot	hypot
#define	__ieee754_j0	j0
#define	__ieee754_j1	j1
#define	__ieee754_y0	y0
#define	__ieee754_y1	y1
#define	__ieee754_jn	jn
#define	__ieee754_yn	yn
#define	__ieee754_remainder remainder
#define	__ieee754_scalb	scalb
#define	__ieee754_sqrtf	sqrtf
#define	__ieee754_acosf	acosf
#define	__ieee754_acoshf acoshf
#define	__ieee754_logf	logf
#define	__ieee754_atanhf atanhf
#define	__ieee754_asinf	asinf
#define	__ieee754_atan2f atan2f
#define	__ieee754_expf	expf
#define	__ieee754_coshf	coshf
#define	__ieee754_fmodf	fmodf
#define	__ieee754_powf	powf
#define	__ieee754_lgammaf lgammaf
#define	__ieee754_gammaf gammaf
#define	__ieee754_lgammaf_r lgammaf_r
#define	__ieee754_gammaf_r gammaf_r
#define	__ieee754_log10f log10f
#define	__ieee754_log2f log2f
#define	__ieee754_sinhf	sinhf
#define	__ieee754_hypotf hypotf
#define	__ieee754_j0f	j0f
#define	__ieee754_j1f	j1f
#define	__ieee754_y0f	y0f
#define	__ieee754_y1f	y1f
#define	__ieee754_jnf	jnf
#define	__ieee754_ynf	ynf
#define	__ieee754_remainderf remainderf
#define	__ieee754_scalbf scalbf

/* fdlibm kernel function */
int	__kernel_rem_pio2(double*,double*,int,int,int);

/* double precision kernel functions */
#ifdef INLINE_REM_PIO2
__inline
#endif
int	__ieee754_rem_pio2(double,double*);
double	__kernel_sin(double,double,int);
double	__kernel_cos(double,double);
double	__kernel_tan(double,double,int);

/* float precision kernel functions */
#ifdef INLINE_REM_PIO2F
__inline
#endif
int	__ieee754_rem_pio2f(float,double*);
#ifdef INLINE_KERNEL_SINDF
__inline
#endif
float	__kernel_sindf(double);
#ifdef INLINE_KERNEL_COSDF
__inline
#endif
float	__kernel_cosdf(double);
#ifdef INLINE_KERNEL_TANDF
__inline
#endif
float	__kernel_tandf(double,int);

/* long double precision kernel functions */
long double __kernel_sinl(long double, long double, int);
long double __kernel_cosl(long double, long double);
long double __kernel_tanl(long double, long double, int);

#endif /* !_MATH_PRIVATE_H_ */
