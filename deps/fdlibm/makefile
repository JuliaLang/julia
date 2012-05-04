#
#  @(#)Makefile 1.4 95/01/18 
# 
#  ====================================================
#  Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
# 
#  Developed at SunSoft, a Sun Microsystems, Inc. business.
#  Permission to use, copy, modify, and distribute this
#  software is freely granted, provided that this notice 
#  is preserved.
#  ====================================================
# 
# 

#
# There are two options in making libm at fdlibm compile time:
# 	_IEEE_LIBM 	--- IEEE libm; smaller, and somewhat faster
#	_MULTI_LIBM	--- Support multi-standard at runtime by 
#			    imposing wrapper functions defined in 
#			    fdlibm.h:
#				_IEEE_MODE 	-- IEEE
#				_XOPEN_MODE 	-- X/OPEN
#				_POSIX_MODE 	-- POSIX/ANSI
#				_SVID3_MODE 	-- SVID
#
# Here is how to set up CFLAGS to create the desired libm at 
# compile time:
#
# 	CFLAGS = -D_IEEE_LIBM		... IEEE libm (recommended)
#	CFLAGS = -D_SVID3_MODE	... Multi-standard supported
#					    libm with SVID as the 
#					    default standard
#	CFLAGS = -D_XOPEN_MODE	... Multi-standard supported
#					    libm with XOPEN as the 
#					    default standard
#	CFLAGS = -D_POSIX_MODE	... Multi-standard supported
#					    libm with POSIX as the 
#					    default standard
#	CFLAGS = 			... Multi-standard supported
#					    libm with IEEE as the 
#					    default standard
# 
# NOTE: if scalb's second arguement is an int, then one must
# define _SCALB_INT in CFLAGS. The default prototype of scalb
# is double scalb(double, double)
#


#
# Default IEEE libm
#
CFLAGS = -D_IEEE_LIBM 

CC = cc

INCFILES = fdlibm.h
.INIT: $(INCFILES)
.KEEP_STATE:
src = 	k_standard.c k_rem_pio2.c \
	k_cos.c k_sin.c k_tan.c \
	e_acos.c e_acosh.c e_asin.c e_atan2.c \
	e_atanh.c e_cosh.c e_exp.c e_fmod.c \
	e_gamma.c e_gamma_r.c e_hypot.c e_j0.c \
	e_j1.c e_jn.c e_lgamma.c e_lgamma_r.c \
	e_log.c e_log10.c e_log2.c e_pow.c e_rem_pio2.c e_remainder.c \
	e_scalb.c e_sinh.c e_sqrt.c \
	w_acos.c w_acosh.c w_asin.c w_atan2.c \
	w_atanh.c w_cosh.c w_exp.c w_fmod.c \
	w_gamma.c w_gamma_r.c w_hypot.c w_j0.c \
	w_j1.c w_jn.c w_lgamma.c w_lgamma_r.c \
	w_log.c w_log10.c w_pow.c w_remainder.c \
	w_scalb.c w_sinh.c w_sqrt.c \
	s_asinh.c s_atan.c s_cbrt.c s_ceil.c s_copysign.c \
	s_cos.c s_erf.c s_expm1.c s_fabs.c s_finite.c s_floor.c \
	s_frexp.c s_ilogb.c s_isnan.c s_ldexp.c s_lib_version.c \
	s_log1p.c s_logb.c s_matherr.c s_modf.c s_nextafter.c \
	s_rint.c s_scalbn.c s_signgam.c s_significand.c s_sin.c \
	s_tan.c s_tanh.c \
	e_acosf.c   e_powf.c        s_cosf.c       \
	e_acoshf.c  e_remainderf.c  s_erff.c      s_modff.c         \
	e_asinf.c   e_rem_pio2f.c   s_exp2f.c     s_nextafterf.c    \
	e_atan2f.c  e_scalbf.c      s_expm1f.c    s_remquof.c       \
	e_atanhf.c  e_sinhf.c       s_fabsf.c     s_rintf.c         \
	e_coshf.c   e_sqrtf.c       s_floorf.c      \
	e_expf.c    k_cosf.c        s_fmaxf.c     s_scalbnf.c       \
	e_fmodf.c   k_rem_pio2f.c   s_fminf.c     s_significandf.c  \
	e_hypotf.c  k_sinf.c        s_frexpf.c    s_sinf.c          \
	e_j0f.c     k_tanf.c        s_ilogbf.c    s_tanf.c          \
	e_j1f.c     s_asinhf.c      s_tanhf.c         \
	e_jnf.c     s_atanf.c       s_truncf.c    s_trunc.c    \
	e_log10f.c  s_cbrtf.c       s_log1pf.c    w_dremf.c         \
	e_log2f.c   s_ceilf.c       s_logbf.c     w_gammaf.c        \
	e_logf.c    s_copysignf.c   w_lgammaf.c   s_finitef.c       \
	e_lgammaf_r.c 	b_exp.c	    b_log.c	b_tgamma.c \
	s_signbit.c

obj = 	$(src:%.c=%.o)

all: libfdm.a 

libfdm.a : $(obj) 
	ar cru libfdm.a $(obj)

source: $(src) readme

clean: 
	/bin/rm -f $(obj) a.out libfdm.a
