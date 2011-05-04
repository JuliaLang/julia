#
# Default IEEE libm
#
CFLAGS = -D_IEEE_LIBM 

CC = cc

src = 	e_acosf.c   e_powf.c        s_cosf.c       \
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
	e_jnf.c     s_atanf.c       s_truncf.c        \
	e_log10f.c  s_cbrtf.c       s_log1pf.c    w_dremf.c         \
	e_log2f.c   s_ceilf.c       s_logbf.c     w_gammaf.c        \
	e_logf.c    s_copysignf.c   w_lgammaf.c   s_finitef.c

obj = 	$(src:%.c=%.o)

all: libmf.a 

libmf.a : $(obj) 
	ar cru libmf.a $(obj)
	ranlib libmf.a

source: $(src)

clean: 
	/bin/rm -f $(obj) a.out libmf.a
