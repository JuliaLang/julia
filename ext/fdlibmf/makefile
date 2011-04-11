#
# Default IEEE libm
#
CFLAGS = -D_IEEE_LIBM 

CC = cc

src = 	e_acosf.c       e_rem_pio2f.c  s_catanhf.c    s_ctanf.c     \
	e_acoshf.c      e_scalbf.c     s_cbrtf.c      s_ctanhf.c    \
	e_asinf.c       e_sinhf.c      s_ccosf.c       s_modff.c \
	e_atan2f.c      e_sqrtf.c      s_ccoshf.c     s_erff.c      s_nextafterf.c \
	e_atanhf.c      k_cosf.c       s_ceilf.c      s_exp2f.c     s_remquof.c \
	e_coshf.c       k_rem_pio2f.c  s_cexpf.c      s_expm1f.c    s_rintf.c \
	e_expf.c        k_sinf.c       s_cimagf.c     s_fabsf.c     s_roundf.c \
	e_fmodf.c       k_tanf.c       s_clogf.c      s_floorf.c    s_scalbnf.c \
	e_hypotf.c      s_asinhf.c     s_conjf.c      s_fmaxf.c     s_significandf.c \
	e_j0f.c         s_atanf.c      s_copysignf.c  s_fminf.c     s_sinf.c \
	e_j1f.c         s_cabsf.c      s_cosf.c       s_frexpf.c    s_tanf.c \
	e_jnf.c         s_cacosf.c     s_cpowf.c      s_ilogbf.c    s_tanhf.c \
	e_log10f.c      s_cacoshf.c    s_cprojf.c     s_truncf.c \
	e_log2f.c       s_cargf.c      s_crealf.c     w_dremf.c \
	e_logf.c        s_casinf.c     s_csinf.c      s_log1pf.c    w_gammaf.c \
	e_powf.c        s_casinhf.c    s_csinhf.c     s_logbf.c     w_lgammaf.c \
	e_remainderf.c  s_catanf.c     s_csqrtf.c

obj = 	$(src:%.c=%.o)

all: libmf.a 

libmf.a : $(obj) 
	ar cru libmf.a $(obj)
	ranlib libmf.a

source: $(src)

clean: 
	/bin/rm -f $(obj) a.out libmf.a
