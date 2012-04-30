src = 	e_exp.S         e_sqrtl.S      s_floorf.S   s_lrintl.S   s_scalbn.S         \
	e_fmod.S        e_sqrt.S       s_floorl.S   s_lrint.S    s_significandf.S   \
	e_log10f.S      s_ceilf.S      s_floor.S    s_remquof.S  s_significand.S    \
	e_log10.S       s_ceill.S      s_llrintf.S  s_remquol.S  s_sin.S \
	e_logf.S        s_ceil.S       s_llrintl.S  s_remquo.S   s_tan.S \
	e_log.S         s_copysignf.S  s_llrint.S   s_rintf.S    s_truncf.S \
	e_remainderf.S  s_copysignl.S  s_logbf.S    s_rintl.S    s_truncl.S \
	e_remainderl.S  s_copysign.S   s_logbl.S    s_rint.S     s_trunc.S \
	e_remainder.S   s_cos.S        s_logb.S     s_scalbnf.S \
	e_sqrtf.S       s_finite.S     s_lrintf.S   s_scalbnl.S \
	s_fabs.S        s_fabsf.S


obj = 	$(src:%.S=%.o)

%.o: %.S
	gcc -m32 -fPIC -c $< -o $@

TARGET = libm387.so

all: $(TARGET)

$(TARGET): $(obj)
	gcc -nostdlib -m32 -shared -o $@ $(obj)

clean: 
	/bin/rm -f $(obj) $(TARGET)
