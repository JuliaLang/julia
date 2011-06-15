src = 	e_remainderf.S  e_sqrtl.S    s_llrint.S  s_lrint.S    s_rintl.S \
	e_remainderl.S  e_sqrt.S     s_logbl.S   s_remquof.S  s_scalbnf.S \
	e_remainder.S   s_llrintf.S  s_lrintf.S  s_remquol.S  s_scalbnl.S \
	e_sqrtf.S       s_llrintl.S  s_lrintl.S  s_remquo.S   s_scalbn.S \
	s_fabs.S	s_fabsf.S

obj = 	$(src:%.S=%.o)

%.o: %.S
	gcc -m64 -fPIC -c $< -o $@

TARGET = libm64.so

all: $(TARGET)

$(TARGET): $(obj)
	gcc -nostdlib -m64 -shared -o $@ $(obj)

clean: 
	/bin/rm -f $(obj) $(TARGET)
