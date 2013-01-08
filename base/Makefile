JULIAHOME = ..
include ../Make.inc

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

all: pcre_h.jl errno_h.jl build_h.jl file_constants.jl

pcre_h.jl:
	$(QUIET_PERL) $(CPP) -dM $(shell $(PCRE_CONFIG) --prefix)/include/pcre.h | perl -nle '/^\s*#define\s+PCRE_(\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "const $$1 = uint32($$2)"' | sort > $@

errno_h.jl:
	$(QUIET_PERL) echo '#include "errno.h"' | $(CC) -E -dM - | perl -nle 'print "const $$1 = int32($$2)" if /^#define\s+(E\w+)\s+(\d+)\s*$$/' | sort > $@

file_constants.jl: ../src/file_constants.h
	$(QUIET_PERL) ${CC} -E -P -DJULIA ../src/file_constants.h | perl -nle 'print "$$1" if /^(\s*const\s+[A-z_]+\s+=\s+[0-9A-z_]+\s*)$$/' > $@

build_h.jl: ../Make.inc ../src/os_detect.h
	$(QUIET_PERL) $(CC) -E -P -DJULIA ../src/os_detect.h | grep OS_NAME > $@
	@echo "const libm_name = \"$(LIBMNAME)\"" >> $@
	@echo "const libblas_name = \"$(LIBBLASNAME)\"" >> $@
	@echo "const liblapack_name = \"$(LIBLAPACKNAME)\"" >> $@
ifeq ($(USE_LIB64), 1)
	@echo "const USE_LIB64 = true" >> $@
else
	@echo "const USE_LIB64 = false" >> $@
endif


clean:
	rm -f *# *~
	rm -f pcre_h.jl
	rm -f errno_h.jl
	rm -f build_h.jl
