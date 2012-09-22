JULIAHOME = ..
include ../Make.inc

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

all: pcre_h.jl errno_h.jl os_detect.jl build_h.jl

pcre_h.jl:
	$(QUIET_PERL) ${CC} -E -dM $(shell $(PCRE_CONFIG) --prefix)/include/pcre.h | perl -nle '/^\s*#define\s+PCRE_(\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "const $$1 = int32($$2)"' | sort > $@

errno_h.jl:
	$(QUIET_PERL) echo '#include "errno.h"' | cpp -dM - | perl -nle 'print "const $$1 = int32($$2)" if /^#define\s+(E\w+)\s+(\d+)\s*$$/' | sort > $@

os_detect.jl: ../src/os_detect.h
	$(QUIET_PERL) ${CC} -E -P -DJULIA ../src/os_detect.h | perl -p -e 's/\\n/\n/g' > $@

build_h.jl: ../Make.inc
	@echo "libblas_name = \"$(LIBBLASNAME)\"" > $@
	@echo "liblapack_name = \"$(LIBLAPACKNAME)\"" >> $@

clean:
	rm -f *# *~
	rm -f pcre_h.jl
	rm -f errno_h.jl
	rm -f os_detect.jl
	rm -f build_h.jl
