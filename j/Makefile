JULIAHOME = ..
include ../Make.inc

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

all: pcre_h.j errno_h.j

pcre_h.j:
	$(QUIET_PERL) cpp -dM $(EXTROOT)/include/pcre.h | perl -nle '/^\s*#define\s+(PCRE\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "const $$1 = int32($$2)"' | sort > $@

errno_h.j:
	$(QUIET_PERL) echo '#include "errno.h"' | cpp -dM - | perl -nle 'print "const $$1 = int32($$2)" if /^#define\s+(E\w+)\s+(\d+)\s*$$/' | sort > $@

clean:
	rm -f *# *~
	rm -f pcre_h.j
	rm -f errno_h.j
