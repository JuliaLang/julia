JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

glpk_h.jl:
	$(QUIET_PERL) cpp -dM $(GLPK_PREFIX)/include/glpk.h | perl -nle '/^\s*#define\s+(GLP\w*)\s*\(?($(GLPK_CONST))\)?\s*$$/ and print "const $$1 = int32($$2)"' | sort > $@
