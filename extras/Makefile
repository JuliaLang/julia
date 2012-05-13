JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

GLPK_VER = 4.43

GLPK_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPK_PREFIX = /usr/
else
GLPK_PREFIX = $(JULIAHOME)/deps/glpk-$(GLPK_VER)/
endif

glpk_h.jl:
	$(QUIET_PERL) cpp -dM $(GLPK_PREFIX)/include/glpk.h | perl -nle '/^\s*#define\s+(GLP\w*)\s*\(?($(GLPK_CONST))\)?\s*$$/ and print "const $$1 = int32($$2)"' | sort > $@
