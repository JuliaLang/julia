JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: glpk_h.jl webrepl_msgtypes_h.jl

GLPK_VER = 4.47

GLPK_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPK_PREFIX = /usr/include
else
GLPK_PREFIX = $(JULIAHOME)/deps/glpk-$(GLPK_VER)/src
endif

glpk_h.jl:
	$(QUIET_PERL) $(CC) -E -dM $(GLPK_PREFIX)/glpk.h | perl -nle '/^\s*#define\s+GLP_(\w*)\s*\(?($(GLPK_CONST))\)?\s*$$/ and print "const $$1 = int32($$2)"' | sort > $@

webrepl_msgtypes_h.jl: ../ui/webserver/message_types.h
	$(QUIET_PERL) $(CC) -E -Dnotdefined $^ > $@

clean:
	rm -f glpk_h.jl julia_message_types_h.jl
