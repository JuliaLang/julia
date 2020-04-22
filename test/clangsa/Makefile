SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

check: $(SRCDIR)

TESTS = $(patsubst $(SRCDIR)/%,%,$(wildcard $(SRCDIR)/*.c)) $(wildcard $(SRCDIR)/*.cpp)

$(SRCDIR) $(TESTS):
	PATH=$(build_bindir):$(build_depsbindir):$$PATH \
	LD_LIBRARY_PATH="${build_libdir}:$$LD_LIBRARY_PATH" \
	CLANGSA_FLAGS="${CLANGSA_FLAGS}" \
	CLANGSACXX_FLAGS="${CLANGSACXX_FLAGS}" \
	CPPFLAGS_FLAGS="${CPPFLAGS_FLAGS}" \
	CFLAGS_FLAGS="${CFLAGS_FLAGS}" \
	CXXFLAGS_FLAGS="${CXXFLAGS_FLAGS}" \
	$(build_depsbindir)/lit/lit.py -v $@

.PHONY: $(TESTS) $(SRCDIR) check all
