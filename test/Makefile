SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc
# TODO: this Makefile ignores BUILDDIR, except for computing JULIA_EXECUTABLE

TESTS = all linalg $(filter-out TestHelpers runtests testdefs,$(subst .jl,,$(wildcard *.jl linalg/*.jl)))

default: all

$(TESTS):
	@cd $(SRCDIR) && $(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --check-bounds=yes --startup-file=no ./runtests.jl $@)

perf:
	@$(MAKE) -C $(SRCDIR)/perf all

clean:
	@$(MAKE) -C $(SRCDIR)/perf clean
	-rm -f $(SRCDIR)/libccalltest.$(SHLIB_EXT) $(SRCDIR)/ccalltest

.PHONY: $(TESTS) perf clean libccalltest

all ccall libccalltest: $(SRCDIR)/libccalltest.$(SHLIB_EXT)
$(SRCDIR)/libccalltest.$(SHLIB_EXT): $(SRCDIR)/ccalltest.c
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< $(fPIC) -shared -o $@ $(LDFLAGS) -DCC="$(CC)")

$(SRCDIR)/ccalltest: $(SRCDIR)/ccalltest.c $(SRCDIR)/libccalltest.$(SHLIB_EXT)
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -o $@ $(LDFLAGS) -DCC="$(CC)")
