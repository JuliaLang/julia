SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc
# TODO: this Makefile ignores BUILDDIR, except for computing JULIA_EXECUTABLE


all: micro kernel cat shootout blas lapack simd sort spell sparse

micro kernel cat shootout blas lapack simd sort spell sparse:
	@$(MAKE) $(QUIET_MAKE) -C $(SRCDIR)/shootout
	@$(call spawn,$(JULIA_EXECUTABLE)) $(call cygpath_w,$(SRCDIR)/$@/perf.jl) | perl -nle '@_=split/,/; printf "%-18s %8.3f %8.3f %8.3f %8.3f\n", $$_[1], $$_[2], $$_[3], $$_[4], $$_[5]'

codespeed:
	@$(MAKE) $(QUIET_MAKE) -C $(SRCDIR)/shootout
	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/micro/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/kernel/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/shootout/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/cat/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/blas/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/lapack/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/simd/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/sort/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/spell/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/sparse/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) $(SRCDIR)/report.jl


clean:
	$(MAKE) -C $(SRCDIR)/micro clean
	$(MAKE) -C $(SRCDIR)/shootout clean

.PHONY: micro kernel cat shootout blas lapack simd sort spell sparse clean
