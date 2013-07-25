JULIAHOME = $(abspath ../..)
include ../../Make.inc

all: micro kernel cat shootout blas sort

micro kernel cat shootout blas sort:
	@$(MAKE) $(QUIET_MAKE) -C shootout
	@$(call spawn,$(JULIA_EXECUTABLE)) $@/perf.jl | perl -nle '@_=split/,/; printf "%-18s %8.3f %8.3f %8.3f %8.3f\n", $$_[1], $$_[2], $$_[3], $$_[4], $$_[5]'

codespeed:
	@$(MAKE) $(QUIET_MAKE) -C shootout
	@$(call spawn,$(JULIA_EXECUTABLE)) micro/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) kernel/perf.jl codespeed
	@$(call spawn,$(JULIA_EXECUTABLE)) shootout/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) cat/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) blas/perf.jl codespeed
#	@$(call spawn,$(JULIA_EXECUTABLE)) sort/perf.jl codespeed


clean:
	rm -f *~
	$(MAKE) -C micro $@
	$(MAKE) -C shootout $@

.PHONY: micro kernel cat shootout blas sort clean
