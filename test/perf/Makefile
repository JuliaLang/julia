JULIAHOME = $(abspath ../..)
include ../../Make.inc

all: micro kernel cat shootout

micro kernel cat shootout:
	@$(MAKE) $(QUIET_MAKE) -C shootout
	@$(call spawn,$(JULIA_EXECUTABLE)) $@/perf.jl | perl -nle '@_=split/,/; printf "%-18s %8.3f %8.3f %8.3f %8.3f\n", $$_[1], $$_[2], $$_[3], $$_[4], $$_[5]'

clean:
	rm -f *~
	$(MAKE) -C micro $@
	$(MAKE) -C shootout $@

.PHONY: micro kernel cat shootout clean
