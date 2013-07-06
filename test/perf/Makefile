JULIAHOME = $(abspath ../..)
include ../../Make.inc

all: micro kernel cat shootout

micro kernel cat:
	@$(call spawn,$(JULIA_EXECUTABLE)) $@/perf.jl | perl -nle '@_=split/,/; printf "%-18s %8.3f\n", $$_[1], $$_[2]'

shootout:
	$(MAKE) -C shootout getall
	@$(call spawn,$(JULIA_EXECUTABLE)) $@/perf.jl | perl -nle '@_=split/,/; printf "%-18s %8.3f\n", $$_[1], $$_[2]'

clean:
	rm -f *~
	$(MAKE) -C micro $@
	$(MAKE) -C shootout $@

.PHONY: micro kernel cat shootout clean
