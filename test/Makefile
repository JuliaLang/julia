JULIAHOME = ..
include ../Make.inc

default::

all extra unicode::
	@$(MAKE) -sC unicode

default all extra \
core numbers strings corelib hashing arrayops \
sparse lapack fft arpack random unicode amos::
	$(QUIET_JULIA) ../julia $@.j

perf:
ifneq ($(MAKECMDGOALS),perf)
	$(QUIET_JULIA) ../julia perf/$@.j
else
	@../julia perf/$@.j | perl -nle '@_=split/,/; printf "%-14s %7.3f\n", $$_[1], $$_[2]'
endif

benchmark:
	@$(MAKE) -C perf $@

clean:
	@$(MAKE) -C unicode $@
	@$(MAKE) -C perf $@

.PHONY: \
	default all extra \
	core numbers strings corelib hashing arrayops \
	sparse lapack fft arpack random amos \
	unicode perf \
	benchmark clean
