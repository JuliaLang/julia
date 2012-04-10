JULIAHOME = ..
include ../Make.inc

default::

all extra unicode::
	@$(MAKE) -sC unicode

default all extra \
core numbers strings corelib hashing arrayops \
glpk linprog sparse lapack fft arpack random unicode \
special bigint bigfloat bitarray \
::
	$(QUIET_JULIA) ../julia ./runtests.jl $@

perf:
ifneq ($(MAKECMDGOALS),perf)
	$(QUIET_JULIA) ../julia perf/$@.jl
else
	@../julia perf/$@.jl | perl -nle '@_=split/,/; printf "%-14s %7.3f\n", $$_[1], $$_[2]'
endif

benchmark:
	@$(MAKE) -C perf $@

clean:
	@$(MAKE) -C unicode $@
	@$(MAKE) -C perf $@

.PHONY: \
	default all extra \
	core numbers strings corelib hashing arrayops glpk \
	linprog sparse lapack fft arpack random special \
	unicode perf bitarray \
	benchmark clean
