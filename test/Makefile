JULIAHOME = ..
include ../Make.inc

default all extra unicode gzip::
	@$(MAKE) -sC unicode

TESTS = default all extra \
core numbers strings unicode corelib hashing \
arrayops linalg fft sparse arpack bitarray \
random math functional bigint dists combinatorics statistics \
glpk linprog bigfloat poly file Rmath remote zlib sound image \
iterators gzip

$(TESTS) ::
	$(QUIET_JULIA) $(JULIA_EXECUTABLE) ./runtests.jl $@

perf:
ifneq ($(MAKECMDGOALS),perf)
	$(QUIET_JULIA) $(JULIA_EXECUTABLE) perf/$@.jl
else
	@$(JULIA_EXECUTABLE) perf/$@.jl | perl -nle '@_=split/,/; printf "%-14s %7.3f\n", $$_[1], $$_[2]'
endif

benchmark:
	@$(MAKE) -C perf $@

clean:
	@$(MAKE) -C unicode $@
	@$(MAKE) -C perf $@

.PHONY: $(TESTS) perf benchmark clean

