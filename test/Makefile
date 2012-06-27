JULIAHOME = ..
include ../Make.inc

default all extra unicode gzip::

TESTS = default all extra \
core numbers strings unicode corelib hashing remote \
arrayops linalg fft dct sparse bitarray suitesparse arpack \
random math functional bigint bigfloat combinatorics \
statistics glpk linprog poly file Rmath remote zlib image \
iostring gzip

$(TESTS) :: libccall.$(SHLIB_EXT) ccall
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
	@$(MAKE) -C perf $@
	-rm -f libccall.${SHLIB_EXT} ccall

.PHONY: $(TESTS) perf benchmark clean

libccall.$(SHLIB_EXT): ccall.c
	    $(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -shared -o $@ $(LDFLAGS) -DCC=$(CC)

ccall: ccall.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -o $@ $(LDFLAGS) -DCC=$(CC)

