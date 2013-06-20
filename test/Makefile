JULIAHOME = $(abspath ..)
include ../Make.inc

TESTS = all core keywordargs numbers strings unicode collections hashing	\
	remote iostring arrayops linalg blas fft dsp sparse bitarray		\
	random math functional bigint sorting statistics spawn parallel		\
	arpack file git pkg pkg2 resolve suitesparse complex version		\
	pollfd mpfr broadcast socket floatapprox priorityqueue

$(TESTS) ::
	$(QUIET_JULIA) $(call spawn,$(JULIA_EXECUTABLE)) ./runtests.jl $@

perf:
ifneq ($(MAKECMDGOALS),perf)
	$(QUIET_JULIA) $(call spawn,$(JULIA_EXECUTABLE)) perf/$@.jl
else
	@$(call spawn,$(JULIA_EXECUTABLE)) perf/$@.jl | perl -nle '@_=split/,/; printf "%-14s %7.3f\n", $$_[1], $$_[2]'
endif

benchmark:
	@$(MAKE) -C perf $@

clean:
	@$(MAKE) -C perf $@
	-rm -f libccalltest.${SHLIB_EXT} ccalltest

.PHONY: $(TESTS) perf benchmark clean

libccalltest.$(SHLIB_EXT): ccalltest.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -fPIC -shared -o $@ $(LDFLAGS) -DCC=$(CC)

ccalltest: ccalltest.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -o $@ $(LDFLAGS) -DCC=$(CC)
