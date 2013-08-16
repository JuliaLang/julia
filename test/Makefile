JULIAHOME = $(abspath ..)
include ../Make.inc

TESTS = all core keywordargs numbers strings unicode collections hashing	\
	remote iostring arrayops linalg blas fft dsp sparse bitarray random	    \
	math functional bigint sorting statistics spawn parallel arpack file	\
	git pkg resolve suitesparse complex version pollfd mpfr	broadcast       \
	socket floatapprox priorityqueue readdlm regex float16 combinatorics

$(TESTS) ::
	@$(PRINT_JULIA) $(call spawn,$(JULIA_EXECUTABLE)) ./runtests.jl $@

perf:
	@$(MAKE) -C perf all

clean:
	@$(MAKE) -C perf $@
	-rm -f libccalltest.${SHLIB_EXT} ccalltest

.PHONY: $(TESTS) perf clean

libccalltest.$(SHLIB_EXT): ccalltest.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -fPIC -shared -o $@ $(LDFLAGS) -DCC=$(CC)

ccalltest: ccalltest.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -o $@ $(LDFLAGS) -DCC=$(CC)
