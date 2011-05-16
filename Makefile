JULIAHOME = $(shell pwd)

include ./Make.inc

default: release

debug release: %: julia-% j/pcre_h.j sys.ji custom.j

julia-debug julia-release:
	$(MAKE) -C src lib$@
#	ln -f lib$@.$(SHLIB_EXT) libjulia.$(SHLIB_EXT)
	$(MAKE) -C ui $@
	ln -f ui/$@-$(DEFAULT_REPL) julia
	ln -f ui/$@-cloud .
	ln -f ui/$@-readline .
	ln -f ui/$@-basic .

sys.ji: ./j/sysimg.j ./j/start_image.j src/boot.j src/dump.c j/*.j
	./julia -b sysimg.j

custom.j:
	if [ ! -f ./custom.j ]; then touch ./custom.j; fi

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

j/pcre_h.j:
	cpp -dM $(EXTROOT)/include/pcre.h | perl -nle '/^\s*#define\s+(PCRE\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "$$1 = $$2"' | sort > $@

test: debug
	./julia test/tests.j

test-utf8:
	./julia test/test_utf8.j

perf: release
	./julia test/perf.j

testall: test test-utf8 perf

SLOCCOUNT = sloccount \
	--addlang makefile \
	--personcost 100000 \
	--effort 3.6 1.2 \
	--schedule 2.5 0.32 \
	--

J_FILES = $(shell git ls-files | grep '\.j$$')

sloccount:
	@for x in $(J_FILES); do cp $$x $${x%.j}.hs; done
	git ls-files | sed 's/\.j$$/.hs/' | xargs $(SLOCCOUNT) | sed 's/haskell/*julia*/g'
	@for x in $(J_FILES); do rm -f $${x%.j}.hs; done

clean:
	rm -f julia
	rm -f julia-{debug,release}-{basic,cloud,readline}
	rm -f libjulia-*.$(SHLIB_EXT)
	rm -f j/pcre_h.j
	rm -f *.ji
	rm -f *~ *#
	$(MAKE) -C src clean
	$(MAKE) -C ui clean

cleanall: clean
	$(MAKE) -C src cleanother

.PHONY: default debug release julia-debug julia-release test test-* testall sloccount clean cleanall
