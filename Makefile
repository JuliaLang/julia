JULIAHOME = $(shell pwd)

include ./Make.inc

default: release
	$(MAKE) -C test quick

debug release: %: julia-% sys.ji

julia-debug julia-release:
	$(MAKE) -C external
	$(MAKE) -C src lib$@
	$(MAKE) -C ui $@
	$(MAKE) -C j
	$(MAKE) -C ui/webserver $@
	ln -f $@-$(DEFAULT_REPL) julia

sys.ji: VERSION j/sysimg.j j/start_image.j src/boot.j src/dump.c j/*.j
	./julia -b sysimg.j

clean:
	rm -f julia
	rm -f *~ *#
	rm -f sys.ji
	$(MAKE) -C j clean
	$(MAKE) -C src clean
	$(MAKE) -C ui clean
	$(MAKE) -C ui/webserver clean
	$(MAKE) -C test/unicode clean
	$(MAKE) -C install clean

cleanall: clean
	$(MAKE) -C src clean-flisp clean-support

distclean: cleanall
	$(MAKE) -C external cleanall

.PHONY: default debug release julia-debug julia-release \
	test testall test-* sloccount clean cleanall

test: release
	$(MAKE) -C test quick core

testall: release
	$(MAKE) -C test all

test-%: release
	$(MAKE) -C test $*

## SLOCCOUNT ##

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
