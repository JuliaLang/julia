JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

default: release

debug release: %: julia-% sys.ji

julia-debug julia-release:
	@$(MAKE) -sC external
	@$(MAKE) -sC src lib$@
	@$(MAKE) -sC ui $@
	@$(MAKE) -sC j
	@ln -f $@-$(DEFAULT_REPL) julia

sys0.ji: src/boot.j src/dump.c j/stage0.j
	$(QUIET_JULIA) ./julia -b stage0.j
	@rm -f sys.ji

# if sys.ji exists, use it to rebuild, otherwise use sys0.ji
sys.ji: VERSION sys0.ji j/stage1.j j/sysimg.j j/start_image.j j/*.j
	$(QUIET_JULIA) ./julia `test -f sys.ji && echo stage1.j || echo -J sys0.ji stage1.j`

install: release
	install -d $(DESTDIR)/usr/share/julia/lib
	install -d $(DESTDIR)/usr/share/julia/j
	install -d $(DESTDIR)/usr/share/julia/contrib
	install -d $(DESTDIR)/usr/share/julia/examples
	install -v -C julia* $(DESTDIR)/usr/share/julia
	install -v -C sys.ji $(DESTDIR)/usr/share/julia
	install -v -C j/* $(DESTDIR)/usr/share/julia/j
	install -v -C examples/*.j $(DESTDIR)/usr/share/julia/examples
	install -v -C lib/libarpack.$(SHLIB_EXT) lib/libfdm.$(SHLIB_EXT) lib/libfftw3.$(SHLIB_EXT)* lib/libfftw3f.$(SHLIB_EXT)* lib/libpcre.$(SHLIB_EXT)* lib/libpcrecpp.$(SHLIB_EXT)* lib/libpcreposix.$(SHLIB_EXT)* lib/librandom.$(SHLIB_EXT) lib/liblapack.$(SHLIB_EXT) lib/libsuitesparse*$(SHLIB_EXT) lib/libgrisu.$(SHLIB_EXT) lib/libamos.$(SHLIB_EXT) $(DESTDIR)/usr/share/julia/lib

dist: release
	rm -fr dist julia-*.tar.gz
	$(MAKE) install DESTDIR=dist
	cd dist/usr/share && tar zcvf ../../../julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz *

deb:
	fakeroot debian/rules binary

debclean:
	fakeroot debian/rules clean

h2j: lib/libLLVM*.a lib/libclang*.a src/h2j.cpp
	$(QUIET_CC) g++ -O2 -fno-rtti -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Iinclude $^ -o $@

clean:
	@rm -f julia
	@rm -f *~ *#
	@rm -f sys0.ji
	@rm -f sys.ji
	@$(MAKE) -sC j clean
	@$(MAKE) -sC src clean
	@$(MAKE) -sC ui clean
	@$(MAKE) -sC ui/webserver clean
	@$(MAKE) -sC test/unicode clean

cleanall: clean
	@$(MAKE) -sC src clean-flisp clean-support

distclean: cleanall

#distclean: cleanall
#	$(MAKE) -C external cleanall

.PHONY: default debug release julia-debug julia-release \
	test testall test-* sloccount clean cleanall

test: release
	@$(MAKE) -sC test default

testall: release
	@$(MAKE) -sC test all

test-%: release
	@$(MAKE) -sC test $*
