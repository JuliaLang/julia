JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

default: release

debug release: %: julia-% sys.ji

julia-debug julia-release:
	@$(MAKE) -sC external
	@$(MAKE) -sC src lib$@
	@$(MAKE) -sC ui $@
	@$(MAKE) -sC jl
	@ln -f $@-$(DEFAULT_REPL) julia

sys0.ji: src/boot.jl src/dump.c jl/stage0.jl
	$(QUIET_JULIA) ./julia -b stage0.jl
	@rm -f sys.ji

# if sys.ji exists, use it to rebuild, otherwise use sys0.ji
sys.ji: VERSION sys0.ji jl/stage1.jl jl/sysimg.jl jl/start_image.jl jl/*.jl
	$(QUIET_JULIA) ./julia `test -f sys.ji && echo stage1.jl || echo -J sys0.ji stage1.jl`

install: release
	install -d $(DESTDIR)$(PREFIX)/share/julia/lib
	install -d $(DESTDIR)$(PREFIX)/share/julia/jl
	install -d $(DESTDIR)$(PREFIX)/share/julia/contrib
	install -d $(DESTDIR)$(PREFIX)/share/julia/examples
	install -v julia* $(DESTDIR)$(PREFIX)/share/julia
	install -v sys.ji $(DESTDIR)$(PREFIX)/share/julia
	install -v jl/* $(DESTDIR)$(PREFIX)/share/julia/jl
	install -v examples/*.jl $(DESTDIR)$(PREFIX)/share/julia/examples
	install -v lib/libarpack.$(SHLIB_EXT) lib/libfdm.$(SHLIB_EXT) lib/libfftw3.$(SHLIB_EXT)* lib/libfftw3f.$(SHLIB_EXT)* lib/libpcre.$(SHLIB_EXT)* lib/libpcrecpp.$(SHLIB_EXT)* lib/libpcreposix.$(SHLIB_EXT)* lib/librandom.$(SHLIB_EXT) lib/liblapack.$(SHLIB_EXT) lib/libsuitesparse*$(SHLIB_EXT) lib/libgrisu.$(SHLIB_EXT) lib/libamos.$(SHLIB_EXT) $(DESTDIR)$(PREFIX)/share/julia/lib

dist: release
	rm -fr dist julia-*.tar.gz
	$(MAKE) install DESTDIR=dist PREFIX=/usr
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
	@$(MAKE) -sC jl clean
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
