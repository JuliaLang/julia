JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

all: default
default: release

debug release:
	@$(MAKE) -s julia-$@
	@$(MAKE) -s sys.ji

julia-debug julia-release:
	@$(MAKE) -sC deps
	@$(MAKE) -sC src lib$@
	@$(MAKE) -sC base
	@$(MAKE) -sC ui $@
	@ln -f $@-$(DEFAULT_REPL) julia

sys0.ji: src/boot.jl src/dump.c base/stage0.jl
	$(QUIET_JULIA) cd base && ../julia -b stage0.jl
	@rm -f sys.ji

# if sys.ji exists, use it to rebuild, otherwise use sys0.ji
sys.ji: VERSION sys0.ji base/*.jl
	$(QUIET_JULIA) cd base && ../julia `test -f ../sys.ji && echo stage1.jl || echo -J sys0.ji stage1.jl`

install: release
	install -d $(DESTDIR)$(PREFIX)/share/julia/usr/lib
	install -d $(DESTDIR)$(PREFIX)/share/julia/base
	install -d $(DESTDIR)$(PREFIX)/share/julia/contrib
	install -d $(DESTDIR)$(PREFIX)/share/julia/examples
	install -d $(DESTDIR)$(PREFIX)/share/julia/extras
	install -v julia $(DESTDIR)$(PREFIX)/share/julia
	install -v julia-release-basic $(DESTDIR)$(PREFIX)/share/julia
	install -v julia-release-webserver $(DESTDIR)$(PREFIX)/share/julia
	install -v sys.ji $(DESTDIR)$(PREFIX)/share/julia
	install -v base/* $(DESTDIR)$(PREFIX)/share/julia/base
	install -v extras/* $(DESTDIR)$(PREFIX)/share/julia/extras
	install -v examples/*.jl $(DESTDIR)$(PREFIX)/share/julia/examples
	install -v $(USRLIB)/*.$(SHLIB_EXT) $(DESTDIR)$(PREFIX)/share/julia/usr/lib

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
	@$(MAKE) -sC base clean
	@$(MAKE) -sC src clean
	@$(MAKE) -sC ui clean
	@$(MAKE) -sC ui/webserver clean
	@$(MAKE) -sC test/unicode clean

cleanall: clean
	@$(MAKE) -sC src clean-flisp clean-support

distclean: cleanall
	rm -fr dist

.PHONY: default debug release julia-debug julia-release \
	test testall test-* sloccount clean cleanall

test: release
	@$(MAKE) -sC test default

testall: release
	@$(MAKE) -sC test all

test-%: release
	@$(MAKE) -sC test $*
