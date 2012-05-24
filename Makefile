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

base/build_h.jl: Make.inc
	@echo "_jl_libblas_name = \"$(LIBBLASNAME)\"" > $@
	@echo "_jl_liblapack_name = \"$(LIBLAPACKNAME)\"" >> $@

sys0.ji: src/boot.jl src/dump.c base/stage0.jl base/build_h.jl
	$(QUIET_JULIA) cd base && ../julia -b stage0.jl
	@rm -f sys.ji

# if sys.ji exists, use it to rebuild, otherwise use sys0.ji
sys.ji: VERSION sys0.ji base/*.jl
	$(QUIET_JULIA) cd base && ../julia `test -f ../sys.ji && echo stage1.jl || echo -J sys0.ji stage1.jl`

install: release
	install -d $(DESTDIR)$(PREFIX)/julia/usr/lib
	install -d $(DESTDIR)$(PREFIX)/julia/usr/sbin
	install -d $(DESTDIR)$(PREFIX)/julia/usr/etc
	install -d $(DESTDIR)$(PREFIX)/julia/base
	install -d $(DESTDIR)$(PREFIX)/julia/contrib
	install -d $(DESTDIR)$(PREFIX)/julia/examples
	install -d $(DESTDIR)$(PREFIX)/julia/extras
	install -d $(DESTDIR)$(PREFIX)/julia/ui/webserver
	install -d $(DESTDIR)$(PREFIX)/julia/ui/website/assets
	install -d $(DESTDIR)$(PREFIX)/julia/ui/website/images
	install -v julia-release-basic $(DESTDIR)$(PREFIX)/julia
	install -v julia-release-webserver $(DESTDIR)$(PREFIX)/julia
	install -v julia-release-readline $(DESTDIR)$(PREFIX)/julia
	install -v julia $(DESTDIR)$(PREFIX)/julia
	install -v sys.ji $(DESTDIR)$(PREFIX)/julia
	install -v base/* $(DESTDIR)$(PREFIX)/julia/base
	install -v extras/* $(DESTDIR)$(PREFIX)/julia/extras
	install -v examples/*.jl $(DESTDIR)$(PREFIX)/julia/examples
	install -v $(USRLIB)/*.$(SHLIB_EXT) $(DESTDIR)$(PREFIX)/julia/usr/lib
	test -z "$(ls -A $(USR)/sbin/*)" || install -v $(USR)/sbin/* $(DESTDIR)$(PREFIX)/julia/usr/sbin
	install -v launch-julia-webserver $(DESTDIR)$(PREFIX)/julia
	install -v ui/webserver/*.jl $(DESTDIR)$(PREFIX)/julia/ui/webserver
	install -v ui/website/*.* $(DESTDIR)$(PREFIX)/julia/ui/website
	install -v ui/website/assets/* $(DESTDIR)$(PREFIX)/julia/ui/website/assets
	install -v ui/website/images/* $(DESTDIR)$(PREFIX)/julia/ui/website/images
	install -v $(USR)/etc/lighttpd.conf $(DESTDIR)$(PREFIX)/julia/usr/etc

dist: release
	rm -fr dist julia-*.tar.gz
	$(MAKE) install DESTDIR=dist
	cd dist && tar zcvf ../julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia
	rm -fr dist

deb:
	fakeroot debian/rules binary

debclean:
	fakeroot debian/rules clean

h2j: usr/lib/libLLVM*.a usr/lib/libclang*.a src/h2j.cpp
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
#	@$(MAKE) -sC deps clean-uv

.PHONY: default debug release julia-debug julia-release \
	test testall test-* sloccount clean cleanall

test: release
	@$(MAKE) -sC test default

testall: release
	@$(MAKE) -sC test all

test-%: release
	@$(MAKE) -sC test $*
