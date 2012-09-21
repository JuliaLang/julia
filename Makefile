JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

all: default
default: release

DIRS = $(BUILD)/bin $(BUILD)/etc $(BUILD)/lib/julia

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,extras base ui,$(eval $(call symlink_target,$(link),$(BUILD)/lib/julia)))

debug release: | $(DIRS) $(BUILD)/lib/julia/extras $(BUILD)/lib/julia/base $(BUILD)/lib/julia/ui
	@$(MAKE) -s julia-$@
	@$(MAKE) JULIA_EXECUTABLE=$(JULIA_EXECUTABLE_$@) -s $(BUILD)/lib/julia/sys.ji

julia-debug julia-release:
	@$(MAKE) -sC deps
	@$(MAKE) -sC src lib$@
	@$(MAKE) -sC base
	@$(MAKE) -sC extras
	@$(MAKE) -sC ui $@
	@ln -sf $(BUILD)/bin/$@-$(DEFAULT_REPL) julia

$(BUILD)/lib/julia/helpdb.jl: doc/helpdb.jl | $(BUILD)/lib/julia
	@cp $< $@

# use sys.ji if it exists, otherwise run two stages
$(BUILD)/lib/julia/sys.ji: VERSION base/*.jl $(BUILD)/lib/julia/helpdb.jl
	$(QUIET_JULIA) cd base && \
	(test -f $(BUILD)/lib/julia/sys.ji || $(JULIA_EXECUTABLE) -b sysimg.jl) && $(JULIA_EXECUTABLE) sysimg.jl || echo "Note: this error is usually fixed by running 'make clean'."

ifeq ($(OS), WINNT)
OPENBLASNAME=openblas-r0.1.1
else
OPENBLASNAME=openblas
endif
PREFIX ?= julia-$(JULIA_COMMIT)
install: release
	mkdir -p $(PREFIX)/{sbin,bin,etc,lib/julia,share/julia}
	cp $(BUILD)/bin/*julia* $(PREFIX)/bin
	cd $(PREFIX)/bin && ln -s julia-release-$(DEFAULT_REPL) julia
	cp -R -L $(BUILD)/lib/julia/* $(PREFIX)/lib/julia
	-cp $(BUILD)/lib/lib{Rmath,amd,amos,arpack,cholmod,colamd,fdm,fftw3,fftw3f,fftw3_threads,fftw3f_threads,glpk,glpk_wrapper,gmp,gmp_wrapper,grisu,history,julia-release,$(OPENBLASNAME),openlibm,pcre,random,readline,suitesparse_wrapper,umfpack,z}.$(SHLIB_EXT) $(PREFIX)/lib
# Web-REPL stuff
	-cp $(BUILD)/lib/mod* $(PREFIX)/lib
	-cp $(BUILD)/sbin/* $(PREFIX)/sbin
	-cp $(BUILD)/etc/* $(PREFIX)/etc
ifeq ($(OS), WINNT)
	-cp dist/windows/* $(PREFIX)
ifeq ($(shell uname),MINGW32_NT-6.1)
	-cp /mingw/bin/{libgfortran-3,libquadmath-0,libgcc_s_dw2-1,libstdc++-6,pthreadgc2}.dll $(PREFIX)/lib
endif
endif

dist: cleanall
	rm -fr julia-*.tar.gz julia-$(JULIA_COMMIT)
	-$(MAKE) -C deps clean-openblas
	$(MAKE) install OPENBLAS_DYNAMIC_ARCH=1
ifeq ($(OS), Darwin)
	-./contrib/fixup-libgfortran.sh $(PREFIX)/lib /usr/local/lib
endif
	tar zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia-$(JULIA_COMMIT)
	rm -fr julia-$(JULIA_COMMIT)

deb:
	fakeroot debian/rules binary

debclean:
	fakeroot debian/rules clean

h2j: $(BUILD)/lib/libLLVM*.a $(BUILD)/lib/libclang*.a src/h2j.cpp
	$(QUIET_CC) g++ -O2 -fno-rtti -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -Iinclude $^ -o $@

clean: | $(CLEAN_TARGETS)
	@$(MAKE) -sC base clean
	@$(MAKE) -sC extras clean
	@$(MAKE) -sC src clean
	@$(MAKE) -sC ui clean
	@$(MAKE) -sC ui/webserver clean
	@$(MAKE) -sC test/unicode clean
	@rm -f julia-{release,debug}-{basic,readline,webserver}
	@rm -f *~ *# *.tar.gz
	@rm -fr $(BUILD)/lib/julia

cleanall: clean
	@$(MAKE) -sC src clean-flisp clean-support
#	@$(MAKE) -sC deps clean-uv

.PHONY: default debug release julia-debug julia-release \
	test testall test-* clean cleanall

test: release
	@$(MAKE) -sC test default

testall: release
	@$(MAKE) -sC test all

test-%: release
	@$(MAKE) -sC test $*
