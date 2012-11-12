JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

all: default
default: release

DIRS = $(BUILD)/bin $(BUILD)/etc $(BUILD)/lib/julia $(BUILD)/share/julia

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,extras base ui test,$(eval $(call symlink_target,$(link),$(BUILD)/lib/julia)))
$(foreach link,doc examples,$(eval $(call symlink_target,$(link),$(BUILD)/share/julia)))

MAKEs = $(MAKE)
ifeq ($(USE_QUIET), 1)
MAKEs += -s
endif

debug release: | $(DIRS) $(BUILD)/lib/julia/extras $(BUILD)/lib/julia/base $(BUILD)/lib/julia/ui $(BUILD)/lib/julia/test $(BUILD)/share/julia/doc $(BUILD)/share/julia/examples
	@$(MAKEs) julia-$@
	@$(MAKEs) JULIA_EXECUTABLE=$(JULIA_EXECUTABLE_$@) $(BUILD)/lib/julia/sys.ji

julia-debug julia-release:
	@$(MAKEs) -C deps
	@$(MAKEs) -C src lib$@
	@$(MAKEs) -C base
	@$(MAKEs) -C extras
	@$(MAKEs) -C ui $@
	@ln -sf $(BUILD)/bin/$@-$(DEFAULT_REPL) julia

$(BUILD)/lib/julia/helpdb.jl: doc/helpdb.jl | $(BUILD)/lib/julia
	@cp $< $@

# use sys.ji if it exists, otherwise run two stages
$(BUILD)/lib/julia/sys.ji: VERSION base/*.jl $(BUILD)/lib/julia/helpdb.jl
	$(QUIET_JULIA) cd base && \
	(test -f $(BUILD)/lib/julia/sys.ji || $(JULIA_EXECUTABLE) -bf sysimg.jl) && $(JULIA_EXECUTABLE) -f sysimg.jl || echo "Note: this error is usually fixed by running 'make clean'."

ifeq ($(OS), WINNT)
OPENBLASNAME=openblas-r0.1.1
else
OPENBLASNAME=openblas
endif
PREFIX ?= julia-$(JULIA_COMMIT)
install: release
	@$(MAKEs) -C test/unicode
	@for subdir in "sbin" "bin" "etc" "lib/julia" "share/julia" ; do \
		mkdir -p $(PREFIX)/$$subdir ; \
	done
	cp $(BUILD)/bin/*julia* $(PREFIX)/bin
	cd $(PREFIX)/bin && ln -s julia-release-$(DEFAULT_REPL) julia
	cp -R -L $(BUILD)/lib/julia/* $(PREFIX)/lib/julia
	-for suffix in "Rmath" "amd" "arpack" "cholmod" "colamd" "openlibm" "fftw3" "fftw3f" "fftw3_threads" "fftw3f_threads" "glpk" "glpk_wrapper" "gmp" "gmp_wrapper" "grisu" "history" "julia-release" "$(OPENBLASNAME)" "openlibm" "pcre" "random" "readline" "suitesparse_wrapper" "tk_wrapper" "spqr" "umfpack" "z" ; do \
		cp $(BUILD)/lib/lib$${suffix}.$(SHLIB_EXT) $(PREFIX)/lib ; \
	done
# Web-REPL stuff
	-cp $(BUILD)/lib/mod* $(PREFIX)/lib
	-cp $(BUILD)/sbin/* $(PREFIX)/sbin
	-cp $(BUILD)/etc/* $(PREFIX)/etc
	-cp -R -L $(BUILD)/share/* $(PREFIX)/share
ifeq ($(OS), WINNT)
	-cp dist/windows/* $(PREFIX)
ifeq ($(shell uname),MINGW32_NT-6.1)
	-for dllname in "libgfortran-3" "libquadmath-0" "libgcc_s_dw2-1" "libstdc++-6,pthreadgc2" ; do \
		cp /mingw/bin/$${dllname}.dll $(PREFIX)/lib ; \
	done
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
	@$(MAKE) -C base clean
	@$(MAKE) -C extras clean
	@$(MAKE) -C src clean
	@$(MAKE) -C ui clean
	@$(MAKE) -C ui/webserver clean
	@$(MAKE) -C test/unicode clean
	@for buildtype in "release" "debug" ; do \
		for repltype in "basic" "readline" "webserver" ; do \
			rm -f julia-$${buildtype}-$${repltype}; \
		done \
	done
	@rm -f *~ *# *.tar.gz
	@rm -fr $(BUILD)/lib/julia

cleanall: clean
	@$(MAKE) -C src clean-flisp clean-support
#	@$(MAKE) -C deps clean-uv

.PHONY: default debug release julia-debug julia-release \
	test testall test-* clean cleanall

test: release
	@$(MAKEs) -C test default

testall: release
	@$(MAKEs) -C test all

test-%: release
	@$(MAKEs) -C test $*
