JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

all: default
default: release

DIRS = $(BUILD)/bin $(BUILD)/etc $(BUILD)/$(JL_LIBDIR) $(BUILD)/$(JL_PRIVATE_LIBDIR) $(BUILD)/share/julia

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,extras base ui test doc examples,$(eval $(call symlink_target,$(link),$(BUILD)/share/julia)))

MAKEs = $(MAKE)
ifeq ($(USE_QUIET), 1)
MAKEs += -s
endif

debug release: | $(DIRS) $(BUILD)/share/julia/extras $(BUILD)/share/julia/base $(BUILD)/share/julia/ui $(BUILD)/share/julia/test $(BUILD)/share/julia/doc $(BUILD)/share/julia/examples
	@$(MAKEs) julia-$@
	@$(MAKEs) JULIA_EXECUTABLE=$(JULIA_EXECUTABLE_$@) $(BUILD)/share/julia/sys.ji

julia-debug julia-release:
	@$(MAKEs) -C deps
	@$(MAKEs) -C src lib$@
	@$(MAKEs) -C base
	@$(MAKEs) -C extras
	@$(MAKEs) -C ui $@
	@ln -sf $(BUILD)/bin/$@-$(DEFAULT_REPL) julia

$(BUILD)/share/julia/helpdb.jl: doc/helpdb.jl | $(BUILD)/share/julia
	@cp $< $@

# use sys.ji if it exists, otherwise run two stages
$(BUILD)/share/julia/sys.ji: VERSION base/*.jl $(BUILD)/share/julia/helpdb.jl
	$(QUIET_JULIA) cd base && \
	(test -f $(BUILD)/share/julia/sys.ji || $(JULIA_EXECUTABLE) -bf sysimg.jl) && $(JULIA_EXECUTABLE) -f sysimg.jl || echo "Note: this error is usually fixed by running 'make clean'."

ifeq ($(OS), WINNT)
OPENBLASNAME=openblas-r0.1.1
else
OPENBLASNAME=openblas
endif
PREFIX ?= julia-$(JULIA_COMMIT)
install: release
	@$(MAKEs) -C test/unicode
	@for subdir in "sbin" "bin" "etc" $(JL_LIBDIR) $(JL_PRIVATE_LIBDIR) "share/julia" ; do \
		mkdir -p $(PREFIX)/$$subdir ; \
	done
	cp $(BUILD)/bin/*julia* $(PREFIX)/bin
	cd $(PREFIX)/bin && ln -s julia-release-$(DEFAULT_REPL) julia
	-for suffix in $(JL_LIBS) ; do \
		cp $(BUILD)/$(JL_LIBDIR)/lib$${suffix}.$(SHLIB_EXT) $(PREFIX)/$(JL_LIBDIR) ; \
	done
	-for suffix in $(JL_PRIVATE_LIBS) ; do \
		cp $(BUILD)/$(JL_PRIVATE_LIBDIR)/lib$${suffix}.$(SHLIB_EXT) $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
	# Web-REPL stuff
	-cp -R -L $(BUILD)/$(JL_LIBDIR)/lighttpd $(PREFIX)/$(JL_LIBDIR)
	-cp $(BUILD)/sbin/* $(PREFIX)/sbin
	-cp $(BUILD)/etc/* $(PREFIX)/etc
	# Copy in all .jl sources as well
	-cp -R -L $(BUILD)/share/julia $(PREFIX)/share/
ifeq ($(OS), WINNT)
	-cp dist/windows/* $(PREFIX)
ifeq ($(shell uname),MINGW32_NT-6.1)
	-for dllname in "libgfortran-3" "libquadmath-0" "libgcc_s_dw2-1" "libstdc++-6,pthreadgc2" ; do \
		cp /mingw/bin/$${dllname}.dll $(PREFIX)/$(JL_LIBDIR) ; \
	done
endif
endif

dist: cleanall
	rm -fr julia-*.tar.gz julia-$(JULIA_COMMIT)
	-$(MAKE) -C deps clean-openblas
	$(MAKE) install OPENBLAS_DYNAMIC_ARCH=1
ifeq ($(OS), Darwin)
	-./contrib/fixup-libgfortran.sh $(PREFIX)/$(JL_LIBDIR) /usr/local/lib
endif
	tar zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia-$(JULIA_COMMIT)
	rm -fr julia-$(JULIA_COMMIT)

deb:
	fakeroot debian/rules binary

debclean:
	fakeroot debian/rules clean

h2j: $(BUILD)/$(JL_LIBDIR)/libLLVM*.a $(BUILD)/$(JL_LIBDIR)/libclang*.a src/h2j.cpp
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
	@rm -fr $(BUILD)/$(JL_LIBDIR)
	@rm -fr $(BUILD)/$(JL_PRIVATE_LIBDIR)

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
