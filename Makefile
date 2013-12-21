JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

# TODO: Code bundled with Julia should be installed into a versioned directory,
# PREFIX/share/julia/VERSDIR, so that in the future one can have multiple
# major versions of Julia installed concurrently. Third-party code that
# is not controlled by Pkg should be installed into
# PREFIX/share/julia/site/VERSDIR (not PREFIX/share/julia/VERSDIR/site ...
# so that PREFIX/share/julia/VERSDIR can be overwritten without touching
# third-party code).
VERSDIR = v`cut -d. -f1-2 < VERSION`

all: default
default: release

DIRS = $(BUILD)/bin $(BUILD)/etc/julia $(BUILD)/lib $(BUILD)/libexec $(BUILD)/share/julia $(BUILD)/share/julia/man/man1
ifneq ($(JL_LIBDIR),bin)
ifneq ($(JL_LIBDIR),lib)
DIRS += $(BUILD)/$(JL_LIBDIR)
endif
endif
ifneq ($(JL_PRIVATE_LIBDIR),bin)
ifneq ($(JL_PRIVATE_LIBDIR),lib)
ifneq ($(JL_PRIVATE_LIBDIR),$(JL_LIBDIR))
DIRS += $(BUILD)/$(JL_PRIVATE_LIBDIR)
endif
endif
endif

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,base test doc examples,$(eval $(call symlink_target,$(link),$(BUILD)/share/julia)))

debug release: | $(DIRS) $(BUILD)/share/julia/base $(BUILD)/share/julia/test $(BUILD)/share/julia/doc $(BUILD)/share/julia/examples $(BUILD)/etc/julia/juliarc.jl
	@$(MAKE) $(QUIET_MAKE) julia-$@
	@export JL_PRIVATE_LIBDIR=$(JL_PRIVATE_LIBDIR) && \
	$(MAKE) $(QUIET_MAKE) LD_LIBRARY_PATH=$(BUILD)/lib:$(LD_LIBRARY_PATH) JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$@)" $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.$(SHLIB_EXT)

julia-debug-symlink:
	@ln -sf $(BUILD)/bin/julia-debug-$(DEFAULT_REPL) julia

julia-release-symlink:
	@ln -sf $(BUILD)/bin/julia-$(DEFAULT_REPL) julia

julia-debug julia-release:
	@-git submodule init --quiet
	@-git submodule update
	@$(MAKE) $(QUIET_MAKE) -C deps
	@$(MAKE) $(QUIET_MAKE) -C src lib$@
	@$(MAKE) $(QUIET_MAKE) -C base
	@$(MAKE) $(QUIET_MAKE) -C ui $@
ifneq ($(OS),WINNT)
ifndef JULIA_VAGRANT_BUILD
	@$(MAKE) $(QUIET_MAKE) $@-symlink
endif
endif

$(BUILD)/share/julia/helpdb.jl: doc/helpdb.jl | $(BUILD)/share/julia
	@cp $< $@

$(BUILD)/share/man/man1/julia.1: doc/man/julia.1 | $(BUILD)/share/julia
	@mkdir -p $(BUILD)/share/man/man1
	@cp $< $@

$(BUILD)/etc/julia/juliarc.jl: etc/juliarc.jl | $(BUILD)/etc/julia
	@cp $< $@

# use sys.ji if it exists, otherwise run two stages
$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys%ji: $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys%bc

$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys%o: $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys%bc
	$(call spawn,$(LLVM_LLC)) -filetype=obj -relocation-model=pic -mattr=-bmi2,-avx2 -o $@ $<

$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys%$(SHLIB_EXT): $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys%o
	$(CXX) -shared -fPIC -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -o $@ $< \
		$$([ $(OS) = Darwin ] && echo -Wl,-undefined,dynamic_lookup || echo -Wl,--unresolved-symbols,ignore-all ) \
		$$([ $(OS) = WINNT ] && echo -ljulia -lssp)

$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys0.bc:
	@$(QUIET_JULIA) cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) --build $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys0 sysimg.jl

$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.bc: VERSION base/*.jl base/pkg/*.jl base/linalg/*.jl base/sparse/*.jl $(BUILD)/share/julia/helpdb.jl $(BUILD)/share/man/man1/julia.1 $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys0.$(SHLIB_EXT)
	@$(QUIET_JULIA) cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) --build $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys \
		-J$(BUILD)/$(JL_PRIVATE_LIBDIR)/$$([ -e $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji ] && echo sys.ji || echo sys0.ji) -f sysimg.jl \
		|| (echo "*** This error is usually fixed by running 'make clean'. If the error persists, try 'make cleanall'. ***" && false)

run-julia-debug run-julia-release: run-julia-%:
	$(MAKE) $(QUIET_MAKE) run-julia JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$*)"
run-julia:
	@$(call spawn,$(JULIA_EXECUTABLE))
run:
	@$(call spawn,$(cmd))

# public libraries, that are installed in $(PREFIX)/lib
JL_LIBS = julia julia-debug

# private libraries, that are installed in $(PREFIX)/lib/julia
JL_PRIVATE_LIBS = random suitesparse_wrapper grisu
ifeq ($(USE_SYSTEM_FFTW),0)
JL_PRIVATE_LIBS += fftw3 fftw3f fftw3_threads fftw3f_threads
endif
ifeq ($(USE_SYSTEM_PCRE),0)
JL_PRIVATE_LIBS += pcre
endif
ifeq ($(USE_SYSTEM_OPENLIBM),0)
JL_PRIVATE_LIBS += openlibm-extras
ifeq ($(USE_SYSTEM_LIBM),0)
JL_PRIVATE_LIBS += openlibm
endif
endif
ifeq ($(USE_SYSTEM_BLAS),0)
JL_PRIVATE_LIBS += openblas
else ifeq ($(USE_SYSTEM_LAPACK),0)
JL_PRIVATE_LIBS += lapack
endif
ifeq ($(USE_SYSTEM_GMP),0)
JL_PRIVATE_LIBS += gmp
endif
ifeq ($(USE_SYSTEM_MPFR),0)
JL_PRIVATE_LIBS += mpfr
endif
ifeq ($(USE_SYSTEM_ARPACK),0)
JL_PRIVATE_LIBS += arpack
endif
ifeq ($(USE_SYSTEM_SUITESPARSE),0)
JL_PRIVATE_LIBS += amd camd ccolamd cholmod colamd umfpack spqr
endif
#ifeq ($(USE_SYSTEM_ZLIB),0)
#JL_PRIVATE_LIBS += z
#endif
ifeq ($(USE_SYSTEM_RMATH),0)
JL_PRIVATE_LIBS += Rmath
endif
ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
JL_PRIVATE_LIBS += gfortblas
endif
endif
endif

ifeq ($(OS),WINNT)
define std_dll
debug release: | $$(BUILD)/$$(JL_LIBDIR)/lib$(1).dll
$$(BUILD)/$$(JL_LIBDIR)/lib$(1).dll: | $$(BUILD)/$$(JL_LIBDIR)
ifeq ($$(BUILD_OS),$$(OS))
	cp $$(call pathsearch,lib$(1).dll,$$(PATH)) $$(BUILD)/$$(JL_LIBDIR) ;
else
	cp $$(call wine_pathsearch,lib$(1).dll,$$(STD_LIB_PATH)) $$(BUILD)/$$(JL_LIBDIR) ;
endif
JL_LIBS += $(1)
endef
$(eval $(call std_dll,gfortran-3))
$(eval $(call std_dll,quadmath-0))
$(eval $(call std_dll,stdc++-6))
ifeq ($(ARCH),i686)
$(eval $(call std_dll,gcc_s_sjlj-1))
else
$(eval $(call std_dll,gcc_s_seh-1))
endif
ifneq ($(BUILD_OS),WINNT)
$(eval $(call std_dll,ssp-0))
endif
endif

PREFIX ?= julia-$(JULIA_COMMIT)
install:
	@$(MAKE) $(QUIET_MAKE) release
	@$(MAKE) $(QUIET_MAKE) debug
	@for subdir in "bin" "libexec" $(JL_LIBDIR) $(JL_PRIVATE_LIBDIR) "share/julia" "share/man/man1" "include/julia" "share/julia/site/"$(VERSDIR) "etc/julia" ; do \
		mkdir -p $(PREFIX)/$$subdir ; \
	done
	cp -a $(BUILD)/bin/julia* $(PREFIX)/bin/
	#-cp -a $(BUILD)/bin/llc$(EXE) $(PREFIX)/libexec # this needs libLLVM-3.3.$(SHLIB_EXT)
ifneq ($(OS),WINNT)
	cp -a $(BUILD)/libexec $(PREFIX)
	cd $(PREFIX)/bin && ln -sf julia-$(DEFAULT_REPL) julia
else
	cp -a $(BUILD)/bin/*.dll $(BUILD)/bin/*.bat $(PREFIX)/bin/
endif
	for suffix in $(JL_LIBS) ; do \
		cp -a $(BUILD)/$(JL_LIBDIR)/lib$${suffix}*.$(SHLIB_EXT)* $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
	for suffix in $(JL_PRIVATE_LIBS) ; do \
		cp -a $(BUILD)/$(JL_LIBDIR)/lib$${suffix}*.$(SHLIB_EXT)* $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
ifeq ($(USE_SYSTEM_LIBUV),0)
ifeq ($(OS),WINNT)
	cp -a $(BUILD)/lib/libuv.a $(PREFIX)/$(JL_PRIVATE_LIBDIR)
	cp -a $(BUILD)/include/tree.h $(PREFIX)/include/julia
else
	cp -a $(BUILD)/$(JL_LIBDIR)/libuv.a $(PREFIX)/$(JL_PRIVATE_LIBDIR)
endif
	cp -a $(BUILD)/include/uv* $(PREFIX)/include/julia
endif
	cp -a src/julia.h src/support/*.h $(PREFIX)/include/julia
	# Copy system image
	cp $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji $(PREFIX)/$(JL_PRIVATE_LIBDIR)
	cp $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.$(SHLIB_EXT) $(PREFIX)/$(JL_PRIVATE_LIBDIR)
	# Copy in all .jl sources as well
	cp -R -L $(BUILD)/share/julia $(PREFIX)/share/
ifeq ($(OS), WINNT)
	cp $(JULIAHOME)/contrib/windows/*.bat $(PREFIX)
endif
	# Copy in beautiful new man page!
	cp $(BUILD)/share/man/man1/julia.1 $(PREFIX)/share/man/man1/
	# Copy etc/julia directory to SYSCONFIGDIR if it is set, otherwise to just $(PREFIX)/etc/
ifneq ($(SYSCONFDIR),)
	mkdir -p $(SYSCONFDIR)
	cp -R $(BUILD)/etc/julia $(SYSCONFDIR)/
else
	cp -R $(BUILD)/etc/julia $(PREFIX)/etc/
endif


dist:
	rm -fr julia-*.tar.gz julia-*.exe julia-$(JULIA_COMMIT)
ifeq ($(USE_SYSTEM_BLAS),0)
ifneq ($(OPENBLAS_DYNAMIC_ARCH),1)
	@echo OpenBLAS must be rebuilt with OPENBLAS_DYNAMIC_ARCH=1 to use dist target
	@false
endif
endif
ifneq ($(PREFIX),julia-$(JULIA_COMMIT))
	$(error PREFIX must not be set for make dist)
endif
	@$(MAKE) install
	cp LICENSE.md julia-$(JULIA_COMMIT)
ifeq ($(OS), Darwin)
	-./contrib/mac/fixup-libgfortran.sh $(PREFIX)/$(JL_PRIVATE_LIBDIR)
endif
	# Copy in juliarc.jl files per-platform for binary distributions as well
	# Note that we don't install to SYSCONFDIR: we always install to PREFIX/etc.
	# If you want to make a distribution with a hardcoded path, you take care of installation
ifeq ($(OS), Darwin)
	-cat ./contrib/mac/juliarc.jl >> $(PREFIX)/etc/julia/juliarc.jl
else ifeq ($(OS), WINNT)
	-cat ./contrib/windows/juliarc.jl >> $(PREFIX)/etc/julia/juliarc.jl
endif

ifeq ($(OS), WINNT)
	[ ! -d dist-extras ] || ( cd dist-extras && \
   		cp 7z.exe 7z.dll libexpat-1.dll zlib1.dll ../$(PREFIX)/bin && \
	    mkdir ../$(PREFIX)/Git && \
	    7z x PortableGit.7z -o"../$(PREFIX)/Git" )
	./dist-extras/7z a -mx9 -sfx7z.sfx julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).exe julia-$(JULIA_COMMIT)
else
	tar zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr julia-$(JULIA_COMMIT)

clean: | $(CLEAN_TARGETS)
	@$(MAKE) -C base clean
	@$(MAKE) -C src clean
	@$(MAKE) -C ui clean
	for repltype in "basic" "readline"; do \
		rm -f usr/bin/julia-debug-$${repltype}; \
		rm -f usr/bin/julia-$${repltype}; \
	done
	@rm -f julia
	@rm -f *~ *# *.tar.gz
	@rm -fr $(BUILD)/$(JL_PRIVATE_LIBDIR)
# Temporarily add this line to the Makefile to remove extras
	@rm -fr $(BUILD)/share/julia/extras

cleanall: clean
	@$(MAKE) -C src clean-flisp clean-support
	@rm -fr $(BUILD)/$(JL_LIBDIR)
ifeq ($(OS),WINNT)
	@rm -rf $(BUILD)/lib
endif
	@$(MAKE) -C deps clean-uv

distclean: cleanall
	@$(MAKE) -C deps distclean
	@$(MAKE) -C doc cleanall
	rm -fr usr

.PHONY: default debug release julia-debug julia-release \
	test testall testall1 test-* clean distclean cleanall \
	run-julia run-julia-debug run-julia-release run \
	install dist

ifeq ($(VERBOSE),1)
.SILENT:
endif

test: release
	@$(MAKE) $(QUIET_MAKE) -C test default

testall: release
	@$(MAKE) $(QUIET_MAKE) -C test all

testall1: release
	@env JULIA_CPU_CORES=1 $(MAKE) $(QUIET_MAKE) -C test all

test-%: release
	@$(MAKE) $(QUIET_MAKE) -C test $*

perf: release
	@$(MAKE) $(QUIET_MAKE) -C test/perf

perf-%: release
	@$(MAKE) $(QUIET_MAKE) -C test/perf $*

# download target for some hardcoded windows dependencies
.PHONY: win-extras wine_path
win-extras:
	[ -d dist-extras ] || mkdir dist-extras
ifneq ($(BUILD_OS),WINNT)
	cp /usr/lib/p7zip/7z /usr/lib/p7zip/7z.so dist-extras
endif
ifneq (,$(filter $(ARCH), i386 i486 i586 i686))
	cd dist-extras && \
	wget -O 7z920.exe http://downloads.sourceforge.net/sevenzip/7z920.exe && \
	7z x -y 7z920.exe 7z.exe 7z.dll 7z.sfx && \
	wget -O mingw-libexpat.rpm http://download.opensuse.org/repositories/windows:/mingw:/win32/SLE_11_SP2/noarch/mingw32-libexpat-2.0.1-5.1.noarch.rpm && \
	wget -O mingw-zlib.rpm http://download.opensuse.org/repositories/windows:/mingw:/win32/SLE_11_SP2/noarch/mingw32-zlib-1.2.7-2.2.noarch.rpm
else ifeq ($(ARCH),x86_64)
	cd dist-extras && \
	wget -O 7z920-x64.msi http://downloads.sourceforge.net/sevenzip/7z920-x64.msi && \
	7z x -y 7z920-x64.msi _7z.exe _7z.dll _7z.sfx && \
	mv _7z.dll 7z.dll && \
	mv _7z.exe 7z.exe && \
	mv _7z.sfx 7z.sfx && \
	wget -O mingw-libexpat.rpm http://download.opensuse.org/repositories/windows:/mingw:/win64/SLE_11_SP2/noarch/mingw64-libexpat-2.0.1-4.1.noarch.rpm && \
	wget -O mingw-zlib.rpm http://download.opensuse.org/repositories/windows:/mingw:/win64/SLE_11_SP2/noarch/mingw64-zlib-1.2.7-2.2.noarch.rpm
else
	$(error no win-extras target for ARCH=$(ARCH))
endif
	cd dist-extras && \
	chmod a+x 7z.exe && \
	7z x -y mingw-libexpat.rpm -so > mingw-libexpat.cpio && \
	7z e -y mingw-libexpat.cpio && \
	7z x -y mingw-zlib.rpm -so > mingw-zlib.cpio && \
	7z e -y mingw-zlib.cpio && \
	wget -O PortableGit.7z http://msysgit.googlecode.com/files/PortableGit-1.8.3-preview20130601.7z
