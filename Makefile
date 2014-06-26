JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

# TODO: Code bundled with Julia should be installed into a versioned directory,
# prefix/share/julia/VERSDIR, so that in the future one can have multiple
# major versions of Julia installed concurrently. Third-party code that
# is not controlled by Pkg should be installed into
# prefix/share/julia/site/VERSDIR (not prefix/share/julia/VERSDIR/site ...
# so that prefix/share/julia/VERSDIR can be overwritten without touching
# third-party code).
VERSDIR = v`cut -d. -f1-2 < VERSION`
INSTALL_F = install -pm644
INSTALL_M = install -pm755

all: default
default: release

# sort is used to remove potential duplicates
DIRS = $(sort $(build_bindir) $(build_libdir) $(build_private_libdir) $(build_libexecdir) $(build_sysconfdir)/julia $(build_datarootdir)/julia $(build_datarootdir)/man/man1)

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,base test doc examples,$(eval $(call symlink_target,$(link),$(build_datarootdir)/julia)))

git-submodules:
ifneq ($(NO_GIT), 1)
	@-git submodule update --init
else
       $(warn "Submodules could not be updated because git is unavailible")
endif

debug release: | $(DIRS) $(build_datarootdir)/julia/base $(build_datarootdir)/julia/test $(build_datarootdir)/julia/doc $(build_datarootdir)/julia/examples $(build_sysconfdir)/julia/juliarc.jl
	@$(MAKE) $(QUIET_MAKE) julia-$@
	@export private_libdir=$(private_libdir) && \
	$(MAKE) $(QUIET_MAKE) LD_LIBRARY_PATH=$(build_libdir):$(LD_LIBRARY_PATH) JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$@)" $(build_private_libdir)/sys.$(SHLIB_EXT)

julia-debug-symlink:
	@ln -sf $(build_bindir)/julia-debug julia

julia-release-symlink:
	@ln -sf $(build_bindir)/julia julia

julia-debug julia-release: git-submodules
	@$(MAKE) $(QUIET_MAKE) -C deps
	@$(MAKE) $(QUIET_MAKE) -C src lib$@
	@$(MAKE) $(QUIET_MAKE) -C base
	@$(MAKE) $(QUIET_MAKE) -C ui $@
ifneq ($(OS),WINNT)
ifndef JULIA_VAGRANT_BUILD
	@$(MAKE) $(QUIET_MAKE) $@-symlink
endif
endif

$(build_datarootdir)/julia/helpdb.jl: doc/helpdb.jl | $(build_datarootdir)/julia
	@cp $< $@

$(build_datarootdir)/man/man1/julia.1: doc/man/julia.1 | $(build_datarootdir)/julia
	@mkdir -p $(build_datarootdir)/man/man1
	@cp $< $@

$(build_sysconfdir)/julia/juliarc.jl: etc/juliarc.jl | $(build_sysconfdir)/julia
	@cp $< $@
ifeq ($(OS), WINNT)
	@cat ./contrib/windows/juliarc.jl >> $(build_sysconfdir)/julia/juliarc.jl
$(build_sysconfdir)/julia/juliarc.jl: contrib/windows/juliarc.jl
endif

# use sys.ji if it exists, otherwise run two stages
$(build_private_libdir)/sys%ji: $(build_private_libdir)/sys%o

.PRECIOUS: $(build_private_libdir)/sys%o

$(build_private_libdir)/sys%$(SHLIB_EXT): $(build_private_libdir)/sys%o
	$(CXX) -shared -fPIC -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -o $@ $< \
		$$([ $(OS) = Darwin ] && echo -Wl,-undefined,dynamic_lookup || echo -Wl,--unresolved-symbols,ignore-all ) \
		$$([ $(OS) = WINNT ] && echo -ljulia -lssp)
ifeq ($(OS), Darwin)
ifeq ($(shell test `dsymutil -v | cut -d\- -f2 | cut -d. -f1` -gt 102 && echo yes), yes)
	dsymutil $@
endif
endif

$(build_private_libdir)/sys0.o:
	@$(QUIET_JULIA) cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C $(JULIA_CPU_TARGET) --build $(call cygpath_w,$(build_private_libdir)/sys0) sysimg.jl

$(build_private_libdir)/sys.o: VERSION base/*.jl base/pkg/*.jl base/linalg/*.jl base/sparse/*.jl $(build_datarootdir)/julia/helpdb.jl $(build_datarootdir)/man/man1/julia.1 $(build_private_libdir)/sys0.$(SHLIB_EXT)
	@$(QUIET_JULIA) cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C $(JULIA_CPU_TARGET) --build $(call cygpath_w,$(build_private_libdir)/sys) \
		-J$(call cygpath_w,$(build_private_libdir))/$$([ -e $(build_private_libdir)/sys.ji ] && echo sys.ji || echo sys0.ji) -f sysimg.jl \
		|| (echo "*** This error is usually fixed by running 'make clean'. If the error persists, try 'make cleanall'. ***" && false)

run-julia-debug run-julia-release: run-julia-%:
	$(MAKE) $(QUIET_MAKE) run-julia JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$*)"
run-julia:
	@$(call spawn,$(JULIA_EXECUTABLE))
run:
	@$(call spawn,$(cmd))

$(build_bindir)/stringpatch: $(build_bindir) contrib/stringpatch.c
	@$(call PRINT_CC, $(CC) -o $(build_bindir)/stringpatch contrib/stringpatch.c)


# public libraries, that are installed in $(prefix)/lib
JL_LIBS = julia julia-debug

# private libraries, that are installed in $(prefix)/lib/julia
JL_PRIVATE_LIBS = random suitesparse_wrapper grisu Rmath
ifeq ($(USE_SYSTEM_FFTW),0)
JL_PRIVATE_LIBS += fftw3 fftw3f fftw3_threads fftw3f_threads
endif
ifeq ($(USE_SYSTEM_PCRE),0)
JL_PRIVATE_LIBS += pcre
endif
ifeq ($(USE_SYSTEM_OPENLIBM),0)
ifeq ($(USE_SYSTEM_LIBM),0)
JL_PRIVATE_LIBS += openlibm
endif
endif
ifeq ($(USE_SYSTEM_OPENSPECFUN),0)
JL_PRIVATE_LIBS += openspecfun
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
ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
JL_PRIVATE_LIBS += gfortblas
endif
endif
endif

ifeq ($(OS),WINNT)
define std_dll
debug release: | $$(build_bindir)/lib$(1).dll
$$(build_bindir)/lib$(1).dll: | $$(build_bindir)
ifeq ($$(BUILD_OS),$$(OS))
	cp $$(call pathsearch,lib$(1).dll,$$(PATH)) $$(build_bindir) ;
else
	cp $$(call wine_pathsearch,lib$(1).dll,$$(STD_LIB_PATH)) $$(build_bindir) ;
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
$(eval $(call std_dll,ssp-0))
endif

prefix ?= $(abspath julia-$(JULIA_COMMIT))
install: $(build_bindir)/stringpatch
	@$(MAKE) $(QUIET_MAKE) release
	@$(MAKE) $(QUIET_MAKE) debug
	@for subdir in $(bindir) $(libexecdir) $(datarootdir)/julia/site/$(VERSDIR) $(datarootdir)/man/man1 $(includedir)/julia $(libdir) $(private_libdir) $(sysconfdir); do \
		mkdir -p $(DESTDIR)$$subdir; \
	done

	$(INSTALL_M) $(build_bindir)/julia* $(DESTDIR)$(bindir)/
ifeq ($(OS),WINNT)
	-$(INSTALL_M) $(build_bindir)/*.dll $(build_bindir)/*.bat $(DESTDIR)$(bindir)/
else
	-cp -a $(build_libexecdir) $(DESTDIR)$(prefix)

	for suffix in $(JL_LIBS) ; do \
		$(INSTALL_M) $(build_libdir)/lib$${suffix}*.$(SHLIB_EXT)* $(DESTDIR)$(private_libdir) ; \
	done
	for suffix in $(JL_PRIVATE_LIBS) ; do \
		$(INSTALL_M) $(build_libdir)/lib$${suffix}*.$(SHLIB_EXT)* $(DESTDIR)$(private_libdir) ; \
	done
endif

ifeq ($(USE_SYSTEM_LIBUV),0)
ifeq ($(OS),WINNT)
	$(INSTALL_M) $(build_libdir)/libuv.a $(DESTDIR)$(private_libdir)
	$(INSTALL_F) $(build_includedir)/tree.h $(DESTDIR)$(includedir)/julia
else
	$(INSTALL_M) $(build_libdir)/libuv.a $(DESTDIR)$(private_libdir)
endif
	$(INSTALL_F) $(build_includedir)/uv* $(DESTDIR)$(includedir)/julia
endif
	$(INSTALL_F) src/julia.h src/options.h src/support/*.h $(DESTDIR)$(includedir)/julia
	# Copy system image
	$(INSTALL_F) $(build_private_libdir)/sys.ji $(DESTDIR)$(private_libdir)
	$(INSTALL_M) $(build_private_libdir)/sys.$(SHLIB_EXT) $(DESTDIR)$(private_libdir)
	# Copy in all .jl sources as well
	cp -R -L $(build_datarootdir)/julia $(DESTDIR)$(datarootdir)/
	# Remove git repository of juliadoc
	-rm -rf $(DESTDIR)$(datarootdir)/julia/doc/juliadoc/.git
	-rm $(DESTDIR)$(datarootdir)/julia/doc/juliadoc/.gitignore
	# Copy in beautiful new man page!
	$(INSTALL_F) $(build_datarootdir)/man/man1/julia.1 $(DESTDIR)$(datarootdir)/man/man1/

	# Update RPATH entries of Julia if $(private_libdir_rel) != $(build_private_libdir_rel)
ifneq ($(private_libdir_rel),$(build_private_libdir_rel))
ifeq ($(OS), Darwin)
	for julia in $(DESTDIR)$(bindir)/julia* ; do \
		install_name_tool -rpath @executable_path/$(build_private_libdir_rel) @executable_path/$(private_libdir_rel) $$julia; \
		install_name_tool -rpath @executable_path/$(build_libdir_rel) @executable_path/$(libdir_rel) $$julia; \
	done
else ifeq ($(OS), Linux)
	for julia in $(DESTDIR)$(bindir)/julia* ; do \
		patchelf --set-rpath '$$ORIGIN/$(private_libdir_rel):$$ORIGIN/$(libdir_rel)' $$julia; \
	done
endif
endif

	# Overwrite JL_SYSTEM_IMAGE_PATH in julia binaries:
	for julia in $(DESTDIR)$(bindir)/julia* ; do \
		$(build_bindir)/stringpatch $$(strings -t x - $$julia | grep "sys.ji$$" | awk '{print $$1;}' ) "$(private_libdir_rel)/sys.ji" 256 $(call cygpath_w,$$julia); \
	done

	mkdir -p $(DESTDIR)$(sysconfdir)
	cp -R $(build_sysconfdir)/julia $(DESTDIR)$(sysconfdir)/

dist-clean:
	rm -fr julia-*.tar.gz julia*.exe julia-*.7z julia-$(JULIA_COMMIT)

dist: dist-clean
ifeq ($(USE_SYSTEM_BLAS),0)
ifneq ($(OPENBLAS_DYNAMIC_ARCH),1)
	@echo OpenBLAS must be rebuilt with OPENBLAS_DYNAMIC_ARCH=1 to use dist target
	@false
endif
endif
ifneq ($(prefix),$(abspath julia-$(JULIA_COMMIT)))
	$(error prefix must not be set for make dist)
endif
ifneq ($(DESTDIR),)
	$(error DESTDIR must not be set for make dist)
endif
	@$(MAKE) install
	cp LICENSE.md $(prefix)
ifeq ($(OS), Darwin)
	-./contrib/mac/fixup-libgfortran.sh $(DESTDIR)$(private_libdir)
endif
	# Copy in juliarc.jl files per-platform for binary distributions as well
	# Note that we don't install to sysconfdir: we always install to $(DESTDIR)$(prefix)/etc.
	# If you want to make a distribution with a hardcoded path, you take care of installation
ifeq ($(OS), Darwin)
	-cat ./contrib/mac/juliarc.jl >> $(DESTDIR)$(prefix)/etc/julia/juliarc.jl
else ifeq ($(OS), WINNT)
	-cat ./contrib/windows/juliarc.jl >> $(DESTDIR)$(prefix)/etc/julia/juliarc.jl
endif

	# purge sys.{dll,so,dylib} as that file is not relocatable across processor architectures
ifeq ($(JULIA_CPU_TARGET), native)
	-rm -f $(DESTDIR)$(private_libdir)/sys.$(SHLIB_EXT)
endif

ifeq ($(OS), WINNT)
	[ ! -d dist-extras ] || ( cd dist-extras && \
		cp 7z.exe 7z.dll libexpat-1.dll zlib1.dll $(bindir) && \
	    mkdir $(DESTDIR)$(prefix)/Git && \
	    7z x PortableGit.7z -o"$(DESTDIR)$(prefix)/Git" )
	cd $(DESTDIR)$(bindir) && rm -f llvm* llc.exe lli.exe opt.exe LTO.dll bugpoint.exe macho-dump.exe
	$(call spawn,./dist-extras/nsis/makensis.exe) -NOCD -DVersion=$(JULIA_VERSION) -DArch=$(ARCH) -DCommit=$(JULIA_COMMIT) ./contrib/windows/build-installer.nsi
	./dist-extras/7z a -mx9 "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" julia-installer.exe
	cat ./contrib/windows/7zS.sfx ./contrib/windows/7zSFX-config.txt "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" > "julia-${JULIA_VERSION}-${ARCH}.exe"
	-rm -f julia-installer.exe
else
	$(TAR) zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr $(prefix)


source-dist: git-submodules
	# Save git information
	-@$(MAKE) -C base version_git.jl.phony
	# Get all the dependencies downloaded
	@$(MAKE) -C deps getall

	# Create file source-dist.tmp to hold all the filenames that go into the tarball
	echo "base/version_git.jl" > source-dist.tmp
	git ls-files >> source-dist.tmp
	ls deps/*.tar.gz deps/*.tar.bz2 deps/*.tgz >> source-dist.tmp
	git submodule --quiet foreach 'git ls-files | sed "s&^&$$path/&"' >> source-dist.tmp

	# Remove unwanted files
	sed '/\.git/d' source-dist.tmp > source-dist.tmp1
	sed '/\.travis/d' source-dist.tmp1 > source-dist.tmp

	# Create tarball
	tar -cz -T source-dist.tmp --no-recursion -f julia-$(JULIA_VERSION)_$(JULIA_COMMIT).tar.gz
	rm -f source-dist.tmp source-dist.tmp1

clean: | $(CLEAN_TARGETS)
	@$(MAKE) -C base clean
	@$(MAKE) -C src clean
	@$(MAKE) -C ui clean
	for repltype in "basic" "readline"; do \
		rm -f $(build_bindir)/julia-debug-$${repltype}; \
		rm -f $(build_bindir)/julia-$${repltype}; \
	done
	@rm -f julia
	@rm -f *~ *# *.tar.gz
	@rm -f $(build_bindir)/stringpatch source-dist.tmp source-dist.tmp1
	@rm -fr $(build_private_libdir)
# Temporarily add this line to the Makefile to remove extras
	@rm -fr $(build_datarootdir)/julia/extras

cleanall: clean
	@$(MAKE) -C src clean-flisp clean-support
	@rm -fr $(build_libdir)
ifeq ($(OS),WINNT)
	@rm -rf $(build_prefix)/lib
endif
	@$(MAKE) -C deps clean-uv

distcleanall: cleanall
	@$(MAKE) -C deps distcleanall
	@$(MAKE) -C doc cleanall
	rm -fr $(build_prefix)

.PHONY: default debug release julia-debug julia-release \
	test testall testall1 test-* clean distcleanall cleanall \
	run-julia run-julia-debug run-julia-release run \
	install dist source-dist git-submodules

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
ifeq (,$(findstring CYGWIN,$(BUILD_OS)))
	cp /usr/lib/p7zip/7z /usr/lib/p7zip/7z.so dist-extras
endif
endif
ifneq (,$(filter $(ARCH), i386 i486 i586 i686))
	cd dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z920.exe && \
	7z x -y 7z920.exe 7z.exe 7z.dll && \
	$(JLDOWNLOAD) http://download.opensuse.org/repositories/windows:/mingw:/win32/SLE_11_SP3/noarch/mingw32-libexpat-2.0.1-6.6.noarch.rpm && \
	mv mingw32-libexpat-*.rpm mingw-libexpat.rpm && \
	$(JLDOWNLOAD) http://download.opensuse.org/repositories/windows:/mingw:/win32/SLE_11_SP3/noarch/mingw32-zlib-1.2.8-2.4.noarch.rpm && \
	mv mingw32-zlib-*.rpm mingw-zlib.rpm
else ifeq ($(ARCH),x86_64)
	cd dist-extras && \
	$(JLDOWNLOAD) 7z920-x64.msi http://downloads.sourceforge.net/sevenzip/7z920-x64.msi && \
	7z x -y 7z920-x64.msi _7z.exe _7z.dll && \
	mv _7z.dll 7z.dll && \
	mv _7z.exe 7z.exe && \
	$(JLDOWNLOAD) http://download.opensuse.org/repositories/windows:/mingw:/win64/SLE_11_SP3/noarch/mingw64-libexpat-2.0.1-5.8.noarch.rpm && \
	mv mingw64-libexpat-*.rpm mingw-libexpat.rpm && \
	$(JLDOWNLOAD) http://download.opensuse.org/repositories/windows:/mingw:/win64/SLE_11_SP3/noarch/mingw64-zlib-1.2.8-2.1.noarch.rpm && \
	mv mingw64-zlib-*.rpm mingw-zlib.rpm
else
	$(error no win-extras target for ARCH=$(ARCH))
endif
	cd dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z920_extra.7z && \
	$(JLDOWNLOAD) https://unsis.googlecode.com/files/nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x 7z.exe && \
	chmod a+x 7z.dll && \
	$(call spawn,./7z.exe) x -y -onsis nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x ./nsis/makensis.exe && \
	7z x -y mingw-libexpat.rpm -so > mingw-libexpat.cpio && \
	7z e -y mingw-libexpat.cpio && \
	7z x -y mingw-zlib.rpm -so > mingw-zlib.cpio && \
	7z e -y mingw-zlib.cpio && \
	$(JLDOWNLOAD) PortableGit.7z http://msysgit.googlecode.com/files/PortableGit-1.8.3-preview20130601.7z
