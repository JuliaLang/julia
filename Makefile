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

#file name of make binary-dist result
ifeq ($(JULIA_BINARYDIST_TARNAME),)
	JULIA_BINARYDIST_TARNAME = julia-$(JULIA_COMMIT)-$(OS)-$(ARCH)
endif

default: $(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: debug release

# sort is used to remove potential duplicates
DIRS = $(sort $(build_bindir) $(build_libdir) $(build_private_libdir) $(build_libexecdir) $(build_sysconfdir)/julia $(build_datarootdir)/julia $(build_man1dir))

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,base test,$(eval $(call symlink_target,$(link),$(build_datarootdir)/julia)))

# Build the HTML docs (skipped if already exists, notably in tarballs)
doc/_build/html:
	@$(MAKE) -C doc html

# doc needs to live under $(build_docdir), not under $(build_datarootdir)/julia/
CLEAN_TARGETS += clean-$(build_docdir)
clean-$(build_docdir):
	@-rm -fr $(abspath $(build_docdir))
$(subst $(abspath $(JULIAHOME))/,,$(abspath $(build_docdir))): $(build_docdir)
$(build_docdir):
	@mkdir -p $@/examples
	@cp -R examples/*.jl $@/examples/
	@cp -R examples/clustermanager $@/examples/

julia-symlink: julia-ui-$(JULIA_BUILD_MODE)
ifneq ($(OS),WINNT)
ifndef JULIA_VAGRANT_BUILD
	@ln -sf "$(shell contrib/relative_path.sh "$(JULIAHOME)" "$(JULIA_EXECUTABLE)")" julia
endif
endif

julia-deps: | $(DIRS) $(build_datarootdir)/julia/base $(build_datarootdir)/julia/test $(build_docdir) $(build_sysconfdir)/julia/juliarc.jl $(build_man1dir)/julia.1
	@$(MAKE) $(QUIET_MAKE) -C deps

julia-base: julia-deps
	@$(MAKE) $(QUIET_MAKE) -C base

julia-libccalltest:
	@$(MAKE) $(QUIET_MAKE) -C test libccalltest

julia-src-release julia-src-debug : julia-src-% : julia-deps
	@$(MAKE) $(QUIET_MAKE) -C src libjulia-$*

julia-ui-release julia-ui-debug : julia-ui-% : julia-src-%
	@$(MAKE) $(QUIET_MAKE) -C ui julia-$*

julia-sysimg : julia-base julia-ui-$(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) $(build_private_libdir)/sys.$(SHLIB_EXT) JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

julia-debug julia-release : julia-% : julia-ui-% julia-symlink julia-sysimg julia-libccalltest

debug release : % : julia-%

check-whitespace:
ifneq ($(NO_GIT), 1)
	@contrib/check-whitespace.sh
else
	$(warn "Skipping whitespace check because git is unavailable")
endif

release-candidate: release test
	@#Check documentation
	@./julia doc/NEWS-update.jl #Add missing cross-references to NEWS.md
	@$(MAKE) -C doc unicode #Rebuild Unicode table if necessary
	@./julia doc/DocCheck.jl > doc/UNDOCUMENTED.rst 2>&1 #Check for undocumented items
	@if [ -z "$(cat doc/UNDOCUMENTED.rst)" ]; then \
		rm doc/UNDOCUMENTED.rst; \
	else \
		echo "Undocumented functions found in doc/UNDOCUMENTED.rst; document them, then retry"; \
		exit 1; \
	fi
	@$(MAKE) -C doc html  SPHINXOPTS="-n" #Rebuild Julia HTML docs pedantically
	@$(MAKE) -C doc latex SPHINXOPTS="-n" #Rebuild Julia PDF docs pedantically
	@$(MAKE) -C doc doctest #Run Julia doctests
	@$(MAKE) -C doc linkcheck #Check all links
	@$(MAKE) -C doc helpdb.jl #Rebuild Julia online documentation for help(), apropos(), etc...

	@# Check to see if the above make invocations changed anything important
	@if [ -n "$$(git status --porcelain)" ]; then \
		echo "Git repository dirty; Verify and commit changes to the repository, then retry"; \
		exit 1; \
	fi

	@#Check that benchmarks work
	@$(MAKE) -C test/perf
	@#Check that netload tests work
	@#for test in test/netload/*.jl; do julia $$test; if [ $$? -ne 0 ]; then exit 1; fi; done
	@echo
	@echo To complete the release candidate checklist:
	@echo

	@echo 1. Remove deprecations in base/deprecated.jl
	@echo 2. Bump VERSION
	@echo 3. Create tag, push to github "\(git tag v\`cat VERSION\` && git push --tags\)"		#"` # These comments deal with incompetent syntax highlighting rules
	@echo 4. Clean out old .tar.gz files living in deps/, "\`git clean -fdx\`" seems to work	#"`
	@echo 5. Replace github release tarball with tarballs created from make light-source-dist and make full-source-dist
	@echo 6. Follow packaging instructions in DISTRIBUTING.md to create binary packages for all platforms
	@echo 7. Upload to AWS, update http://julialang.org/downloads and http://status.julialang.org/stable links
	@echo 8. Announce on mailing lists
	@echo 9. Change master to release-0.X in base/version.jl and base/version_git.sh as in 4cb1e20
	@echo

$(build_docdir)/helpdb.jl: doc/helpdb.jl | $(build_docdir)
	@cp $< $@

$(build_man1dir)/julia.1: doc/man/julia.1 | $(build_man1dir)
	@mkdir -p $(build_man1dir)
	@cp $< $@

$(build_sysconfdir)/julia/juliarc.jl: etc/juliarc.jl | $(build_sysconfdir)/julia
	@cp $< $@
ifeq ($(OS), WINNT)
	@cat ./contrib/windows/juliarc.jl >> $(build_sysconfdir)/julia/juliarc.jl
$(build_sysconfdir)/julia/juliarc.jl: contrib/windows/juliarc.jl
endif

.SECONDARY: $(build_private_libdir)/inference0.o
.SECONDARY: $(build_private_libdir)/inference.o
.SECONDARY: $(build_private_libdir)/sys.o

$(build_private_libdir)/%.$(SHLIB_EXT): $(build_private_libdir)/%.o
ifneq ($(USEMSVC), 1)
	@$(call PRINT_LINK, $(CXX) -shared -fPIC -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -o $@ $< \
		$$([ $(OS) = Darwin ] && echo '' -Wl,-undefined,dynamic_lookup || echo '' -Wl,--unresolved-symbols,ignore-all ) \
		$$([ $(OS) = WINNT ] && echo '' -ljulia -lssp))
	$(DSYMUTIL) $@
else
	@true
endif

CORE_SRCS := base/boot.jl base/coreimg.jl \
		base/abstractarray.jl \
		base/array.jl \
		base/bool.jl \
		base/build_h.jl \
		base/dict.jl \
		base/error.jl \
		base/essentials.jl \
		base/expr.jl \
		base/functors.jl \
		base/hashing.jl \
		base/inference.jl \
		base/int.jl \
		base/intset.jl \
		base/iterator.jl \
		base/nofloat_hashing.jl \
		base/number.jl \
		base/operators.jl \
		base/options.jl \
		base/pointer.jl \
		base/promotion.jl \
		base/range.jl \
		base/reduce.jl \
		base/reflection.jl \
		base/subarray.jl \
		base/subarray2.jl \
		base/tuple.jl

BASE_SRCS := $(wildcard base/*.jl base/*/*.jl base/*/*/*.jl)

$(build_private_libdir)/inference0.o: $(CORE_SRCS) | $(build_private_libdir)
	@$(call PRINT_JULIA, cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C $(JULIA_CPU_TARGET) --build $(call cygpath_w,$(build_private_libdir)/inference0) -f \
		coreimg.jl)

$(build_private_libdir)/inference.o: $(build_private_libdir)/inference0.$(SHLIB_EXT)
	@$(call PRINT_JULIA, cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C $(JULIA_CPU_TARGET) --build $(call cygpath_w,$(build_private_libdir)/inference) -f \
		-J $(call cygpath_w,$(build_private_libdir)/inference0.ji) coreimg.jl)

COMMA:=,
$(build_private_libdir)/sys.o: VERSION $(BASE_SRCS) $(build_docdir)/helpdb.jl $(build_private_libdir)/inference.$(SHLIB_EXT)
	@$(call PRINT_JULIA, cd base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C $(JULIA_CPU_TARGET) --build $(call cygpath_w,$(build_private_libdir)/sys) -f \
		-J $(call cygpath_w,$(build_private_libdir)/inference.ji) sysimg.jl \
		|| { echo '*** This error is usually fixed by running `make clean`. If the error persists$(COMMA) try `make cleanall`. ***' && false; } )

$(build_bindir)/stringreplace: contrib/stringreplace.c | $(build_bindir)
	@$(call PRINT_CC, $(CC) -o $(build_bindir)/stringreplace contrib/stringreplace.c)


# public libraries, that are installed in $(prefix)/lib
JL_LIBS = julia julia-debug

# private libraries, that are installed in $(prefix)/lib/julia
JL_PRIVATE_LIBS = suitesparse_wrapper Rmath
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
ifeq ($(USE_SYSTEM_DSFMT),0)
JL_PRIVATE_LIBS += dSFMT
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
ifeq ($(USE_SYSTEM_LIBGIT2),0)
JL_PRIVATE_LIBS += git2
endif
ifeq ($(USE_SYSTEM_ARPACK),0)
JL_PRIVATE_LIBS += arpack
endif
ifeq ($(USE_SYSTEM_SUITESPARSE),0)
JL_PRIVATE_LIBS += amd camd ccolamd cholmod colamd umfpack spqr suitesparseconfig
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
julia-deps: | $$(build_bindir)/lib$(1).dll
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

install: $(build_bindir)/stringreplace doc/_build/html
	@$(MAKE) $(QUIET_MAKE) all
	@for subdir in $(bindir) $(libexecdir) $(datarootdir)/julia/site/$(VERSDIR) $(docdir) $(man1dir) $(includedir)/julia $(libdir) $(private_libdir) $(sysconfdir); do \
		mkdir -p $(DESTDIR)$$subdir; \
	done

	$(INSTALL_M) $(build_bindir)/julia* $(DESTDIR)$(bindir)/
ifeq ($(OS),WINNT)
	-$(INSTALL_M) $(build_bindir)/*.dll $(DESTDIR)$(bindir)/
else
	-cp -a $(build_libexecdir) $(DESTDIR)$(prefix)

	# Copy over .dSYM directories directly
ifeq ($(OS),Darwin)
	-cp -a $(build_libdir)/*.dSYM $(DESTDIR)$(private_libdir)
endif

	for suffix in $(JL_LIBS) ; do \
		for lib in $(build_libdir)/lib$${suffix}*.$(SHLIB_EXT)*; do \
			if [ "$${lib##*.}" != "dSYM" ]; then \
				$(INSTALL_M) $$lib $(DESTDIR)$(private_libdir) ; \
			fi \
		done \
	done
	for suffix in $(JL_PRIVATE_LIBS) ; do \
		for lib in $(build_libdir)/lib$${suffix}*.$(SHLIB_EXT)*; do \
			if [ "$${lib##*.}" != "dSYM" ]; then \
				$(INSTALL_M) $$lib $(DESTDIR)$(private_libdir) ; \
			fi \
		done \
	done

	# Copy in libssl and libcrypto if they exist
ifeq ($(OS),Linux)
	-$(INSTALL_M) $(build_libdir)/libssl*.so* $(DESTDIR)$(private_libdir)
	-$(INSTALL_M) $(build_libdir)/libcrypto*.so* $(DESTDIR)$(private_libdir)
endif
endif

ifeq ($(USE_SYSTEM_LIBUV),0)
ifeq ($(OS),WINNT)
	$(INSTALL_F) $(build_includedir)/tree.h $(DESTDIR)$(includedir)/julia
endif
	$(INSTALL_F) $(build_includedir)/uv* $(DESTDIR)$(includedir)/julia
endif
	$(INSTALL_F) src/julia.h src/julia_version.h src/options.h src/support/*.h $(DESTDIR)$(includedir)/julia
	# Copy system image
	$(INSTALL_F) $(build_private_libdir)/sys.ji $(DESTDIR)$(private_libdir)
	$(INSTALL_M) $(build_private_libdir)/sys.$(SHLIB_EXT) $(DESTDIR)$(private_libdir)
	# Copy in system image build script
	$(INSTALL_M) contrib/build_sysimg.jl $(DESTDIR)$(datarootdir)/julia/
	# Copy in standalone executable build script
	$(INSTALL_M) contrib/build_executable.jl $(DESTDIR)$(datarootdir)/julia/
	# Copy in standalone julia-config script
	$(INSTALL_M) contrib/julia-config.jl $(DESTDIR)$(datarootdir)/julia/
	# Copy in all .jl sources as well
	cp -R -L $(build_datarootdir)/julia $(DESTDIR)$(datarootdir)/
	# Copy documentation
	cp -R -L $(build_docdir)/* $(DESTDIR)$(docdir)/
	cp -R -L doc/_build/html $(DESTDIR)$(docdir)/
	-rm $(DESTDIR)$(docdir)/html/.buildinfo
	# Remove perf suite
	-rm -rf $(DESTDIR)$(datarootdir)/julia/test/perf/
	# Remove various files which should not be installed
	-rm -f $(DESTDIR)$(datarootdir)/julia/base/version_git.sh
	-rm -f $(DESTDIR)$(datarootdir)/julia/test/Makefile
	# Copy in beautiful new man page
	$(INSTALL_F) $(build_man1dir)/julia.1 $(DESTDIR)$(man1dir)/
	# Copy icon and .desktop file
	mkdir -p $(DESTDIR)$(datarootdir)/icons/hicolor/scalable/apps/
	$(INSTALL_F) contrib/julia.svg $(DESTDIR)$(datarootdir)/icons/hicolor/scalable/apps/
	-touch -c $(DESTDIR)$(datarootdir)/icons/hicolor/
	-gtk-update-icon-cache $(DESTDIR)$(datarootdir)/icons/hicolor/
	mkdir -p $(DESTDIR)$(datarootdir)/applications/
	$(INSTALL_F) contrib/julia.desktop $(DESTDIR)$(datarootdir)/applications/
	# Install appdata file
	mkdir -p $(DESTDIR)$(datarootdir)/appdata/
	$(INSTALL_F) contrib/julia.appdata.xml $(DESTDIR)$(datarootdir)/appdata/

	# Update RPATH entries and JL_SYSTEM_IMAGE_PATH if $(private_libdir_rel) != $(build_private_libdir_rel)
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

	# Overwrite JL_SYSTEM_IMAGE_PATH in julia library
	for julia in $(DESTDIR)$(private_libdir)/libjulia*.$(SHLIB_EXT) ; do \
		$(call spawn,$(build_bindir)/stringreplace $$(strings -t x - $$julia | grep "sys.ji$$" | awk '{print $$1;}' ) "$(private_libdir_rel)/sys.ji" 256 $(call cygpath_w,$$julia)); \
	done
endif

	mkdir -p $(DESTDIR)$(sysconfdir)
	cp -R $(build_sysconfdir)/julia $(DESTDIR)$(sysconfdir)/

distclean dist-clean:
	rm -fr julia-*.tar.gz julia*.exe julia-*.7z julia-$(JULIA_COMMIT)

dist:
	@echo \'dist\' target is deprecated: use \'binary-dist\' instead.

binary-dist: distclean
ifeq ($(USE_SYSTEM_BLAS),0)
ifneq ($(OPENBLAS_DYNAMIC_ARCH),1)
	@echo OpenBLAS must be rebuilt with OPENBLAS_DYNAMIC_ARCH=1 to use binary-dist target
	@false
endif
endif
ifneq ($(prefix),$(abspath julia-$(JULIA_COMMIT)))
	$(error prefix must not be set for make binary-dist)
endif
ifneq ($(DESTDIR),)
	$(error DESTDIR must not be set for make binary-dist)
endif
	@$(MAKE) install
	cp LICENSE.md $(prefix)
ifneq ($(OS), WINNT)
	-./contrib/fixup-libgfortran.sh $(DESTDIR)$(private_libdir)
endif
ifeq ($(OS), Linux)
	-./contrib/fixup-libstdc++.sh $(DESTDIR)$(private_libdir)
endif
	# Copy in juliarc.jl files per-platform for binary distributions as well
	# Note that we don't install to sysconfdir: we always install to $(DESTDIR)$(prefix)/etc.
	# If you want to make a distribution with a hardcoded path, you take care of installation
ifeq ($(OS), Darwin)
	-cat ./contrib/mac/juliarc.jl >> $(DESTDIR)$(prefix)/etc/julia/juliarc.jl
endif

	# purge sys.{dll,so,dylib} as that file is not relocatable across processor architectures
ifeq ($(JULIA_CPU_TARGET), native)
	-rm -f $(DESTDIR)$(private_libdir)/sys.$(SHLIB_EXT)
endif

ifeq ($(OS), WINNT)
ifeq ($(ARCH),x86_64)
	# If we are running on WIN64, also delete sys.dll until we switch to llvm3.5+
	-rm -f $(DESTDIR)$(private_libdir)/sys.$(SHLIB_EXT)
endif

	[ ! -d dist-extras ] || ( cd dist-extras && \
		cp 7z.exe 7z.dll libexpat-1.dll zlib1.dll libgfortran-3.dll libquadmath-0.dll libstdc++-6.dll libgcc_s_s*-1.dll libssp-0.dll $(bindir) && \
	    mkdir $(DESTDIR)$(prefix)/Git && \
	    7z x PortableGit.7z -o"$(DESTDIR)$(prefix)/Git" && \
	    echo "[core] eol = lf" >> "$(DESTDIR)$(prefix)/Git/etc/gitconfig" && \
	    sed -i "s/\bautocrlf = true$$/autocrlf = input/" "$(DESTDIR)$(prefix)/Git/etc/gitconfig" && \
	    cp busybox.exe $(DESTDIR)$(prefix)/Git/bin/echo.exe && \
	    cp busybox.exe $(DESTDIR)$(prefix)/Git/bin/printf.exe )
	cd $(DESTDIR)$(bindir) && rm -f llvm* llc.exe lli.exe opt.exe LTO.dll bugpoint.exe macho-dump.exe

	# create file listing for uninstall. note: must have Windows path separators and line endings.
	cd $(prefix) && find * | sed -e 's/\//\\/g' -e 's/$$/\r/g' > etc/uninstall.log

	# build nsis package
	$(call spawn,./dist-extras/nsis/makensis.exe) -NOCD -DVersion=$(JULIA_VERSION) -DArch=$(ARCH) -DCommit=$(JULIA_COMMIT) ./contrib/windows/build-installer.nsi

	# compress nsis installer and combine with 7zip self-extracting header
	./dist-extras/7z a -mx9 "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" julia-installer.exe
	cat ./contrib/windows/7zS.sfx ./contrib/windows/7zSFX-config.txt "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" > "julia-${JULIA_VERSION}-${ARCH}.exe"
	-rm -f julia-installer.exe
else
	$(TAR) zcvf $(JULIA_BINARYDIST_TARNAME).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr $(prefix)

light-source-dist.tmp: doc/_build/html
	# Save git information
	-@$(MAKE) -C base version_git.jl.phony

	# Create file light-source-dist.tmp to hold all the filenames that go into the tarball
	echo "base/version_git.jl" > light-source-dist.tmp
	git ls-files | sed -e '/\.git/d' -e '/\.travis/d' >> light-source-dist.tmp
	find doc/_build/html >> light-source-dist.tmp

# Make tarball with only Julia code
light-source-dist: light-source-dist.tmp
	# Prefix everything with the current directory name (usually "julia"), then create tarball
	DIRNAME=$$(basename $$(pwd)); \
	sed -e "s_.*_$$DIRNAME/&_" light-source-dist.tmp > light-source-dist.tmp1; \
	cd ../ && tar -cz -T $$DIRNAME/light-source-dist.tmp1 --no-recursion -f $$DIRNAME/julia-$(JULIA_VERSION)_$(JULIA_COMMIT).tar.gz

source-dist:
	@echo \'source-dist\' target is deprecated: use \'full-source-dist\' instead.

# Make tarball with Julia code plus all dependencies
full-source-dist: light-source-dist.tmp
	# Get all the dependencies downloaded
	@$(MAKE) -C deps getall NO_GIT=1

	# Create file full-source-dist.tmp to hold all the filenames that go into the tarball
	cp light-source-dist.tmp full-source-dist.tmp
	-ls deps/*.tar.gz deps/*.tar.bz2 deps/*.tar.xz deps/*.tgz deps/*.zip >> full-source-dist.tmp

	# Prefix everything with the current directory name (usually "julia"), then create tarball
	DIRNAME=$$(basename $$(pwd)); \
	sed -e "s_.*_$$DIRNAME/&_" full-source-dist.tmp > full-source-dist.tmp1; \
	cd ../ && tar -cz -T $$DIRNAME/full-source-dist.tmp1 --no-recursion -f $$DIRNAME/julia-$(JULIA_VERSION)_$(JULIA_COMMIT)-full.tar.gz

clean: | $(CLEAN_TARGETS)
	@$(MAKE) -C base clean
	@$(MAKE) -C doc clean
	@$(MAKE) -C src clean
	@$(MAKE) -C ui clean
	@$(MAKE) -C test clean
	@rm -f julia
	@rm -f *~ *# *.tar.gz
	@rm -f $(build_bindir)/stringreplace \
		light-source-dist.tmp light-source-dist.tmp1 \
		full-source-dist.tmp full-source-dist.tmp1
	@rm -fr $(build_private_libdir)
# Temporarily add this line to the Makefile to remove extras
	@rm -fr $(build_datarootdir)/julia/extras

cleanall: clean
	@$(MAKE) -C src clean-flisp clean-support
	@rm -fr $(build_shlibdir)
ifeq ($(OS),WINNT)
	@rm -rf $(build_prefix)/lib
endif
	@$(MAKE) -C deps clean-libuv

distcleanall: cleanall
	@$(MAKE) -C deps distcleanall
	@$(MAKE) -C doc cleanall
	rm -fr $(build_prefix)

.PHONY: default debug release check-whitespace release-candidate \
	julia-debug julia-release julia-deps \
	julia-ui-release julia-ui-debug julia-src-release julia-src-debug \
	julia-symlink julia-base julia-sysimg \
	test testall testall1 test clean distcleanall cleanall \
	run-julia run-julia-debug run-julia-release run \
	install binary-dist light-source-dist.tmp light-source-dist \
	dist full-source-dist source-dist

test: check-whitespace $(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C test default JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

testall: check-whitespace $(JULIA_BUILD_MODE)
	cp $(build_prefix)/lib/julia/sys.ji local.ji && $(JULIA_EXECUTABLE) -J local.ji -e 'true' && rm local.ji
	@$(MAKE) $(QUIET_MAKE) -C test all JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

testall1: check-whitespace $(JULIA_BUILD_MODE)
	@env JULIA_CPU_CORES=1 $(MAKE) $(QUIET_MAKE) -C test all JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

test-%: check-whitespace $(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C test $* JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

perf: release
	@$(MAKE) $(QUIET_MAKE) -C test/perf JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

perf-%: release
	@$(MAKE) $(QUIET_MAKE) -C test/perf $* JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

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
	../contrib/windows/winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win32/openSUSE_13.1 \
	"mingw32-libgfortran3 mingw32-libquadmath0 mingw32-libstdc++6 mingw32-libgcc_s_sjlj1 mingw32-libssp0 mingw32-libexpat1 mingw32-zlib1"
else ifeq ($(ARCH),x86_64)
	cd dist-extras && \
	$(JLDOWNLOAD) 7z920-x64.msi http://downloads.sourceforge.net/sevenzip/7z920-x64.msi && \
	7z x -y 7z920-x64.msi _7z.exe _7z.dll && \
	mv _7z.dll 7z.dll && \
	mv _7z.exe 7z.exe && \
	../contrib/windows/winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win64/openSUSE_13.1 \
	"mingw64-libgfortran3 mingw64-libquadmath0 mingw64-libstdc++6 mingw64-libgcc_s_seh1 mingw64-libssp0 mingw64-libexpat1 mingw64-zlib1"
else
	$(error no win-extras target for ARCH=$(ARCH))
endif
	cd dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z920_extra.7z && \
	$(JLDOWNLOAD) https://unsis.googlecode.com/files/nsis-2.46.5-Unicode-setup.exe && \
	$(JLDOWNLOAD) busybox.exe http://intgat.tigress.co.uk/rmy/files/busybox/busybox-w32-TIG-1778-g15efec6.exe && \
	chmod a+x 7z.exe && \
	chmod a+x 7z.dll && \
	$(call spawn,./7z.exe) x -y -onsis nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x ./nsis/makensis.exe && \
	chmod a+x busybox.exe && \
	$(JLDOWNLOAD) PortableGit.7z https://github.com/msysgit/msysgit/releases/download/Git-1.9.5-preview20141217/PortableGit-1.9.5-preview20141217.7z
