JULIAHOME := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
include $(JULIAHOME)/Make.inc

# TODO: Code bundled with Julia should be installed into a versioned directory,
# prefix/share/julia/VERSDIR, so that in the future one can have multiple
# major versions of Julia installed concurrently. Third-party code that
# is not controlled by Pkg should be installed into
# prefix/share/julia/site/VERSDIR (not prefix/share/julia/VERSDIR/site ...
# so that prefix/share/julia/VERSDIR can be overwritten without touching
# third-party code).
VERSDIR := v`cut -d. -f1-2 < $(JULIAHOME)/VERSION`

#file name of make binary-dist result
ifeq ($(JULIA_BINARYDIST_TARNAME),)
	JULIA_BINARYDIST_TARNAME := julia-$(JULIA_COMMIT)-$(OS)-$(ARCH)
endif

default: $(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: debug release

# sort is used to remove potential duplicates
DIRS := $(sort $(build_bindir) $(build_depsbindir) $(build_libdir) $(build_private_libdir) $(build_libexecdir) $(build_sysconfdir)/julia $(build_datarootdir)/julia $(build_man1dir))
ifneq ($(BUILDROOT),$(JULIAHOME))
BUILDDIRS := $(BUILDROOT) $(addprefix $(BUILDROOT)/,base src ui doc deps test test/perf)
BUILDDIRMAKE := $(addsuffix /Makefile,$(BUILDDIRS))
DIRS := $(DIRS) $(BUILDDIRS)
$(BUILDDIRMAKE): | $(BUILDDIRS)
	@# add Makefiles to the build directories for convenience (pointing back to the source location of each)
	@echo '# -- This file is automatically generated in julia/Makefile -- #' > $@
	@echo 'BUILDROOT=$(BUILDROOT)' >> $@
	@echo 'include $(JULIAHOME)$(patsubst $(BUILDROOT)%,%,$@)' >> $@
julia-deps: | $(BUILDDIRMAKE)
configure-y: | $(BUILDDIRMAKE)
configure:
ifeq ("$(origin O)", "command line")
	@if [ "$$(ls '$(BUILDROOT)' 2> /dev/null)" ]; then \
		echo 'WARNING: configure called on non-empty directory $(BUILDROOT)'; \
		read -p "Proceed [y/n]? " answer; \
	else \
		answer=y;\
	fi; \
	[ $$answer = 'y' ] && $(MAKE) configure-$$answer
else
	$(error "cannot rerun configure from within a build directory")
endif
else
configure:
	$(error "must specify O=builddir to run the Julia `make configure` target")
endif

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,base test,$(eval $(call symlink_target,$(link),$(build_datarootdir)/julia)))

julia_flisp.boot.inc.phony: julia-deps
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src julia_flisp.boot.inc.phony

# Build the HTML docs (skipped if already exists, notably in tarballs)
$(BUILDROOT)/doc/_build/html:
	@$(MAKE) -C $(BUILDROOT)/doc html

# doc needs to live under $(build_docdir), not under $(build_datarootdir)/julia/
CLEAN_TARGETS += clean-docdir
clean-docdir:
	@-rm -fr $(abspath $(build_docdir))
$(build_prefix)/.examples: $(wildcard $(JULIAHOME)/examples/*.jl) $(shell find $(JULIAHOME)/examples/clustermanager)
	@echo Copying in usr/share/doc/julia/examples
	@-rm -fr $(build_docdir)/examples
	@mkdir -p $(build_docdir)/examples
	@cp -R $(JULIAHOME)/examples/*.jl $(build_docdir)/examples/
	@cp -R $(JULIAHOME)/examples/clustermanager $(build_docdir)/examples/
	@echo 1 > $@

julia-symlink: julia-ui-$(JULIA_BUILD_MODE)
ifneq ($(OS),WINNT)
ifndef JULIA_VAGRANT_BUILD
	@ln -sf "$(shell $(JULIAHOME)/contrib/relative_path.sh "$(BUILDROOT)" "$(JULIA_EXECUTABLE)")" $(BUILDROOT)/julia
endif
endif

julia-deps: | $(DIRS) $(build_datarootdir)/julia/base $(build_datarootdir)/julia/test
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/deps

julia-base: julia-deps $(build_sysconfdir)/julia/juliarc.jl $(build_man1dir)/julia.1
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/base

julia-libccalltest: julia-deps
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src libccalltest

julia-src-release julia-src-debug : julia-src-% : julia-deps julia_flisp.boot.inc.phony
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src libjulia-$*

julia-ui-release julia-ui-debug : julia-ui-% : julia-src-%
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/ui julia-$*

julia-inference : julia-base julia-ui-$(JULIA_BUILD_MODE) $(build_prefix)/.examples
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT) $(build_private_libdir)/inference.ji JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

julia-sysimg-release : julia-inference julia-ui-release
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT) $(build_private_libdir)/sys.$(SHLIB_EXT) JULIA_BUILD_MODE=release

julia-sysimg-debug : julia-inference julia-ui-debug
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT) $(build_private_libdir)/sys-debug.$(SHLIB_EXT) JULIA_BUILD_MODE=debug

julia-debug julia-release : julia-% : julia-ui-% julia-sysimg-% julia-symlink julia-libccalltest

debug release : % : julia-%

julia-genstdlib: julia-sysimg-$(JULIA_BUILD_MODE)
	@$(call PRINT_JULIA, $(JULIA_EXECUTABLE) $(call cygpath_w, $(JULIAHOME)/doc/genstdlib.jl))

docs: julia-genstdlib
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/doc

check-whitespace:
ifneq ($(NO_GIT), 1)
	@$(JULIAHOME)/contrib/check-whitespace.sh
else
	$(warn "Skipping whitespace check because git is unavailable")
endif

release-candidate: release testall
	@$(JULIA_EXECUTABLE) $(JULIAHOME)/contrib/add_license_to_files.jl #add license headers
	@$(JULIA_EXECUTABLE) $(JULIAHOME)/doc/genstdlib.jl
	@#Check documentation
	@$(JULIA_EXECUTABLE) $(JULIAHOME)/doc/NEWS-update.jl #Add missing cross-references to NEWS.md
	@$(MAKE) -C $(BUILDROOT)/doc unicode #Rebuild Unicode table if necessary
	@$(JULIA_EXECUTABLE) $(JULIAHOME)/doc/DocCheck.jl > $(BUILDROOT)/doc/UNDOCUMENTED.rst 2>&1 #Check for undocumented items
	@if [ -z "$(cat $(BUILDROOT)/doc/UNDOCUMENTED.rst)" ]; then \
		rm $(BUILDROOT)/doc/UNDOCUMENTED.rst; \
	else \
		echo "Undocumented functions found in doc/UNDOCUMENTED.rst; document them, then retry"; \
		exit 1; \
	fi
	@$(MAKE) -C $(BUILDROOT)/doc html  SPHINXOPTS="-n" #Rebuild Julia HTML docs pedantically
	@$(MAKE) -C $(BUILDROOT)/doc latex SPHINXOPTS="-n" #Rebuild Julia PDF docs pedantically
	@$(MAKE) -C $(BUILDROOT)/doc doctest #Run Julia doctests
	@$(MAKE) -C $(BUILDROOT)/doc linkcheck #Check all links

	@# Check to see if the above make invocations changed anything important
	@if [ -n "$$(git status --porcelain)" ]; then \
		echo "Git repository dirty; Verify and commit changes to the repository, then retry"; \
		exit 1; \
	fi

	@#Check that benchmarks work
	@$(MAKE) -C $(BUILDROOT)/test/perf
	@#Check that netload tests work
	@#for test in test/netload/*.jl; do julia $$test; if [ $$? -ne 0 ]; then exit 1; fi; done
	@echo
	@echo To complete the release candidate checklist:
	@echo

	@echo 1. Remove deprecations in base/deprecated.jl
	@echo 2. Update references to the julia version in the source directories, such as in README.md
	@echo 3. Bump VERSION
	@echo 4. Increase SOMAJOR and SOMINOR if needed.
	@echo 5. Create tag, push to github "\(git tag v\`cat VERSION\` && git push --tags\)"		#"` # These comments deal with incompetent syntax highlighting rules
	@echo 6. Clean out old .tar.gz files living in deps/, "\`git clean -fdx\`" seems to work	#"`
	@echo 7. Replace github release tarball with tarballs created from make light-source-dist and make full-source-dist
	@echo 8. Follow packaging instructions in DISTRIBUTING.md to create binary packages for all platforms
	@echo 9. Upload to AWS, update http://julialang.org/downloads and http://status.julialang.org/stable links
	@echo 10. Update checksums on AWS for tarball and packaged binaries
	@echo 11. Announce on mailing lists
	@echo 12. Change master to release-0.X in base/version.jl and base/version_git.sh as in 4cb1e20
	@echo

$(build_man1dir)/julia.1: $(JULIAHOME)/doc/man/julia.1 | $(build_man1dir)
	@echo Copying in usr/share/man/man1/julia.1
	@mkdir -p $(build_man1dir)
	@cp $< $@

$(build_sysconfdir)/julia/juliarc.jl: $(JULIAHOME)/etc/juliarc.jl | $(build_sysconfdir)/julia
	@echo Creating usr/etc/julia/juliarc.jl
	@cp $< $@
ifeq ($(OS), WINNT)
	@cat $(JULIAHOME)/contrib/windows/juliarc.jl >> $(build_sysconfdir)/julia/juliarc.jl
$(build_sysconfdir)/julia/juliarc.jl: $(JULIAHOME)/contrib/windows/juliarc.jl
endif

$(build_private_libdir)/%.$(SHLIB_EXT): $(build_private_libdir)/%.o
	@$(call PRINT_LINK, $(CXX) $(LDFLAGS) -shared $(fPIC) -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -o $@ $< \
		$(if $(findstring -debug.$(SHLIB_EXT),$(notdir $@)),-ljulia-debug,-ljulia) \
		$$([ $(OS) = WINNT ] && echo '' -lssp))
	@$(INSTALL_NAME_CMD)$(notdir $@) $@
	@$(DSYMUTIL) $@

CORE_SRCS := $(addprefix $(JULIAHOME)/, \
		base/boot.jl base/coreimg.jl \
		base/abstractarray.jl \
		base/array.jl \
		base/bool.jl \
		base/dict.jl \
		base/error.jl \
		base/essentials.jl \
		base/generator.jl \
		base/expr.jl \
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
		base/tuple.jl)
BASE_SRCS := $(sort $(shell find $(JULIAHOME)/base -name \*.jl) $(shell find $(BUILDROOT)/base -name \*.jl))

$(build_private_libdir)/inference.ji: $(CORE_SRCS) | $(build_private_libdir)
	@$(call PRINT_JULIA, cd $(JULIAHOME)/base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C $(JULIA_CPU_TARGET) --output-ji $(call cygpath_w,$@) \
		--startup-file=no -g0 -O0 coreimg.jl)

RELBUILDROOT := $(shell $(JULIAHOME)/contrib/relative_path.sh "$(JULIAHOME)/base" "$(BUILDROOT)/base/")
COMMA:=,
define sysimg_builder
$$(build_private_libdir)/sys$1.o: $$(build_private_libdir)/inference.ji $$(JULIAHOME)/VERSION $$(BASE_SRCS)
	@$$(call PRINT_JULIA, cd $$(JULIAHOME)/base && \
	$$(call spawn,$3) $2 -C $$(JULIA_CPU_TARGET) --output-o $$(call cygpath_w,$$@) $$(JULIA_SYSIMG_BUILD_FLAGS) \
		--startup-file=no --sysimage $$(call cygpath_w,$$<) sysimg.jl $$(RELBUILDROOT) \
		|| { echo '*** This error is usually fixed by running `make clean`. If the error persists$$(COMMA) try `make cleanall`. ***' && false; } )
.SECONDARY: $(build_private_libdir)/sys$1.o
endef
$(eval $(call sysimg_builder,,-O3,$(JULIA_EXECUTABLE_release)))
$(eval $(call sysimg_builder,-debug,-O0,$(JULIA_EXECUTABLE_debug)))

$(build_depsbindir)/stringreplace: $(JULIAHOME)/contrib/stringreplace.c | $(build_depsbindir)
	@$(call PRINT_CC, $(HOSTCC) -o $(build_depsbindir)/stringreplace $(JULIAHOME)/contrib/stringreplace.c)


# public libraries, that are installed in $(prefix)/lib
JL_LIBS := julia julia-debug

# private libraries, that are installed in $(prefix)/lib/julia
JL_PRIVATE_LIBS := ccalltest
ifeq ($(USE_GPL_LIBS), 1)
JL_PRIVATE_LIBS += suitesparse_wrapper
endif
ifeq ($(USE_SYSTEM_FFTW),0)
ifeq ($(USE_GPL_LIBS), 1)
JL_PRIVATE_LIBS += fftw3 fftw3f fftw3_threads fftw3f_threads
endif
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
ifeq ($(USE_SYSTEM_MBEDTLS),0)
JL_PRIVATE_LIBS += mbedtls mbedcrypto mbedx509
endif
ifeq ($(USE_SYSTEM_LIBSSH2),0)
JL_PRIVATE_LIBS += ssh2
endif
ifeq ($(USE_SYSTEM_CURL),0)
JL_PRIVATE_LIBS += curl
endif
ifeq ($(USE_SYSTEM_LIBGIT2),0)
JL_PRIVATE_LIBS += git2
endif
ifeq ($(USE_SYSTEM_ARPACK),0)
JL_PRIVATE_LIBS += arpack
endif
ifeq ($(USE_SYSTEM_SUITESPARSE),0)
ifeq ($(USE_GPL_LIBS), 1)
JL_PRIVATE_LIBS += amd camd ccolamd cholmod colamd umfpack spqr suitesparseconfig
endif
endif
ifeq ($(USE_SYSTEM_LLVM),0)
ifeq ($(USE_LLVM_SHLIB),1)
JL_PRIVATE_LIBS += LLVM
endif
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
julia-deps: | $$(build_bindir)/lib$(1).dll $$(build_depsbindir)/lib$(1).dll
$$(build_bindir)/lib$(1).dll: | $$(build_bindir)
	cp $$(call pathsearch,lib$(1).dll,$$(STD_LIB_PATH)) $$(build_bindir)
$$(build_depsbindir)/lib$(1).dll: | $$(build_depsbindir)
	cp $$(call pathsearch,lib$(1).dll,$$(STD_LIB_PATH)) $$(build_depsbindir)
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
define stringreplace
	$(build_depsbindir)/stringreplace $$(strings -t x - $1 | grep '$2' | awk '{print $$1;}') '$3' 255 "$(call cygpath_w,$1)"
endef

install: $(build_depsbindir)/stringreplace $(BUILDROOT)/doc/_build/html
	@$(MAKE) $(QUIET_MAKE) all
	@for subdir in $(bindir) $(libexecdir) $(datarootdir)/julia/site/$(VERSDIR) $(docdir) $(man1dir) $(includedir)/julia $(libdir) $(private_libdir) $(sysconfdir); do \
		mkdir -p $(DESTDIR)$$subdir; \
	done

	$(INSTALL_M) $(build_bindir)/julia* $(DESTDIR)$(bindir)/
ifeq ($(OS),WINNT)
	-$(INSTALL_M) $(build_bindir)/*.dll $(DESTDIR)$(bindir)/
	-$(INSTALL_M) $(build_libdir)/libjulia.dll.a $(DESTDIR)$(libdir)/
	-$(INSTALL_M) $(build_libdir)/libjulia-debug.dll.a $(DESTDIR)$(libdir)/
	-$(INSTALL_M) $(build_bindir)/libopenlibm.dll.a $(DESTDIR)$(libdir)/
else
	-cp -a $(build_libexecdir) $(DESTDIR)$(prefix)

	# Copy over .dSYM directories directly
ifeq ($(OS),Darwin)
	-cp -a $(build_libdir)/*.dSYM $(DESTDIR)$(libdir)
	-cp -a $(build_private_libdir)/*.dSYM $(DESTDIR)$(private_libdir)
endif

	for suffix in $(JL_LIBS) ; do \
		for lib in $(build_libdir)/lib$${suffix}*.$(SHLIB_EXT)*; do \
			if [ "$${lib##*.}" != "dSYM" ]; then \
				$(INSTALL_M) $$lib $(DESTDIR)$(libdir) ; \
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
	$(INSTALL_F) $(addprefix $(JULIAHOME)/,src/julia.h src/julia_threads.h src/support/*.h) $(DESTDIR)$(includedir)/julia
	$(INSTALL_F) $(BUILDROOT)/src/julia_version.h $(DESTDIR)$(includedir)/julia
	# Copy system image
	-$(INSTALL_F) $(build_private_libdir)/sys.ji $(DESTDIR)$(private_libdir)
	$(INSTALL_M) $(build_private_libdir)/sys.$(SHLIB_EXT) $(DESTDIR)$(private_libdir)
	$(INSTALL_M) $(build_private_libdir)/sys-debug.$(SHLIB_EXT) $(DESTDIR)$(private_libdir)
	# Copy in system image build script
	$(INSTALL_M) $(JULIAHOME)/contrib/build_sysimg.jl $(DESTDIR)$(datarootdir)/julia/
	# Copy in standalone julia-config script
	$(INSTALL_M) $(JULIAHOME)/contrib/julia-config.jl $(DESTDIR)$(datarootdir)/julia/
	# Copy in all .jl sources as well
	cp -R -L $(build_datarootdir)/julia $(DESTDIR)$(datarootdir)/
	# Copy documentation
	cp -R -L $(build_docdir)/* $(DESTDIR)$(docdir)/
	cp -R -L $(BUILDROOT)/doc/_build/html $(DESTDIR)$(docdir)/
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
	$(INSTALL_F) $(JULIAHOME)/contrib/julia.svg $(DESTDIR)$(datarootdir)/icons/hicolor/scalable/apps/
	-touch -c $(DESTDIR)$(datarootdir)/icons/hicolor/
	-gtk-update-icon-cache $(DESTDIR)$(datarootdir)/icons/hicolor/
	mkdir -p $(DESTDIR)$(datarootdir)/applications/
	$(INSTALL_F) $(JULIAHOME)/contrib/julia.desktop $(DESTDIR)$(datarootdir)/applications/
	# Install appdata file
	mkdir -p $(DESTDIR)$(datarootdir)/appdata/
	$(INSTALL_F) $(JULIAHOME)/contrib/julia.appdata.xml $(DESTDIR)$(datarootdir)/appdata/

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
	$(call stringreplace,$(DESTDIR)$(libdir)/libjulia.$(SHLIB_EXT),sys.$(SHLIB_EXT)$$,$(private_libdir_rel)/sys.$(SHLIB_EXT))
	$(call stringreplace,$(DESTDIR)$(libdir)/libjulia-debug.$(SHLIB_EXT),sys-debug.$(SHLIB_EXT)$$,$(private_libdir_rel)/sys-debug.$(SHLIB_EXT))
endif

	mkdir -p $(DESTDIR)$(sysconfdir)
	cp -R $(build_sysconfdir)/julia $(DESTDIR)$(sysconfdir)/

distclean dist-clean:
	-rm -fr $(BUILDROOT)/julia-*.tar.gz $(BUILDROOT)/julia*.exe $(BUILDROOT)/julia-*.7z $(BUILDROOT)/julia-$(JULIA_COMMIT)

dist:
	@echo \'dist\' target is deprecated: use \'binary-dist\' instead.

binary-dist: distclean
ifeq ($(USE_SYSTEM_BLAS),0)
ifeq ($(ISX86),1)
ifneq ($(OPENBLAS_DYNAMIC_ARCH),1)
	@echo OpenBLAS must be rebuilt with OPENBLAS_DYNAMIC_ARCH=1 to use binary-dist target
	@false
endif
endif
endif
ifneq ($(prefix),$(abspath julia-$(JULIA_COMMIT)))
	$(error prefix must not be set for make binary-dist)
endif
ifneq ($(DESTDIR),)
	$(error DESTDIR must not be set for make binary-dist)
endif
	@$(MAKE) -C $(BUILDROOT) -f $(JULIAHOME)/Makefile install
	cp $(JULIAHOME)/LICENSE.md $(BUILDROOT)/julia-$(JULIA_COMMIT)
ifneq ($(OS), WINNT)
	-$(CUSTOM_LD_LIBRARY_PATH) PATH=$(PATH):$(build_depsbindir) $(JULIAHOME)/contrib/fixup-libgfortran.sh $(DESTDIR)$(private_libdir)
endif
ifeq ($(OS), Linux)
	-$(JULIAHOME)/contrib/fixup-libstdc++.sh $(DESTDIR)$(libdir) $(DESTDIR)$(private_libdir)
	# We need to bundle ca certs on linux now that we're using libgit2 with ssl
ifeq ($(shell [ -e $(shell openssl version -d | cut -d '"' -f 2)/cert.pem ] && echo exists),exists)
	-cp $(shell openssl version -d | cut -d '"' -f 2)/cert.pem $(DESTDIR)$(datarootdir)/julia/
endif
endif
	# Copy in juliarc.jl files per-platform for binary distributions as well
	# Note that we don't install to sysconfdir: we always install to $(DESTDIR)$(prefix)/etc.
	# If you want to make a distribution with a hardcoded path, you take care of installation
ifeq ($(OS), Darwin)
	-cat $(JULIAHOME)/contrib/mac/juliarc.jl >> $(DESTDIR)$(prefix)/etc/julia/juliarc.jl
endif

ifeq ($(OS), WINNT)
	[ ! -d $(JULIAHOME)/dist-extras ] || ( cd $(JULIAHOME)/dist-extras && \
		cp 7z.exe 7z.dll libexpat-1.dll zlib1.dll libgfortran-3.dll libquadmath-0.dll libstdc++-6.dll libgcc_s_s*-1.dll libssp-0.dll $(BUILDROOT)/julia-$(JULIA_COMMIT)/bin )
ifeq ($(USE_GPL_LIBS), 1)
	[ ! -d $(JULIAHOME)/dist-extras ] || ( cd $(JULIAHOME)/dist-extras && \
		cp busybox.exe $(BUILDROOT)/julia-$(JULIA_COMMIT)/bin )
endif
	cd $(BUILDROOT)/julia-$(JULIA_COMMIT)/bin && rm -f llvm* llc.exe lli.exe opt.exe LTO.dll bugpoint.exe macho-dump.exe

	# create file listing for uninstall. note: must have Windows path separators and line endings.
	cd $(BUILDROOT)/julia-$(JULIA_COMMIT) && find * | sed -e 's/\//\\/g' -e 's/$$/\r/g' > etc/uninstall.log

	# build nsis package
	cd $(BUILDROOT) && $(call spawn,$(JULIAHOME)/dist-extras/nsis/makensis.exe) -NOCD -DVersion=$(JULIA_VERSION) -DArch=$(ARCH) -DCommit=$(JULIA_COMMIT) -DMUI_ICON="$(call cygpath_w,$(JULIAHOME)/contrib/windows/julia.ico)" $(call cygpath_w,$(JULIAHOME)/contrib/windows/build-installer.nsi)

	# compress nsis installer and combine with 7zip self-extracting header
	cd $(BUILDROOT) && $(JULIAHOME)/dist-extras/7z a -mx9 "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" julia-installer.exe
	cd $(BUILDROOT) && cat $(JULIAHOME)/contrib/windows/7zS.sfx $(JULIAHOME)/contrib/windows/7zSFX-config.txt "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" > "julia-${JULIA_VERSION}-${ARCH}.exe"
	-rm -f $(BUILDROOT)/julia-installer.exe
else
	cd $(BUILDROOT) && $(TAR) zcvf $(JULIA_BINARYDIST_TARNAME).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr $(BUILDROOT)/julia-$(JULIA_COMMIT)

# this target does not accept BUILDROOT
light-source-dist.tmp: $(JULIAHOME)/doc/_build/html
	# Save git information
	-@$(MAKE) -C $(JULIAHOME)/base version_git.jl.phony

	# Create file light-source-dist.tmp to hold all the filenames that go into the tarball
	echo "base/version_git.jl" > light-source-dist.tmp
	git ls-files | sed -e '/\.git/d' -e '/\.travis/d' >> light-source-dist.tmp
	find doc/_build/html >> light-source-dist.tmp

# Make tarball with only Julia code
# this target does not accept BUILDROOT
light-source-dist: light-source-dist.tmp
	# Prefix everything with the current directory name (usually "julia"), then create tarball
	DIRNAME=$$(basename $$(pwd)); \
	sed -e "s_.*_$$DIRNAME/&_" light-source-dist.tmp > light-source-dist.tmp1; \
	cd ../ && tar -cz -T $$DIRNAME/light-source-dist.tmp1 --no-recursion -f $$DIRNAME/julia-$(JULIA_VERSION)_$(JULIA_COMMIT).tar.gz

source-dist:
	@echo \'source-dist\' target is deprecated: use \'full-source-dist\' instead.

# Make tarball with Julia code plus all dependencies
# this target does not accept BUILDROOT
full-source-dist: light-source-dist.tmp
	# Get all the dependencies downloaded
	@$(MAKE) -C deps getall NO_GIT=1

	# Create file full-source-dist.tmp to hold all the filenames that go into the tarball
	cp light-source-dist.tmp full-source-dist.tmp
	-ls deps/srccache/*.tar.gz deps/srccache/*.tar.bz2 deps/srccache/*.tar.xz deps/srccache/*.tgz deps/srccache/*.zip >> full-source-dist.tmp

	# Prefix everything with the current directory name (usually "julia"), then create tarball
	DIRNAME=$$(basename $$(pwd)); \
	sed -e "s_.*_$$DIRNAME/&_" full-source-dist.tmp > full-source-dist.tmp1; \
	cd ../ && tar -cz -T $$DIRNAME/full-source-dist.tmp1 --no-recursion -f $$DIRNAME/julia-$(JULIA_VERSION)_$(JULIA_COMMIT)-full.tar.gz

clean: | $(CLEAN_TARGETS)
	@-$(MAKE) -C $(BUILDROOT)/base clean
	@-$(MAKE) -C $(BUILDROOT)/doc clean
	@-$(MAKE) -C $(BUILDROOT)/src clean
	@-$(MAKE) -C $(BUILDROOT)/ui clean
	@-$(MAKE) -C $(BUILDROOT)/test clean
	-rm -f $(BUILDROOT)/julia
	-rm -f $(BUILDROOT)/*.tar.gz
	-rm -f $(build_depsbindir)/stringreplace \
	   $(BUILDROOT)/light-source-dist.tmp $(BUILDROOT)/light-source-dist.tmp1 \
	   $(BUILDROOT)/full-source-dist.tmp $(BUILDROOT)/full-source-dist.tmp1
	-rm -fr $(build_private_libdir)
# Teporarily add this line to the Makefile to remove extras
	-rm -fr $(build_datarootdir)/julia/extras
	-rm -f $(build_prefix)/.examples

cleanall: clean
	@-$(MAKE) -C $(BUILDROOT)/src clean-flisp clean-support
	@-$(MAKE) -C $(BUILDROOT)/deps clean-libuv
	-rm -fr $(build_prefix) $(build_staging)

distcleanall: cleanall
	@-$(MAKE) -C $(BUILDROOT)/deps distcleanall
	@-$(MAKE) -C $(BUILDROOT)/doc cleanall

.PHONY: default debug release check-whitespace release-candidate \
	julia-debug julia-release julia-deps \
	julia-ui-release julia-ui-debug julia-src-release julia-src-debug \
	julia-symlink julia-base julia-inference julia-sysimg-release julia-sysimg-debug \
	test testall testall1 test clean distcleanall cleanall clean-* \
	run-julia run-julia-debug run-julia-release run \
	install binary-dist light-source-dist.tmp light-source-dist \
	dist full-source-dist source-dist

test: check-whitespace $(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test default JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

testall: check-whitespace $(JULIA_BUILD_MODE)
	cp $(build_private_libdir)/sys$(JULIA_LIBSUFFIX).$(SHLIB_EXT) $(BUILDROOT)/local.$(SHLIB_EXT) && $(JULIA_EXECUTABLE) -J $(call cygpath_w,$(BUILDROOT)/local.$(SHLIB_EXT)) -e 'true' && rm $(BUILDROOT)/local.$(SHLIB_EXT)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test all JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

testall1: check-whitespace $(JULIA_BUILD_MODE)
	@env JULIA_CPU_CORES=1 $(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test all JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

test-%: check-whitespace $(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test $* JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

perf: release
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test/perf JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

perf-%: release
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test/perf $* JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

# download target for some hardcoded windows dependencies
.PHONY: win-extras wine_path
win-extras:
	[ -d $(JULIAHOME)/dist-extras ] || mkdir $(JULIAHOME)/dist-extras
ifneq ($(BUILD_OS),WINNT)
ifeq (,$(findstring CYGWIN,$(BUILD_OS)))
	cp /usr/lib/p7zip/7z /usr/lib/p7zip/7z.so $(JULIAHOME)/dist-extras
endif
endif
ifneq (,$(filter $(ARCH), i386 i486 i586 i686))
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z920.exe && \
	7z x -y 7z920.exe 7z.exe 7z.dll && \
	../contrib/windows/winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win32/openSUSE_13.2 \
		"mingw32-libexpat1 mingw32-zlib1" && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw32-libgfortran3-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw32-libquadmath0-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw32-libstdc++6-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw32-libgcc_s_sjlj1-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw32-libssp0-5.3.0-1.1.noarch.rpm && \
	for i in *.rpm; do 7z x -y $$i; done && \
	for i in *.cpio; do 7z x -y $$i; done && \
	cp usr/i686-w64-mingw32/sys-root/mingw/bin/*.dll .
else ifeq ($(ARCH),x86_64)
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) 7z920-x64.msi http://downloads.sourceforge.net/sevenzip/7z920-x64.msi && \
	7z x -y 7z920-x64.msi _7z.exe _7z.dll && \
	mv _7z.dll 7z.dll && \
	mv _7z.exe 7z.exe && \
	../contrib/windows/winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win64/openSUSE_13.2 \
		"mingw64-libexpat1 mingw64-zlib1" && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw64-libgfortran3-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw64-libquadmath0-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw64-libstdc++6-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw64-libgcc_s_seh1-5.3.0-1.1.noarch.rpm && \
	$(JLDOWNLOAD) https://julialang.s3.amazonaws.com/bin/winnt/extras/mingw64-libssp0-5.3.0-1.1.noarch.rpm && \
	for i in *.rpm; do 7z x -y $$i; done && \
	for i in *.cpio; do 7z x -y $$i; done && \
	cp usr/x86_64-w64-mingw32/sys-root/mingw/bin/*.dll .
else
	$(error no win-extras target for ARCH=$(ARCH))
endif
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z920_extra.7z && \
	$(JLDOWNLOAD) https://unsis.googlecode.com/files/nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x 7z.exe && \
	chmod a+x 7z.dll && \
	$(call spawn,./7z.exe) x -y -onsis nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x ./nsis/makensis.exe
ifeq ($(USE_GPL_LIBS), 1)
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) busybox.exe http://frippery.org/files/busybox/busybox-w32-FRP-483-g31277ab.exe && \
	chmod a+x busybox.exe
endif

# various statistics about the build that may interest the user
ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_SIZE := llvm-size$(EXE)
else
LLVM_SIZE := $(build_depsbindir)/llvm-size$(EXE)
endif
build-stats:
	@echo $(JULCOLOR)' ==> ./julia binary sizes'$(ENDCOLOR)
	$(call spawn,$(LLVM_SIZE) -A $(build_private_libdir)/sys.$(SHLIB_EXT) $(build_shlibdir)/libjulia.$(SHLIB_EXT) $(build_bindir)/julia$(EXE))
	@echo $(JULCOLOR)' ==> ./julia launch speedtest'$(ENDCOLOR)
	@time $(call spawn,$(build_bindir)/julia$(EXE) -e '')
	@time $(call spawn,$(build_bindir)/julia$(EXE) -e '')
	@time $(call spawn,$(build_bindir)/julia$(EXE) -e '')
