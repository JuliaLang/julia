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

DIRS = $(BUILD)/bin $(BUILD)/lib $(BUILD)/$(JL_PRIVATE_LIBDIR) $(BUILD)/share/julia

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,extras base test doc examples,$(eval $(call symlink_target,$(link),$(BUILD)/share/julia)))

QUIET_MAKE =
ifeq ($(USE_QUIET), 1)
QUIET_MAKE = -s
endif

debug release: | $(DIRS) $(BUILD)/share/julia/extras $(BUILD)/share/julia/base $(BUILD)/share/julia/test $(BUILD)/share/julia/doc $(BUILD)/share/julia/examples
	@$(MAKE) $(QUIET_MAKE) julia-$@
	@export JL_PRIVATE_LIBDIR=$(JL_PRIVATE_LIBDIR) && \
	$(MAKE) $(QUIET_MAKE) LD_LIBRARY_PATH=$(BUILD)/lib:$(LD_LIBRARY_PATH) JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$@)" $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji

julia-debug julia-release:
	@-git submodule init --quiet
	@-git submodule update
	@$(MAKE) $(QUIET_MAKE) -C deps
	@$(MAKE) $(QUIET_MAKE) -C src lib$@
	@$(MAKE) $(QUIET_MAKE) -C base
	@$(MAKE) $(QUIET_MAKE) -C ui $@
ifneq ($(OS),WINNT)
	@ln -sf $(BUILD)/bin/$@-$(DEFAULT_REPL) julia
endif

$(BUILD)/share/julia/helpdb.jl: doc/helpdb.jl | $(BUILD)/share/julia
	@cp $< $@

COMMIT:
	@#this is a .PHONY target so that it will always run
	echo `git rev-parse --short HEAD`-$(OS)-$(ARCH) \(`date +"%Y-%m-%d %H:%M:%S"`\) > COMMIT

# use sys.ji if it exists, otherwise run two stages
$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji: VERSION base/*.jl base/pkg/*.jl base/linalg/*.jl $(BUILD)/share/julia/helpdb.jl
	@$(MAKE) $(QUIET_MAKE) COMMIT
	$(QUIET_JULIA) cd base && \
	(test -f $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji || $(call spawn,$(JULIA_EXECUTABLE)) -bf sysimg.jl) && $(call spawn,$(JULIA_EXECUTABLE)) -f sysimg.jl || echo "*** This error is usually fixed by running 'make clean'. If the error persists, try 'make cleanall'. ***"

run-julia-debug run-julia-release: run-julia-%:
	$(MAKE) $(QUIET_MAKE) run-julia JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$*)"
run-julia:
	@$(call spawn,$(JULIA_EXECUTABLE))
run:
	@$(call spawn,$(cmd))

# public libraries, that are installed in $(PREFIX)/lib
JL_LIBS = julia-release julia-debug

# private libraries, that are installed in $(PREFIX)/lib/julia
JL_PRIVATE_LIBS = amd arpack camd ccolamd cholmod colamd \
                  fftw3 fftw3f fftw3_threads fftw3f_threads \
                  gmp grisu openlibm openlibm-extras pcre \
                  random Rmath spqr suitesparse_wrapper \
                  umfpack z openblas mpfr

PREFIX ?= julia-$(JULIA_COMMIT)
install:
	@$(MAKE) $(QUIET_MAKE) debug
	@$(MAKE) $(QUIET_MAKE) release
	@for subdir in "bin" "libexec" $(JL_LIBDIR) $(JL_PRIVATE_LIBDIR) "share/julia" "include/julia" "share/julia/site/"$(VERSDIR) ; do \
		mkdir -p $(PREFIX)/$$subdir ; \
	done
	cp -a $(BUILD)/bin $(PREFIX)
ifneq ($(OS),WINNT)
	cd $(PREFIX)/bin && ln -sf julia-release-$(DEFAULT_REPL) julia
endif
	-for suffix in $(JL_LIBS) ; do \
		cp -a $(BUILD)/$(JL_LIBDIR)/lib$${suffix}*.$(SHLIB_EXT)* $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
	-for suffix in $(JL_PRIVATE_LIBS) ; do \
		cp -a $(BUILD)/lib/lib$${suffix}*.$(SHLIB_EXT)* $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
ifeq ($(USE_SYSTEM_LIBUV),0)
	cp -a $(BUILD)/lib/libuv.a $(PREFIX)/$(JL_PRIVATE_LIBDIR)
	cp -a $(BUILD)/include/uv* $(PREFIX)/include/julia
endif
	cp -a src/julia.h $(PREFIX)/include/julia
	# Copy system image
	cp $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji $(PREFIX)/$(JL_PRIVATE_LIBDIR)
	# Copy in all .jl sources as well
	cp -R -L $(BUILD)/share/julia $(PREFIX)/share/
ifeq ($(OS), WINNT)
	cp $(JULIAHOME)/contrib/windows/*.bat $(PREFIX)
endif
	cp $(JULIAHOME)/VERSION $(PREFIX)/share/julia/VERSION
	$(MAKE) $(QUIET_MAKE) COMMIT
	cp $(JULIAHOME)/COMMIT $(PREFIX)/share/julia/COMMIT

dist:
	rm -fr julia-*.tar.gz julia-$(JULIA_COMMIT)
ifneq ($(OPENBLAS_DYNAMIC_ARCH),1)
	@echo OpenBLAS must be rebuilt with OPENBLAS_DYNAMIC_ARCH=1 to use dist target
	@false
endif
	@$(MAKE) install
ifeq ($(OS), Darwin)
	-./contrib/mac/fixup-libgfortran.sh $(PREFIX)/$(JL_PRIVATE_LIBDIR)
endif
ifeq ($(OS), WINNT)
	-[ -e dist-extras/7za.exe ] && cp dist-extras/7za.exe $(PREFIX)/bin/7z.exe
	-[ -e dist-extras/PortableGit-1.8.1.2-preview20130201.7z ] && \
	  mkdir $(PREFIX)/Git && \
	  7z x dist-extras/PortableGit-1.8.1.2-preview20130201.7z -o"$(PREFIX)/Git"
ifeq ($(BUILD_OS),WINNT)
	cp $(call pathsearch,libgfortran-3.dll,$(PATH)) $(PREFIX)/$(JL_LIBDIR) ;
	cp $(call pathsearch,libquadmath-0.dll,$(PATH)) $(PREFIX)/$(JL_LIBDIR) ;
	cp $(call pathsearch,libgcc_s_sjlj-1.dll,$(PATH)) $(PREFIX)/$(JL_LIBDIR) ;
	cp $(call pathsearch,libstdc++-6.dll,$(PATH)) $(PREFIX)/$(JL_LIBDIR) ;
	#cp $(call pathsearch,libssp-0.dll,$(PATH)) $(PREFIX)/$(JL_LIBDIR) ;
else
	cp $(call wine_pathsearch,libgfortran-3.dll,$(WINE_PATH)) $(PREFIX)/$(JL_LIBDIR) ;
	cp $(call wine_pathsearch,libquadmath-0.dll,$(WINE_PATH)) $(PREFIX)/$(JL_LIBDIR) ;
ifeq ($(ARCH),i686)
	cp $(call wine_pathsearch,libgcc_s_sjlj-1.dll,$(WINE_PATH)) $(PREFIX)/$(JL_LIBDIR) ;
else
	cp $(call wine_pathsearch,libgcc_s_seh-1.dll,$(WINE_PATH)) $(PREFIX)/$(JL_LIBDIR) ;
endif
	cp $(call wine_pathsearch,libstdc++-6.dll,$(WINE_PATH)) $(PREFIX)/$(JL_LIBDIR) ;
	cp $(call wine_pathsearch,libssp-0.dll,$(WINE_PATH)) $(PREFIX)/$(JL_LIBDIR) ;
endif
	zip -r -9 julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).zip julia-$(JULIA_COMMIT)
else
	tar zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr julia-$(JULIA_COMMIT)

clean: | $(CLEAN_TARGETS)
	@$(MAKE) -C base clean
	@$(MAKE) -C src clean
	@$(MAKE) -C ui clean
	@for buildtype in "release" "debug" ; do \
		for repltype in "basic" "readline"; do \
			rm -f julia-$${buildtype}-$${repltype}; \
		done \
	done
	@rm -f julia
	@rm -f *~ *# *.tar.gz
	@rm -fr $(BUILD)/$(JL_PRIVATE_LIBDIR)

cleanall: clean
	@$(MAKE) -C src clean-flisp clean-support
	@rm -fr $(BUILD)/$(JL_LIBDIR)
	@$(MAKE) -C deps clean-uv

distclean: cleanall
	@$(MAKE) -C deps distclean
	@$(MAKE) -C doc cleanall
	rm -fr usr

.PHONY: default debug release julia-debug julia-release \
	test testall test-* clean distclean cleanall \
	run-julia run-julia-debug run-julia-release run \
   	COMMIT install dist

test: release
	@$(MAKE) $(QUIET_MAKE) -C test default

testall: release
	@$(MAKE) $(QUIET_MAKE) -C test all

test-%: release
	@$(MAKE) $(QUIET_MAKE) -C test $*

# download target for some hardcoded windows dependencies
.PHONY: win-extras, wine_path
win-extras:
	cd dist-extras && \
	wget -O 7za920.zip http://downloads.sourceforge.net/sevenzip/7za920.zip && \
	wget -O PortableGit-1.8.1.2-preview20130201.7z https://msysgit.googlecode.com/files/PortableGit-1.8.1.2-preview20130201.7z
wine_path:
	$(info $(WINE_PATH))
	@echo "wine cmd /c \"set \$$PATH=...\";%PATH% && program"
	@echo
