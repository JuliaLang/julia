JULIAHOME = $(abspath .)
include $(JULIAHOME)/Make.inc

all: default
default: release

DIRS = $(BUILD)/bin $(BUILD)/lib $(BUILD)/$(JL_PRIVATE_LIBDIR) $(BUILD)/share/julia

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,extras base test doc examples ui,$(eval $(call symlink_target,$(link),$(BUILD)/share/julia)))

QUIET_MAKE =
ifeq ($(USE_QUIET), 1)
QUIET_MAKE = -s
endif

debug release: | $(DIRS) $(BUILD)/share/julia/extras $(BUILD)/share/julia/base $(BUILD)/share/julia/test $(BUILD)/share/julia/doc $(BUILD)/share/julia/examples $(BUILD)/share/julia/ui
	@$(MAKE) $(QUIET_MAKE) julia-$@
	@export JL_PRIVATE_LIBDIR=$(JL_PRIVATE_LIBDIR) && \
	$(MAKE) $(QUIET_MAKE) LD_LIBRARY_PATH=$(BUILD)/lib:$(LD_LIBRARY_PATH) JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$@)" $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji

julia-debug julia-release:
	@-git submodule init --quiet
	@-git submodule update
	@$(MAKE) $(QUIET_MAKE) -C deps
	@$(MAKE) $(QUIET_MAKE) -C src lib$@
	@$(MAKE) $(QUIET_MAKE) -C base
	@$(MAKE) $(QUIET_MAKE) -C extras
	@$(MAKE) $(QUIET_MAKE) -C ui $@
	@ln -sf $(BUILD)/bin/$@-$(DEFAULT_REPL) julia

$(BUILD)/share/julia/helpdb.jl: doc/helpdb.jl | $(BUILD)/share/julia
	@cp $< $@

# use sys.ji if it exists, otherwise run two stages
$(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji: VERSION base/*.jl base/pkg/*.jl $(BUILD)/share/julia/helpdb.jl
	@#echo `git rev-parse --short HEAD`-$(OS)-$(ARCH) \(`date +"%Y-%m-%d %H:%M:%S"`\) > COMMIT
	$(QUIET_JULIA) cd base && \
	(test -f $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji || $(JULIA_EXECUTABLE) -bf sysimg.jl) && $(JULIA_EXECUTABLE) -f sysimg.jl || echo "Note: this error is usually fixed by running 'make clean'. If the error persists, 'make cleanall' may help."

run-julia-debug run-julia-release: run-julia-%:
	$(MAKE) $(QUIET_MAKE) run-julia JULIA_EXECUTABLE="$(JULIA_EXECUTABLE_$*)"
run-julia:
	#winedbg --gdb
	$(JULIA_EXECUTABLE)

# public libraries, that are installed in $(PREFIX)/lib
JL_LIBS = julia-release julia-debug

# private libraries, that are installed in $(PREFIX)/lib/julia
JL_PRIVATE_LIBS = amd arpack cholmod colamd fftw3 fftw3f fftw3_threads \
                  fftw3f_threads gmp grisu \
                  openlibm openlibm-extras pcre \
		  random Rmath spqr suitesparse_wrapper \
		  umfpack z openblas

PREFIX ?= julia-$(JULIA_COMMIT)
install: release webrepl
	@for subdir in "sbin" "bin" "etc" "libexec" $(JL_LIBDIR) $(JL_PRIVATE_LIBDIR) "share/julia" ; do \
		mkdir -p $(PREFIX)/$$subdir ; \
	done
ifeq ($(OS), Darwin)
	$(MAKE) -C deps install-git
	-cp -a $(BUILD)/libexec $(PREFIX)
	-cp -a $(BUILD)/share $(PREFIX)
endif
	cp -a $(BUILD)/bin $(PREFIX)
	cd $(PREFIX)/bin && ln -sf julia-release-$(DEFAULT_REPL) julia
	-for suffix in $(JL_LIBS) ; do \
		cp -a $(BUILD)/$(JL_LIBDIR)/lib$${suffix}*.$(SHLIB_EXT)* $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
	-for suffix in $(JL_PRIVATE_LIBS) ; do \
		cp -a $(BUILD)/lib/lib$${suffix}*.$(SHLIB_EXT)* $(PREFIX)/$(JL_PRIVATE_LIBDIR) ; \
	done
	# Copy system image
	cp $(BUILD)/$(JL_PRIVATE_LIBDIR)/sys.ji $(PREFIX)/$(JL_PRIVATE_LIBDIR)
	# Copy in all .jl sources as well
	cp -R -L $(BUILD)/share/julia $(PREFIX)/share/
	-cp $(BUILD)/etc/nginx.conf $(PREFIX)/etc/
ifeq ($(OS), WINNT)
	-cp $(JULIAHOME)/contrib/windows/* $(PREFIX)
endif
	cp $(JULIAHOME)/VERSION $(PREFIX)/share/julia/VERSION
	echo `git rev-parse --short HEAD`-$(OS)-$(ARCH) \(`date +"%Y-%m-%d %H:%M:%S"`\) > $(PREFIX)/share/julia/COMMIT

dist: 
	rm -fr julia-*.tar.gz julia-$(JULIA_COMMIT)
#	-$(MAKE) -C deps clean-openblas
	$(MAKE) install OPENBLAS_DYNAMIC_ARCH=1
ifeq ($(OS), Darwin)
	-./contrib/fixup-libgfortran.sh $(PREFIX)/$(JL_PRIVATE_LIBDIR)
endif
ifeq ($(OS), WINNT)
	cp -R $(BUILD)/sbin $(PREFIX)
	-[ -e dist-extras/7za.exe ] && cp dist-extras/7za.exe $(PREFIX)/bin/7z.exe
	-[ -e dist-extras/PortableGit-1.8.0-preview20121022.7z ] && \
	  mkdir $(PREFIX)/Git && \
	  7z x dist-extras/PortableGit-1.8.0-preview20121022.7z -o"$(PREFIX)/Git"
ifeq ($(shell uname),MINGW32_NT-6.1)
	for dllname in "libgfortran-3" "libquadmath-0" "libgcc_s_dw2-1" "libstdc++-6,pthreadgc2" ; do \
		cp /mingw/bin/$${dllname}.dll $(PREFIX)/$(JL_LIBDIR) ; \
	done
else
	for dllname in "libgfortran-3" "libquadmath-0" "libgcc_s_sjlj-1" "libstdc++-6" ; do \
		cp /usr/lib/gcc/i686-w64-mingw32/4.6/$${dllname}.dll $(PREFIX)/$(JL_LIBDIR) ; \
	done
endif
	zip -r -9 julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).zip julia-$(JULIA_COMMIT)
else
	tar zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr julia-$(JULIA_COMMIT)

clean: | $(CLEAN_TARGETS)
	@$(MAKE) -C base clean
	@$(MAKE) -C extras clean
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
#	@$(MAKE) -C deps clean-uv

.PHONY: default debug release julia-debug julia-release \
	test testall test-* clean cleanall webrepl \
	run-julia run-julia-debug run-julia-release

test: release
	@$(MAKE) $(QUIET_MAKE) -C test default

testall: release
	@$(MAKE) $(QUIET_MAKE) -C test all

test-%: release
	@$(MAKE) $(QUIET_MAKE) -C test $*

webrepl: all
ifeq ($(USE_SYSTEM_NGINX), 0)
	@$(MAKE) $(QUIET_MAKE) -C deps install-nginx
endif
	@$(MAKE) -C ui/webserver julia-release

# download target for some hardcoded windows dependencies
.PHONY: win-extras
win-extras:
	cd dist-extras && \
	wget http://downloads.sourceforge.net/sevenzip/7za920.zip && \
	wget https://msysgit.googlecode.com/files/PortableGit-1.8.0-preview20121022.7z

