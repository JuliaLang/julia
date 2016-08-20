## high-level setup ##
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
ifeq ($(abspath .),$(abspath $(SRCDIR)))
BUILDDIR := build
else
BUILDDIR := .
endif
include $(SRCDIR)/Versions.make
include $(JULIAHOME)/Make.inc
include $(SRCDIR)/llvm-ver.make

# Special comments:
#
# all targets in here should follow the same structure,
# and provide a get-a, configure-a, compile-a, check-a, and install-a
# additionally all targets should be listed in the getall target for easier off-line compilation
# if you are adding a new target, it can help to copy an similar, existing target
#
# autoconf configure-driven scripts: llvm pcre arpack fftw unwind gmp mpfr patchelf libuv curl
# custom Makefile rules: openlibm dsfmt suitesparse-wrapper suitesparse lapack openblas utf8proc objconv osxunwind
# entirely custom: virtualenv
# CMake libs: libgit2 libssh2 mbedtls
#
# downloaded from git: llvm-svn, libuv, libopenlibm, utf8proc, openspecfun, libgit2, libssh2
#
# there are rules in this file with the . replaced by a %
# this is some magic Makefile trick that tells make
# that all targets with a % in them on that line will
# be rebuilt in a single invocation
#

## Some shared configuration options ##

CONFIGURE_COMMON := --prefix=$(abspath $(build_prefix)) --build=$(BUILD_MACHINE) --libdir=$(abspath $(build_libdir)) --bindir=$(abspath $(build_depsbindir)) $(CUSTOM_LD_LIBRARY_PATH)
ifneq ($(XC_HOST),)
CONFIGURE_COMMON += --host=$(XC_HOST)
endif
ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
CONFIGURE_COMMON += LDFLAGS="$(LDFLAGS) -Wl,--stack,8388608"
endif
endif
CONFIGURE_COMMON += F77="$(FC)" CC="$(CC) $(DEPS_CFLAGS)" CXX="$(CXX) $(DEPS_CXXFLAGS)"

CMAKE_CC_ARG := $(CC_ARG) $(DEPS_CFLAGS)
CMAKE_CXX_ARG := $(CXX_ARG) $(DEPS_CXXFLAGS)

CMAKE_COMMON := -DCMAKE_INSTALL_PREFIX:PATH=$(build_prefix) -DCMAKE_PREFIX_PATH=$(build_prefix) -DLIB_INSTALL_DIR=$(build_shlibdir)
ifneq ($(VERBOSE), 0)
CMAKE_COMMON += -DCMAKE_VERBOSE_MAKEFILE=ON
endif
# The call to which here is to work around https://cmake.org/Bug/view.php?id=14366
CMAKE_COMMON += -DCMAKE_C_COMPILER="$$(which $(CC_BASE))"
ifneq ($(strip $(CMAKE_CC_ARG)),)
CMAKE_COMMON += -DCMAKE_C_COMPILER_ARG1="$(CMAKE_CC_ARG)"
endif
CMAKE_COMMON += -DCMAKE_CXX_COMPILER="$(CXX_BASE)"
ifneq ($(strip $(CMAKE_CXX_ARG)),)
CMAKE_COMMON += -DCMAKE_CXX_COMPILER_ARG1="$(CMAKE_CXX_ARG)"
endif

ifeq ($(OS),WINNT)
CMAKE_COMMON += -DCMAKE_SYSTEM_NAME=Windows
ifneq ($(BUILD_OS),WINNT)
CMAKE_COMMON += -DCMAKE_RC_COMPILER="$$(which $(CROSS_COMPILE)windres)"
endif
endif

# For now this is LLVM specific, but I expect it won't be in the future
ifeq ($(LLVM_USE_CMAKE),1)
ifeq ($(CMAKE_GENERATOR),Ninja)
CMAKE_GENERATOR_COMMAND := -G Ninja
else ifeq ($(CMAKE_GENERATOR),make)
CMAKE_GENERATOR_COMMAND := -G "Unix Makefiles"
else
$(error Unknown CMake generator '$(CMAKE_GENERATOR)'. Options are 'Ninja' and 'make')
endif
endif

# If the top-level Makefile is called with environment variables,
# they will override the values passed above to ./configure
MAKE_COMMON := DESTDIR="" prefix=$(build_prefix) bindir=$(build_depsbindir) libdir=$(build_libdir) shlibdir=$(build_shlibdir) libexecdir=$(build_libexecdir) datarootdir=$(build_datarootdir) includedir=$(build_includedir) sysconfdir=$(build_sysconfdir) O=


## Overall configuration of which rules exist and should be run by default ##

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

ifeq ($(USE_GPL_LIBS), 1)
DEP_LIBS := suitesparse-wrapper
else
DEP_LIBS :=
endif

ifeq ($(USE_SYSTEM_LIBUV), 0)
DEP_LIBS += libuv
endif

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
DEP_LIBS += unwind
else ifeq ($(OS), FreeBSD)
DEP_LIBS += unwind
else ifeq ($(OS), Darwin)
DEP_LIBS += osxunwind
endif
endif

ifeq ($(OS), Linux)
ifeq ($(USE_SYSTEM_PATCHELF), 0)
DEP_LIBS += patchelf
PATCHELF:=$(build_depsbindir)/patchelf
else
PATCHELF:=patchelf
endif
endif
PATCHELF_BIN := $(CUSTOM_LD_LIBRARY_PATH) $(PATCHELF)

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_OPENLIBM), 0)
ifeq ($(USE_SYSTEM_LIBM), 0)
DEP_LIBS += openlibm
endif
endif

ifeq ($(USE_SYSTEM_OPENSPECFUN), 0)
DEP_LIBS += openspecfun
endif

ifeq ($(USE_SYSTEM_DSFMT), 0)
DEP_LIBS += dsfmt
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
DEP_LIBS += llvm
endif

ifeq ($(USE_SYSTEM_PCRE), 0)
DEP_LIBS += pcre
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
DEP_LIBS += openblas
ifeq ($(USE_BLAS64), 1)
ifeq ($(OS), Darwin)
DEP_LIBS += objconv
endif
endif
endif

ifeq ($(USE_GPL_LIBS), 1)
ifeq ($(USE_SYSTEM_FFTW), 0)
DEP_LIBS += fftw
endif
endif

ifeq ($(USE_SYSTEM_GMP), 0)
DEP_LIBS += gmp
endif

ifeq ($(USE_SYSTEM_LIBGIT2), 0)
ifeq ($(USE_SYSTEM_MBEDTLS), 0)
DEP_LIBS += mbedtls
endif

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
DEP_LIBS += libssh2
endif

ifneq ($(OS), WINNT)
ifeq ($(USE_SYSTEM_CURL), 0)
DEP_LIBS += curl
endif
endif

DEP_LIBS += libgit2
endif # USE_SYSTEM_LIBGIT2

ifeq ($(USE_SYSTEM_MPFR), 0)
DEP_LIBS += mpfr
endif

ifeq ($(USE_SYSTEM_ARPACK), 0)
DEP_LIBS += arpack
endif

ifeq ($(USE_GPL_LIBS), 1)
ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
DEP_LIBS += suitesparse
endif
endif

ifeq ($(USE_SYSTEM_UTF8PROC), 0)
DEP_LIBS += utf8proc
endif

# Only compile standalone LAPACK if we are not using OpenBLAS.
# OpenBLAS otherwise compiles LAPACK as part of its build.
# This is useful where one wants to use the vendor BLAS, but
# build LAPACK as the vendor LAPACK may be too old (eg. Apple vecLib)
ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(USE_SYSTEM_LAPACK), 0)
DEP_LIBS += lapack
endif
endif

#Platform specific flags

ifeq ($(OS), WINNT)
LIBTOOL_CCLD := CCLD="$(CC) -no-undefined -avoid-version"
endif

## Common build target prefixes

default: install | $(build_prefix)
get: $(addprefix get-, $(DEP_LIBS))
configure: $(addprefix configure-, $(DEP_LIBS))
compile: $(addprefix compile-, $(DEP_LIBS))
check: $(addprefix check-, $(DEP_LIBS))
install: $(addprefix install-, $(DEP_LIBS))
cleanall: $(addprefix clean-, $(DEP_LIBS))
distcleanall: $(addprefix distclean-, $(DEP_LIBS))
	rm -rf $(build_prefix)
getall: get-llvm get-libuv get-pcre get-openlibm get-openspecfun get-dsfmt get-openblas get-lapack get-fftw get-suitesparse get-arpack get-unwind get-osxunwind get-gmp get-mpfr get-patchelf get-utf8proc get-virtualenv get-objconv get-mbedtls get-libssh2 get-curl get-libgit2

## PATHS ##
# sort is used to remove potential duplicates
DIRS := $(sort $(build_bindir) $(build_depsbindir) $(build_libdir) $(build_includedir) $(build_sysconfdir) $(build_datarootdir) $(build_staging))

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(build_prefix): | $(DIRS)
$(eval $(call dir_target,$(SRCDIR)/srccache))

## A rule for calling `make install` ##
#	rule: dependencies
#   	$(call make-install,rel-build-directory,add-args)
#   	touch -c $@
# this rule ensures that make install is more nearly atomic
# so it's harder to get half-installed (or half-reinstalled) dependencies
MAKE_DESTDIR = "$(build_staging)/$1"
define staged-install
	rm -rf $(build_staging)/$1
	$2
	mkdir -p $(build_prefix)
	cp -af $(build_staging)/$1$(build_prefix)/* $(build_prefix)
endef

define make-install
	$(call staged-install,$1,+$(MAKE) -C $(BUILDDIR)/$1 install $(MAKE_COMMON) $2 DESTDIR=$(call MAKE_DESTDIR,$1))
endef

include $(SRCDIR)/tools/git-external.mk

include $(SRCDIR)/llvm.mk
include $(SRCDIR)/libuv.mk
include $(SRCDIR)/pcre.mk
include $(SRCDIR)/openlibm.mk
include $(SRCDIR)/openspecfun.mk
include $(SRCDIR)/dsfmt.mk
include $(SRCDIR)/objconv.mk
include $(SRCDIR)/blas.mk
include $(SRCDIR)/arpack.mk
include $(SRCDIR)/fftw.mk
include $(SRCDIR)/utf8proc.mk
include $(SRCDIR)/suitesparse.mk
include $(SRCDIR)/unwind.mk
include $(SRCDIR)/gmp.mk
include $(SRCDIR)/mpfr.mk
include $(SRCDIR)/patchelf.mk
include $(SRCDIR)/mbedtls.mk
include $(SRCDIR)/libssh2.mk
include $(SRCDIR)/curl.mk
include $(SRCDIR)/libgit2.mk
include $(SRCDIR)/virtualenv.mk

## phony targets ##

.PHONY: default compile install cleanall distcleanall \
	get-* configure-* compile-* check-* install-* \
	clean-* distclean-* reinstall-* update-llvm
