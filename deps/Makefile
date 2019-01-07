## high-level setup ##
default: install
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
SRCCACHE := $(abspath $(SRCDIR)/srccache)
JULIAHOME := $(abspath $(SRCDIR)/..)
ifeq ($(abspath .),$(abspath $(SRCDIR)))
BUILDDIR := scratch
else
BUILDDIR := .
endif
include $(SRCDIR)/Versions.make
include $(JULIAHOME)/Make.inc
include $(SRCDIR)/tools/common.mk
include $(SRCDIR)/tools/git-external.mk
include $(SRCDIR)/tools/bb-install.mk

BUILDDIR := $(BUILDDIR)$(MAYBE_HOST)

# Special comments:
#
# all targets in here should follow the same structure,
# and provide a get-a, extract-a, configure-a, compile-a, check-a, fastcheck-a, and install-a
# additionally all targets should be listed in the getall target for easier off-line compilation
# if you are adding a new target, it can help to copy an similar, existing target
#
# autoconf configure-driven scripts: llvm pcre unwind gmp mpfr patchelf libuv curl
# custom Makefile rules: openlibm dsfmt suitesparse-wrapper suitesparse lapack openblas utf8proc objconv osxunwind libwhich
# CMake libs: libgit2 libssh2 mbedtls
#
# downloaded from git: llvm-svn, libuv, libopenlibm, utf8proc, libgit2, libssh2
#
# there are rules in this file with the . replaced by a %
# this is some magic Makefile trick that tells make
# that all targets with a % in them on that line will
# be rebuilt in a single invocation
#
# to debug 'define' rules, replace eval at the usage site with info or error
#

## Overall configuration of which rules exist and should be run by default ##

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

DEP_LIBS :=

ifneq ($(OS),emscripten) # For now
ifeq ($(USE_GPL_LIBS), 1)
DEP_LIBS += suitesparse-wrapper
endif
endif

ifeq ($(USE_SYSTEM_LIBUV), 0)
ifeq ($(DISABLE_LIBUV), 0)
DEP_LIBS += libuv
endif
endif

ifeq ($(DISABLE_LIBUNWIND), 0)
ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
DEP_LIBS += unwind
else ifeq ($(OS), FreeBSD)
DEP_LIBS += unwind
else ifeq ($(OS), Darwin)
DEP_LIBS += osxunwind
endif
endif
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
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

ifeq ($(USE_SYSTEM_GMP), 0)
DEP_LIBS += gmp
endif

ifneq ($(OS), emscripten)
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
endif # emscripten

ifeq ($(USE_SYSTEM_MPFR), 0)
DEP_LIBS += mpfr
endif

ifneq ($(OS), emscripten)
ifeq ($(USE_GPL_LIBS), 1)
ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
DEP_LIBS += suitesparse
endif
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

ifneq ($(OS), emscripten)
ifneq ($(OS), WINNT)
DEP_LIBS += libwhich
endif
endif

DEP_LIBS_STAGED := $(filter-out suitesparse suitesparse-wrapper osxunwind,$(DEP_LIBS)) # unlist targets that have not been converted to use the staged-install


## Common build target prefixes

default: | $(build_prefix)
get: $(addprefix get-, $(DEP_LIBS))
extract: $(addprefix extract-, $(DEP_LIBS))
configure: $(addprefix configure-, $(DEP_LIBS))
compile: $(addprefix compile-, $(DEP_LIBS))
check: $(addprefix check-, $(DEP_LIBS))
fastcheck: $(addprefix fastcheck-, $(DEP_LIBS))
stage: $(addprefix stage-, $(DEP_LIBS_STAGED))
install: $(addprefix install-, $(DEP_LIBS))
uninstall: $(addprefix uninstall-, $(DEP_LIBS_STAGED))
cleanall: $(addprefix clean-, $(DEP_LIBS))
distcleanall: $(addprefix distclean-, $(DEP_LIBS))
	rm -rf $(build_prefix)
getall: get-llvm get-libuv get-pcre get-openlibm get-dsfmt get-openblas get-lapack get-suitesparse get-unwind get-osxunwind get-gmp get-mpfr get-patchelf get-utf8proc get-objconv get-mbedtls get-libssh2 get-curl get-libgit2 get-libwhich

include $(SRCDIR)/llvm.mk
include $(SRCDIR)/libuv.mk
include $(SRCDIR)/pcre.mk
include $(SRCDIR)/openlibm.mk
include $(SRCDIR)/dsfmt.mk
include $(SRCDIR)/objconv.mk
include $(SRCDIR)/blas.mk
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
include $(SRCDIR)/libwhich.mk
