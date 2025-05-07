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
# autoconf configure-driven scripts: pcre unwind gmp mpfr patchelf libuv curl openssl
# custom Makefile rules: openlibm dsfmt libsuitesparse lapack blastrampoline openblas utf8proc objconv libwhich
# CMake libs: llvm llvmunwind libgit2 libssh2 libtracyclient
#
# downloadable via git: llvm-svn, libuv, libopenlibm, utf8proc, libgit2, libssh2, libtracyclient, mmtk_julia
#
# to debug 'define' rules, replace eval at the usage site with info or error


## Overall configuration of which rules exist and should be run by default ##

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

DEP_LIBS := JuliaSyntax

ifeq ($(USE_SYSTEM_LIBBLASTRAMPOLINE), 0)
DEP_LIBS += blastrampoline
endif

ifeq ($(USE_SYSTEM_CSL), 0)
DEP_LIBS += csl
endif

ifeq ($(SANITIZE), 1)
DEP_LIBS += sanitizers
endif

ifeq ($(USE_SYSTEM_LIBUV), 0)
DEP_LIBS += libuv
endif

ifeq ($(WITH_TRACY), 1)
DEP_LIBS += libtracyclient
endif

ifeq ($(DISABLE_LIBUNWIND), 0)
ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
DEP_LIBS += unwind
else ifeq ($(OS), FreeBSD)
DEP_LIBS += unwind
else ifeq ($(OS), OpenBSD)
DEP_LIBS += llvmunwind
else ifeq ($(OS), Darwin)
DEP_LIBS += llvmunwind
endif
endif
endif

ifneq (,$(findstring $(OS),Linux FreeBSD OpenBSD))
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

ifeq ($(USE_SYSTEM_LLD), 0)
DEP_LIBS += lld
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

ifeq ($(USE_SYSTEM_OPENSSL), 0)
DEP_LIBS += openssl
endif

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
DEP_LIBS += libssh2
endif

ifeq ($(USE_SYSTEM_NGHTTP2), 0)
DEP_LIBS += nghttp2
endif

ifeq ($(USE_SYSTEM_CURL), 0)
DEP_LIBS += curl
endif

ifeq ($(USE_SYSTEM_LIBGIT2), 0)
DEP_LIBS += libgit2
endif

ifeq ($(USE_SYSTEM_MPFR), 0)
DEP_LIBS += mpfr
endif

# Only some of the modules in SuiteSparse are GPL.
# xref: `remove-libsuitesparse-gpl-lib` in libsuitesparse.mk
ifeq ($(USE_SYSTEM_LIBSUITESPARSE), 0)
DEP_LIBS += libsuitesparse
endif

ifeq ($(USE_SYSTEM_UTF8PROC), 0)
DEP_LIBS += utf8proc
endif

ifeq ($(USE_SYSTEM_ZLIB), 0)
DEP_LIBS += zlib
endif

ifeq ($(USE_SYSTEM_ZSTD), 0)
DEP_LIBS += zstd
endif

ifeq ($(USE_SYSTEM_P7ZIP), 0)
DEP_LIBS += p7zip
endif

ifeq ($(USE_INTEL_JITEVENTS), 1)
ifeq ($(USE_BINARYBUILDER_LLVM), 0)
DEP_LIBS += ittapi
endif
endif

ifeq ($(WITH_ITTAPI),1)
DEP_LIBS += ittapi
endif

ifeq ($(WITH_NVTX),1)
DEP_LIBS += nvtx
endif

ifneq ($(WITH_TERMINFO),0)
DEP_LIBS += terminfo
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

ifeq ($(USE_SYSTEM_LIBWHICH), 0)
ifneq ($(OS), WINNT)
DEP_LIBS += libwhich
endif
endif

ifeq (${USE_THIRD_PARTY_GC},mmtk)
DEP_LIBS += mmtk_julia
endif

DEP_LIBS_STAGED := $(DEP_LIBS)

# list all targets
DEP_LIBS_STAGED_ALL := llvm llvm-tools clang llvmunwind unwind libuv pcre \
	openlibm dsfmt blastrampoline openblas lapack gmp mpfr patchelf utf8proc \
	objconv openssl libssh2 nghttp2 curl libgit2 libwhich zlib zstd p7zip csl \
	sanitizers libsuitesparse lld libtracyclient ittapi nvtx JuliaSyntax \
	terminfo mmtk_julia
DEP_LIBS_ALL := $(DEP_LIBS_STAGED_ALL)

ifneq ($(USE_BINARYBUILDER_OPENBLAS),0)
DEP_LIBS_ALL := $(filter-out lapack,$(DEP_LIBS_ALL))
endif

ifeq ($(USE_BINARYBUILDER_LLVM),0)
DEP_LIBS_ALL := $(filter-out clang llvm-tools,$(DEP_LIBS_ALL))
endif

ifeq ($(USE_BINARYBUILDER_LIBSUITESPARSE),0)
DEP_LIBS_STAGED := $(filter-out libsuitesparse,$(DEP_LIBS_STAGED))
endif

## Common build target prefixes

default: | $(build_prefix)
get: $(addprefix get-, $(DEP_LIBS))
extract: $(addprefix extract-, $(DEP_LIBS))
configure: $(addprefix configure-, $(DEP_LIBS))
compile: $(addprefix compile-, $(DEP_LIBS))
check: $(addprefix check-, $(DEP_LIBS))
fastcheck: $(addprefix fastcheck-, $(DEP_LIBS))
stage: $(addprefix stage-, $(DEP_LIBS_STAGED))
install: version-check $(addprefix install-, $(DEP_LIBS))
version-check: $(addprefix version-check-, $(DEP_LIBS_STAGED))

uninstall: $(addprefix uninstall-, $(DEP_LIBS_STAGED_ALL))
cleanall: $(addprefix clean-, $(DEP_LIBS_ALL))
distcleanall: $(addprefix distclean-, $(DEP_LIBS_ALL))
	rm -rf $(build_prefix)
getall: $(addprefix get-, $(DEP_LIBS_ALL))

include $(SRCDIR)/BOLT.mk
include $(SRCDIR)/csl.mk
include $(SRCDIR)/sanitizers.mk
include $(SRCDIR)/ittapi.mk
include $(SRCDIR)/nvtx.mk
include $(SRCDIR)/llvm.mk
include $(SRCDIR)/libuv.mk
include $(SRCDIR)/pcre.mk
include $(SRCDIR)/openlibm.mk
include $(SRCDIR)/dsfmt.mk
include $(SRCDIR)/objconv.mk
include $(SRCDIR)/blastrampoline.mk
include $(SRCDIR)/openblas.mk
include $(SRCDIR)/utf8proc.mk
include $(SRCDIR)/libsuitesparse.mk
include $(SRCDIR)/zlib.mk
include $(SRCDIR)/zstd.mk
include $(SRCDIR)/unwind.mk
include $(SRCDIR)/gmp.mk
include $(SRCDIR)/mpfr.mk
include $(SRCDIR)/patchelf.mk
include $(SRCDIR)/openssl.mk
include $(SRCDIR)/libssh2.mk
include $(SRCDIR)/nghttp2.mk
include $(SRCDIR)/curl.mk
include $(SRCDIR)/libgit2.mk
include $(SRCDIR)/libwhich.mk
include $(SRCDIR)/p7zip.mk
include $(SRCDIR)/libtracyclient.mk
include $(SRCDIR)/terminfo.mk

# MMTk
include $(SRCDIR)/mmtk_julia.mk

# vendored Julia libs
include $(SRCDIR)/JuliaSyntax.mk

include $(SRCDIR)/tools/uninstallers.mk
