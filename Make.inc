# -*- mode: makefile-gmake -*-
# vi:syntax=make

## Note:
##   It is generally preferable to change these options, for
##   your local machine, in a file named `Make.user` in the toplevel
##   and build directories
##
## For developers, take care to not insert comments on the same line as
## variable declarations. The spaces between the variable value and the
## comment will be included in the value.

# Julia precompilation options
# Set to zero to turn off extra precompile (e.g. for the REPL)
JULIA_PRECOMPILE ?= 1

FORCE_ASSERTIONS ?= 0

# OPENBLAS build options
OPENBLAS_TARGET_ARCH:=
OPENBLAS_SYMBOLSUFFIX:=
OPENBLAS_LIBNAMESUFFIX:=

# If OPENBLAS_TARGET_ARCH is set, we default to disabling OPENBLAS_DYNAMIC_ARCH
ifneq ($(OPENBLAS_TARGET_ARCH),)
OPENBLAS_DYNAMIC_ARCH:=0
else
OPENBLAS_DYNAMIC_ARCH:=1
endif
OPENBLAS_USE_THREAD:=1

# Flags for using libraries available on the system instead of building them.
# Please read the notes around usage of SYSTEM flags in README.md
# Issues resulting from use of SYSTEM versions will generally not be accepted.
USE_SYSTEM_LLVM:=0
USE_SYSTEM_LIBUNWIND:=0
DISABLE_LIBUNWIND:=0
USE_SYSTEM_PCRE:=0
USE_SYSTEM_LIBM:=0
USE_SYSTEM_OPENLIBM:=0
UNTRUSTED_SYSTEM_LIBM:=0
USE_SYSTEM_DSFMT:=0
USE_SYSTEM_BLAS:=0
USE_SYSTEM_LAPACK:=0
USE_SYSTEM_GMP:=0
USE_SYSTEM_MPFR:=0
USE_SYSTEM_SUITESPARSE:=0
USE_SYSTEM_LIBUV:=0
USE_SYSTEM_UTF8PROC:=0
USE_SYSTEM_MBEDTLS:=0
USE_SYSTEM_LIBSSH2:=0
USE_SYSTEM_CURL:=0
USE_SYSTEM_LIBGIT2:=0
USE_SYSTEM_PATCHELF:=0
USE_SYSTEM_ZLIB:=0
USE_SYSTEM_P7ZIP:=0

# Link to the LLVM shared library
USE_LLVM_SHLIB := 1

## Settings for various Intel tools
# Set to 1 to use MKL
USE_INTEL_MKL ?= 0
# Set to 1 to use Intel LIBM
USE_INTEL_LIBM ?= 0
# Set to 1 to enable profiling with Intel VTune Amplifier
USE_INTEL_JITEVENTS ?= 0
# Set to 1 to use Intel C, C++, and FORTRAN compilers
USEICC  ?= 0
USEIFC  ?= 0

# Enable threading with one thread
JULIA_THREADS := 1

ifeq ($(USE_MKL), 1)
$(warning "The julia make variable USE_MKL has been renamed to USE_INTEL_MKL")
USE_INTEL_MKL := 1
endif

# Set to 1 to enable profiling with OProfile
USE_OPROFILE_JITEVENTS ?= 0

# Set to 1 to enable profiling with perf
USE_PERF_JITEVENTS ?= 0

# libc++ is standard on OS X 10.9, but not for earlier releases
USE_LIBCPP := 0

# assume we don't have LIBSSP support in our compiler, will enable later if likely true
HAVE_SSP := 0

# GC debugging options
WITH_GC_VERIFY := 0
WITH_GC_DEBUG_ENV := 0

# Prevent picking up $ARCH from the environment variables
ARCH:=

# We need python for things like BB triplet recognition and relative path computation.
# We don't really care about version, generally, so just find something that works:
PYTHON := "$(shell which python 2>/dev/null || which python3 2>/dev/null || which python2 2>/dev/null || echo not found)"
PYTHON_SYSTEM := $(shell $(PYTHON) -c 'from __future__ import print_function; import platform; print(platform.system())')

# If we're running on Cygwin, but using a native-windows Python, we need to use cygpath -w
ifneq ($(and $(filter $(PYTHON_SYSTEM),Windows),$(findstring CYGWIN,$(shell uname))),)
python_cygpath = `cygpath -w $(1)`
else
python_cygpath = $(1)
endif

# Get a relative path easily
define rel_path
$(shell $(PYTHON) $(call python_cygpath,$(JULIAHOME)/contrib/relative_path.py) $(call python_cygpath,$(1)) $(call python_cygpath,$(2)))
endef

# pick up BUILDROOT from O= if it isn't already set (from recursive make)
ifeq ($(BUILDROOT),)
ifeq ("$(origin O)", "command line")
  BUILDROOT := $(abspath $O)
  BUILDDIR := $(abspath $(BUILDROOT)/$(call rel_path,$(JULIAHOME),$(SRCDIR)))
  $(info $(shell printf '\033[32;1mBuilding into $(BUILDROOT)\033[0m')) # use printf to expand the escape sequences
else
  BUILDROOT:=$(JULIAHOME)
endif
endif
export BUILDROOT
unexport O

# Make sure the user didn't try to specify a path that will confuse the shell / make
METACHARACTERS := ][?*{}() $$%:;&|!\#,\\`\":
ifneq (,$(findstring ',$(value BUILDROOT)))
$(error cowardly refusing to build into directory with a single-quote in the path)
endif
ifneq (,$(findstring ',$(value JULIAHOME)))
$(error cowardly refusing to build from source directory with a single-quote in the path)
endif
ifneq (,$(shell echo '$(value BUILDROOT)' | grep '[$(METACHARACTERS)]'))
$(error cowardly refusing to build into directory with a shell-metacharacter in the path\
    (got: $(value BUILDROOT)))
endif
ifneq (,$(shell echo '$(value JULIAHOME)' | grep '[$(METACHARACTERS)]'))
$(error cowardly refusing to build from source directory with a shell-metacharacter in the path\
    (got: $(value JULIAHOME)))
endif

# we include twice to pickup user definitions better
# include from JULIAHOME first so that BUILDROOT can override
MAYBE_HOST :=
ifneq ($(BUILDING_HOST_TOOLS),1)
MAKE_USER_FNAME = Make.user
else
MAYBE_HOST := /host
MAKE_USER_FNAME = Make.host.user
endif

ifeq (exists, $(shell [ -e $(JULIAHOME)/$(MAKE_USER_FNAME) ] && echo exists ))
include $(JULIAHOME)/$(MAKE_USER_FNAME)
endif
ifeq (exists, $(shell [ -e $(BUILDROOT)/$(MAKE_USER_FNAME) ] && echo exists ))
include $(BUILDROOT)/$(MAKE_USER_FNAME)
endif

# disable automatic Makefile rules
.SUFFIXES:

# find out if git repository is available
ifeq ($(shell [ -e $(JULIAHOME)/.git ] && echo true || echo "Warning: git information unavailable; versioning information limited" >&2), true)
NO_GIT := 0
else
NO_GIT := 1
endif

# Julia's Semantic Versioning system labels the three decimal places in a version number as
# the major, minor and patch versions.  Typically the major version would be incremented
# whenever a backwards-incompatible change is made, the minor version would be incremented
# whenever major backwards-compatible changes are made, and the patch version would be
# incremented whenever smaller changes are made.  However, before v1.0.0, the major
# version number is always zero and the meanings shift down a place; the minor version
# number becomes the major version number, the patch version number becomes the minor
# version number, and there is no patch version number to speak of.  In this case, the
# version v0.4.1 has backwards-compatible changes as compared to v0.4.0, and the
# version v0.5.0 has major backwards-incompatible changes as compared to v0.4.X.
JULIA_VERSION := $(shell cat $(JULIAHOME)/VERSION)
JULIA_MAJOR_VERSION := $(shell echo $(JULIA_VERSION) | cut -d'-' -f 1 | cut -d'.' -f 1)
JULIA_MINOR_VERSION := $(shell echo $(JULIA_VERSION) | cut -d'-' -f 1 | cut -d'.' -f 2)
JULIA_PATCH_VERSION := $(shell echo $(JULIA_VERSION) | cut -d'-' -f 1 | cut -d'.' -f 3)

# libjulia's SONAME will follow the format libjulia.so.$(SOMAJOR). Before v1.0.0,
# SOMAJOR will be a two-decimal value, e.g. libjulia.so.0.5, whereas at and beyond
# v1.0.0, SOMAJOR will be simply the major version number, e.g. libjulia.so.1
# The file itself will ultimately symlink to libjulia.so.$(SOMAJOR).$(SOMINOR)
ifeq ($(JULIA_MAJOR_VERSION),0)
SOMAJOR := $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION)
SOMINOR := $(JULIA_PATCH_VERSION)
else
SOMAJOR := $(JULIA_MAJOR_VERSION)
SOMINOR := $(JULIA_MINOR_VERSION)
endif

ifneq ($(NO_GIT), 1)
JULIA_COMMIT := $(shell git -C $(JULIAHOME) rev-parse --short=10 HEAD)
else
JULIA_COMMIT := $(JULIA_VERSION)
endif

# Override `JULIA_COMMIT` to `JULIA_VERSION` if we're on a tagged commit
ifeq ($(shell git -C $(JULIAHOME) describe --tags --exact-match > /dev/null 2>&1 && echo true),true)
JULIA_COMMIT := $(JULIA_VERSION)
endif

# Whether to use GPL libraries or not.
USE_GPL_LIBS ?= 1

# Whether to install Julia as a framework on Darwin (Apple) platforms.
DARWIN_FRAMEWORK ?= 0

# Override in Make.user to customize the framework:
FRAMEWORK_NAME ?= Julia
FRAMEWORK_VERSION ?= $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION)

# The prefix for all code sign identifiers.
DARWIN_CODESIGN_ID_BASE ?= org.julialang.julia
# The codesign id for the ui/repl (also embedded in Info.plist).
darwin_codesign_id_julia_ui := $(DARWIN_CODESIGN_ID_BASE).ui
# The prefix for all deps.
darwin_codesign_id_julia_deps := $(DARWIN_CODESIGN_ID_BASE).deps

# Directories and file structure of a Darwin framework:
framework_directory:=$(FRAMEWORK_NAME).framework
framework_versions:=$(framework_directory)/Versions
framework_currver:=$(framework_versions)/$(FRAMEWORK_VERSION)
framework_dylib:=$(framework_currver)/$(FRAMEWORK_NAME)
framework_headers:=$(framework_currver)/Headers
framework_documentation:=$(framework_currver)/Documentation
framework_resources:=$(framework_currver)/Resources
framework_frameworks:=$(framework_currver)/Frameworks
framework_modules:=$(framework_currver)/Modules
framework_helpers:=$(framework_currver)/Helpers
framework_infoplist:=$(framework_resources)/Info.plist

# Directories where said libraries get installed to
prefix ?= $(BUILDROOT)/julia-$(JULIA_COMMIT)
ifeq ($(DARWIN_FRAMEWORK), 1)
bindir := $(prefix)/$(framework_helpers)
libdir := $(prefix)/$(framework_currver)
libexecdir := $(prefix)/$(framework_helpers)
datarootdir := $(prefix)/$(framework_resources)
docdir := $(prefix)/$(framework_documentation)
mandir := $(datarootdir)/man
man1dir := $(mandir)/man1
includedir := $(prefix)/$(framework_headers)
sysconfdir := $(prefix)/$(framework_resources)
else
bindir := $(prefix)/bin
libdir := $(prefix)/lib
libexecdir := $(prefix)/libexec
datarootdir := $(prefix)/share
docdir := $(datarootdir)/doc/julia
mandir := $(datarootdir)/man
man1dir := $(mandir)/man1
includedir := $(prefix)/include
sysconfdir := $(prefix)/etc
endif

# Directories where things get built into
build_prefix := $(BUILDROOT)/usr$(MAYBE_HOST)
ifeq ($(BUILDING_HOST_TOOLS), 1)
build_staging := $(BUILDROOT)/usr-host-staging
else
build_staging := $(build_prefix)-staging
endif
build_bindir := $(build_prefix)/bin
build_depsbindir := $(build_prefix)/tools
build_libdir := $(build_prefix)/lib
build_libexecdir := $(build_prefix)/libexec
build_datarootdir := $(build_prefix)/share
build_mandir := $(build_datarootdir)/man
build_man1dir := $(build_mandir)/man1
build_includedir := $(build_prefix)/include
build_sysconfdir := $(build_prefix)/etc

# This used for debian packaging, to conform to library layout guidelines
ifeq ($(MULTIARCH_INSTALL), 1)
MULTIARCH := $(shell gcc -print-multiarch)
libdir := $(prefix)/lib/$(MULTIARCH)
build_libdir := $(build_prefix)/lib/$(MULTIARCH)
endif

# Private library directories
ifeq ($(DARWIN_FRAMEWORK), 1)
private_libdir := $(prefix)/$(framework_frameworks)
else
private_libdir := $(libdir)/julia
endif
build_private_libdir := $(build_libdir)/julia

# A helper functions for dealing with lazily-evaluated, expensive operations..  Spinning
# up a python process to, for exaxmple, parse a TOML file is expensive, and we must wait
# until the TOML files are on-disk before we can parse them.  This means that we cannot
# use `:=` (since we do not want to evaluate these rules now, we want to evaluate them
# when we use them, so we use `=`) however we also do not want to re-evaluate them
# multiple times.  So we define a caching mechanism where the rules are still lazily
# evaluated, but we cache the value such that the second time around we don't have to
# re-evaluate them.  Usage example:
#
# EXPENSIVE_OPERATION = $(shell prog args...)
# CACHED_RESULT = $(call hit_cache,EXPENSIVE_OPERATION)
#
# The first time you use `$(CACHED_RESULT)`, it will invoke `$(EXPENSIVE_OPERATION)`,
# but after that point, it will not, unless `$(EXPENSIVE_OPERATION)` evaluated to the
# empty string, in which case it will be re-evaluated.
define hit_cache
$(if $(_CACHE-$(1)),,$(eval _CACHE-$(1) := $($(1))))$(_CACHE-$(1))
endef

# Calculate relative paths to libdir, private_libdir, datarootdir, and sysconfdir
define cache_rel_path
$(1)_rel_eval = $(call rel_path,$(2),$($(1)))
$(1)_rel = $$(call hit_cache,$(1)_rel_eval)
endef
$(foreach D,libdir private_libdir datarootdir libexecdir docdir sysconfdir includedir,$(eval $(call cache_rel_path,$(D),$(bindir))))
$(foreach D,build_libdir build_private_libdir,$(eval $(call cache_rel_path,$(D),$(build_bindir))))

INSTALL_F := $(JULIAHOME)/contrib/install.sh 644
INSTALL_M := $(JULIAHOME)/contrib/install.sh 755

# LLVM Options
LLVMROOT := $(build_prefix)
LLVM_ASSERTIONS := 0
LLVM_DEBUG := 0
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG := 0
# set to 1 to get lldb (often does not work, no chance with llvm3.2 and earlier)
# see http://lldb.llvm.org/build.html for dependencies
BUILD_LLDB := 0

# Options to enable Polly and its code-generation options
USE_POLLY := 0
USE_POLLY_OPENMP := 0  # Enable OpenMP code-generation
USE_POLLY_ACC := 0     # Enable GPU code-generation

# Options to use MLIR
USE_MLIR := 0

# Cross-compile
#XC_HOST := i686-w64-mingw32
#XC_HOST := x86_64-w64-mingw32

# Path to cmake (override in Make.user if needed)
CMAKE ?= cmake
CMAKE_GENERATOR ?= make

# Point pkg-config to only look at our libraries, overriding whatever
# the user may have unwittingly set.  To pass PKG_CONFIG_* variables
# through to the buildsystem, these must be set either on the command
# line, or through `override` directives within Make.user
export PKG_CONFIG_PATH = $(JULIAHOME)/usr/lib/pkgconfig
export PKG_CONFIG_LIBDIR = $(JULIAHOME)/usr/lib/pkgconfig

# Figure out OS and architecture
BUILD_OS := $(shell uname)

ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
XC_HOST ?= $(shell uname -m)-w64-mingw32
endif

ifeq ($(XC_HOST),)
CROSS_COMPILE:=
# delayed expansion of $(CC), since it won't be computed until later
HOSTCC = $(CC)
else
HOSTCC ?= gcc
OPENBLAS_DYNAMIC_ARCH := 1
override CROSS_COMPILE:=$(XC_HOST)-
ifneq (,$(findstring mingw,$(XC_HOST)))
override OS := WINNT
else ifneq (,$(findstring emscripten,$(XC_HOST)))
override OS := emscripten
override CROSS_COMPILE:=
else
ifeq (,$(OS))
$(error "unknown XC_HOST variable set, please set OS")
endif
endif
endif

JLDOWNLOAD := $(JULIAHOME)/deps/tools/jldownload
JLCHECKSUM := $(JULIAHOME)/deps/tools/jlchecksum

# Figure out OS and architecture
OS := $(BUILD_OS)

ifneq (,$(findstring MINGW,$(OS)))
override OS := WINNT
endif
ifneq (,$(findstring MINGW,$(BUILD_OS)))
override BUILD_OS := WINNT
endif
ifneq (,$(findstring MSYS,$(OS)))
override OS := WINNT
endif
ifneq (,$(findstring MSYS,$(BUILD_OS)))
override BUILD_OS := WINNT
endif

ifeq ($(BUILD_OS), WINNT)
BUILD_EXE := .exe
else ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
BUILD_EXE := .exe
else
BUILD_EXE :=
endif
ifeq ($(OS), WINNT)
fPIC :=
EXE := .exe
else
fPIC := -fPIC
EXE :=
endif

JULIACODEGEN := LLVM

# flag for disabling assertions
ifeq ($(FORCE_ASSERTIONS), 1)
# C++ code needs to include LLVM header with the same assertion flag as LLVM
# Use this flag to re-enable assertion in our code after all the LLVM headers are included
CXX_DISABLE_ASSERTION :=
DISABLE_ASSERTIONS :=
else
CXX_DISABLE_ASSERTION := -DJL_NDEBUG
DISABLE_ASSERTIONS := -DNDEBUG -DJL_NDEBUG
endif

# Compiler specific stuff

ifeq ($(USEMSVC), 1)
USEGCC := 0
USECLANG := 0
USEICC := 0
else
ifeq ($(USECLANG), 1)
USEGCC := 0
USEICC := 0
else
ifeq ($(USEICC), 1)
USEGCC := 0
USECLANG := 0
else  # default to gcc
USEGCC := 1
USECLANG := 0
USEICC := 0
endif
endif
endif

ifeq ($(USEIFC), 1)
FC := ifort
else
FC := $(CROSS_COMPILE)gfortran
endif

STDLIBCPP_FLAG :=

ifeq ($(OS), FreeBSD)
USEGCC := 0
USECLANG := 1
endif

ifeq ($(OS), Darwin)
DARWINVER := $(shell uname -r | cut -b 1-2)
DARWINVER_GTE13 := $(shell expr `uname -r | cut -b 1-2` \>= 13)
ifeq ($(DARWINVER), 10) # Snow Leopard specific configuration
USEGCC := 1
USECLANG := 0
OPENBLAS_TARGET_ARCH:=NEHALEM
OPENBLAS_DYNAMIC_ARCH:=0
USE_SYSTEM_LIBUNWIND:=1
else
ifeq ($(DARWINVER_GTE13),1)
USE_LIBCPP := 1
STDLIBCPP_FLAG := -stdlib=libstdc++
else
USE_LIBCPP := 0
endif
USEGCC := 0
USECLANG := 1
endif
endif

ifeq ($(USEGCC),1)
ifeq ($(USE_LIBCPP),1)
$(error USE_LIBCPP only supported with clang. Try setting USE_LIBCPP=0)
endif
ifeq ($(SANITIZE),1)
$(error Address Sanitizer only supported with clang. Try setting SANITIZE=0)
endif
CC := $(CROSS_COMPILE)gcc
CXX := $(CROSS_COMPILE)g++
JCFLAGS := -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
# AArch64 needs this flag to generate the .eh_frame used by libunwind
JCPPFLAGS := -fasynchronous-unwind-tables
JCXXFLAGS := -pipe $(fPIC) -fno-rtti
ifneq ($(OS), WINNT)
# Do not enable on windows to avoid warnings from libuv.
JCXXFLAGS += -pedantic
endif
DEBUGFLAGS := -O0 -ggdb2 -DJL_DEBUG_BUILD -fstack-protector-all
SHIPFLAGS := -O3 -ggdb2 -falign-functions
endif

ifeq ($(USECLANG),1)
CC := $(CROSS_COMPILE)clang
CXX := $(CROSS_COMPILE)clang++
JCFLAGS := -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
# AArch64 needs this flag to generate the .eh_frame used by libunwind
JCPPFLAGS := -fasynchronous-unwind-tables
JCXXFLAGS := -pipe $(fPIC) -fno-rtti -pedantic
DEBUGFLAGS := -O0 -g -DJL_DEBUG_BUILD -fstack-protector-all
SHIPFLAGS := -O3 -g
ifeq ($(OS), Darwin)
ifeq ($(USE_LIBCPP), 1)
MACOSX_VERSION_MIN := 10.8
CC += -stdlib=libc++ -mmacosx-version-min=$(MACOSX_VERSION_MIN)
CXX += -stdlib=libc++ -mmacosx-version-min=$(MACOSX_VERSION_MIN)
FC += -mmacosx-version-min=$(MACOSX_VERSION_MIN)
else
MACOSX_VERSION_MIN := 10.6
CC += $(STDLIBCPP_FLAG) -mmacosx-version-min=$(MACOSX_VERSION_MIN)
CXX += $(STDLIBCPP_FLAG) -mmacosx-version-min=$(MACOSX_VERSION_MIN)
endif
# export MACOSX_DEPLOYMENT_TARGET so that ld picks it up, especially for deps
export MACOSX_DEPLOYMENT_TARGET=$(MACOSX_VERSION_MIN)
JCPPFLAGS += -D_LARGEFILE_SOURCE -D_DARWIN_USE_64_BIT_INODE=1
endif
endif

ifeq ($(USEICC),1)
ifeq ($(USE_LIBCPP),1)
$(error USE_LIBCPP only supported with clang. Try setting USE_LIBCPP=0)
endif
ifeq ($(SANITIZE),1)
$(error Address Sanitizer only supported with clang. Try setting SANITIZE=0)
endif
CC  := icc
CXX := icpc
JCFLAGS := -std=gnu11 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64 -fp-model precise -fp-model except -no-ftz
JCPPFLAGS :=
JCXXFLAGS := -pipe $(fPIC) -fno-rtti
DEBUGFLAGS := -O0 -g -DJL_DEBUG_BUILD -fstack-protector-all
SHIPFLAGS := -O3 -g -falign-functions
endif

ifeq ($(USECCACHE), 1)
# Expand CC, CXX and FC here already because we want the original definition and not the ccache version.
CC_ARG   := $(CC)
CXX_ARG  := $(CXX)
FC_ARG   := $(FC)
# Expand CC, CXX and FC here already to avoid recursive referencing.
CC_FULL  := ccache $(CC)
CXX_FULL := ccache $(CXX)
FC_FULL  := ccache $(FC)
# Add an extra indirection to make CC/CXX/FC non-simple vars
# (because of how -m$(BINARY) is added later on).
CC       := $(CC_FULL)
CXX      := $(CXX_FULL)
FC       := $(FC_FULL)
CC_BASE  := ccache
CXX_BASE := ccache
FC_BASE  := ccache
ifeq ($(USECLANG),1)
# ccache and Clang don't do well together
# http://petereisentraut.blogspot.be/2011/05/ccache-and-clang.html
CC += -Qunused-arguments
CXX += -Qunused-arguments
# http://petereisentraut.blogspot.be/2011/09/ccache-and-clang-part-2.html
export CCACHE_CPP2 := yes
endif
else #USECCACHE
CC_BASE := $(shell echo $(CC) | cut -d' ' -f1)
CC_ARG := $(shell echo $(CC) | cut -s -d' ' -f2-)
CXX_BASE := $(shell echo $(CXX) | cut -d' ' -f1)
CXX_ARG := $(shell echo $(CXX) | cut -s -d' ' -f2-)
FC_BASE := $(shell echo $(FC) | cut -d' ' -f1)
FC_ARG := $(shell echo $(FC) | cut -s -d' ' -f2-)
endif

JFFLAGS := -O2 $(fPIC)
ifneq ($(USEMSVC),1)
CPP := $(CC) -E
AR := $(CROSS_COMPILE)ar
AS := $(CROSS_COMPILE)as
LD := $(CROSS_COMPILE)ld
else #USEMSVC
CPP := $(CC) -EP
AR := lib
ifeq ($(ARCH),x86_64)
AS := ml64
else
AS := ml
endif #ARCH
LD := link
endif #USEMSVC
RANLIB := $(CROSS_COMPILE)ranlib
OBJCOPY := $(CROSS_COMPILE)objcopy

# file extensions
ifeq ($(OS), WINNT)
  SHLIB_EXT := dll
else ifeq ($(OS), Darwin)
  SHLIB_EXT := dylib
else
  SHLIB_EXT := so
endif

# On Windows, we want shared library files to end up in $(build_bindir), instead of $(build_libdir)
ifeq ($(OS),WINNT)
build_shlibdir := $(build_bindir)
else
build_shlibdir := $(build_libdir)
endif

ifeq ($(OS), FreeBSD)
LOCALBASE ?= /usr/local
else
LOCALBASE ?= /usr
endif

ifeq (exists, $(shell [ -e $(JULIAHOME)/$(MAKE_USER_FNAME) ] && echo exists ))
include $(JULIAHOME)/$(MAKE_USER_FNAME)
endif
ifeq (exists, $(shell [ -e $(BUILDROOT)/$(MAKE_USER_FNAME) ] && echo exists ))
include $(BUILDROOT)/$(MAKE_USER_FNAME)
endif

# A bit of a kludge to work around libraries linking to FreeBSD's outdated system libgcc_s
# Instead, let's link to the libgcc_s corresponding to the installation of gfortran
ifeq ($(OS),FreeBSD)
ifneq (,$(findstring gfortran,$(FC)))

# First let's figure out what version of GCC we're dealing with
_GCCMAJOR := $(shell $(FC) -dumpversion | cut -d'.' -f1)
_GCCMINOR := $(shell $(FC) -dumpversion | cut -d'.' -f2)

# The ports system uses major and minor for GCC < 5 (e.g. gcc49 for GCC 4.9), otherwise major only
ifeq ($(_GCCMAJOR),4)
  _GCCVER := $(_GCCMAJOR)$(_GCCMINOR)
else
  _GCCVER := $(_GCCMAJOR)
endif

# Allow the user to specify this in Make.user
GCCPATH ?= $(LOCALBASE)/lib/gcc$(_GCCVER)

# We're going to copy over the libraries we need from GCCPATH into build_libdir, then
# tell everyone to look for them there. At install time, the build_libdir added into
# the RPATH here is removed by patchelf.
LDFLAGS += -L$(build_libdir) -Wl,-rpath,$(build_libdir)

endif # gfortran
endif # FreeBSD

ifneq ($(CC_BASE)$(CXX_BASE),$(shell echo $(CC) | cut -d' ' -f1)$(shell echo $(CXX) | cut -d' ' -f1))
    $(error Forgot override directive on CC or CXX in Make.user? Cowardly refusing to build)
endif

ifeq ($(DARWIN_FRAMEWORK),1)
ifneq ($(OS), Darwin)
	$(error Darwin framework cannot be enabled for non-Darwin OS)
endif
endif

ifeq ($(SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
SANITIZE_OPTS := -fsanitize=memory -fsanitize-memory-track-origins -fno-omit-frame-pointer
SANITIZE_LDFLAGS := $(SANITIZE_OPTS)
else
SANITIZE_OPTS := -fsanitize=address -mllvm -asan-stack=0
SANITIZE_LDFLAGS := -fsanitize=address
endif
JCXXFLAGS += $(SANITIZE_OPTS)
JCFLAGS += $(SANITIZE_OPTS)
JLDFLAGS += $(SANITIZE_LDFLAGS)
endif

TAR := $(shell which gtar 2>/dev/null || which tar 2>/dev/null)
TAR_TEST := $(shell $(TAR) --help 2>&1  | egrep 'bsdtar|strip-components')
ifeq (,$(findstring components,$(TAR_TEST)))
ifneq (bsdtar,$(findstring bsdtar,$(TAR_TEST)))
$(error "please install either GNU tar or bsdtar")
endif
endif

ifeq ($(WITH_GC_VERIFY), 1)
JCXXFLAGS += -DGC_VERIFY
JCFLAGS += -DGC_VERIFY
endif

ifeq ($(WITH_GC_DEBUG_ENV), 1)
JCXXFLAGS += -DGC_DEBUG_ENV
JCFLAGS += -DGC_DEBUG_ENV
endif

# ===========================================================================

# Select the cpu architecture to target, or automatically detects the user's compiler
# ARCH is the first element of the triple, and gives the CPU class (e.g. x86_64)
# MARCH is the CPU type, and accepts anything that can be passed to the gcc -march flag
#    it is set equal to ARCH (for cases where the two are the same, such as i686)
#    it can be set to native to optimize all code for the user's machine (not just the JIT code)
#    if MARCH is set newer than the native processor, be forewarned that the compile might fail
# JULIA_CPU_TARGET is the JIT-only complement to MARCH. Setting it explicitly is not generally necessary,
#    since it is set equal to MARCH by default

BUILD_MACHINE := $(shell $(HOSTCC) -dumpmachine)

# Clang spells mingw `-windows-gnu`, but autotools, etc
# don't recognize that, so canonicalize to mingw32
BUILD_MACHINE := $(subst windows-gnu,mingw32,$(BUILD_MACHINE))

ifeq ($(ARCH),)
override ARCH := $(shell $(CC) -dumpmachine | sed "s/\([^-]*\).*$$/\1/")
else
ifneq ($(XC_HOST),)
XC_HOST := $(ARCH)$(shell echo $(XC_HOST) | sed "s/[^-]*\(.*\)$$/\1/")
ifneq ($(findstring arm, $(ARCH))$(findstring aarch64, $(ARCH)),)
MCPU := $(subst _,-,$(ARCH)) # Arm prefers MCPU over MARCH
else
MARCH := $(subst _,-,$(ARCH))
endif
else # insert ARCH into HOST
XC_HOST := $(ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
endif
endif

ifeq ($(ARCH),mingw32)
$(error "the mingw32 compiler you are using fails the openblas testsuite. please see the README.windows document for a replacement")
else ifeq (cygwin, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with cygwin-target compilers. set XC_HOST to i686-w64-mingw32 or x86_64-w64-mingw32 for mingw cross-compile")
else ifeq (msys, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with msys-target compilers. please see the README.windows document for instructions on setting up mingw-w64 compilers")
endif

ifeq ($(BUILD_OS),Darwin)
## Mac is a rather cool 64-bit user-space on 32-bit kernel architecture, so to determine arch we detect
## the capabilities of the hardware, rather than the compiler or kernel, and make a substitution
BUILD_ARCH := $(shell echo $(BUILD_MACHINE) | sed "s/\([^-]*\).*$$/\1/")
ifeq ($(BUILD_ARCH),x86_64)
BUILD_ARCH := i686
else ifeq ($(BUILD_ARCH),i386)
BUILD_ARCH := i686
endif
ifeq ($(BUILD_ARCH),i686)
ifeq ($(shell sysctl -n hw.cpu64bit_capable),1)
BUILD_ARCH := x86_64
endif
BUILD_MACHINE := $(BUILD_ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
endif
ifeq ($(BUILD_OS),$(OS))
ARCH := $(BUILD_OS)
endif
endif

# Detect common pre-SSE2 JULIA_CPU_TARGET values known not to work (#7185)
ifeq ($(MARCH),)
ifneq ($(findstring $(ARCH),i386 i486 i586 i686),)
MARCH := pentium4
endif
endif

ifneq ($(findstring $(MARCH),i386 i486 i586 i686 pentium pentium2 pentium3),)
$(error Pre-SSE2 CPU targets not supported. To create a generic 32-bit x86 binary, \
pass 'MARCH=pentium4'.)
endif

# We map amd64 to x86_64 for compatibility with systems that identify 64-bit systems as such
ifeq ($(ARCH),amd64)
override ARCH := x86_64
endif

# We map arm64 (Apple spelling) to aarch64 to avoid having to deal with both spellings everywhere
ifeq ($(ARCH),arm64)
override ARCH := aarch64
endif

ifeq ($(ARCH),i386)
BINARY:=32
ISX86:=1
else ifeq ($(ARCH),i387)
BINARY:=32
ISX86:=1
else ifeq ($(ARCH),i486)
BINARY:=32
ISX86:=1
else ifeq ($(ARCH),i586)
BINARY:=32
ISX86:=1
else ifeq ($(ARCH),i686)
BINARY:=32
ISX86:=1
else ifeq ($(ARCH),x86_64)
BINARY:=64
ISX86:=1
else
# For all other architectures (ARM, PPC, AArch64, etc.)
ISX86:=0
endif

# If we are running on powerpc64le or ppc64le, set certain options automatically
ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
JCFLAGS += -fsigned-char
OPENBLAS_DYNAMIC_ARCH:=0
OPENBLAS_TARGET_ARCH:=POWER8
BINARY:=64
# GCC doesn't do -march= on ppc64le
MARCH=
endif

# If we are running on powerpc64 or ppc64, fail out dramatically
ifneq (,$(filter $(ARCH), powerpc64 ppc64))
$(error Big-endian PPC64 is not supported, to ignore this error, set ARCH=ppc64le)
endif

# File name of make binary-dist result
ifeq ($(JULIA_BINARYDIST_FILENAME),)
DIST_OS:=$(shell echo $(OS) | tr '[:upper:]' '[:lower:]')
ifeq (WINNT,$(OS))
DIST_OS:=win
endif
ifeq (Linux,$(OS))
DIST_OS:=linux
endif
ifeq (Darwin,$(OS))
DIST_OS:=mac
endif
DIST_ARCH:=$(ARCH)
ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
DIST_ARCH:=ppc64le
endif
ifeq (1,$(ISX86))
DIST_ARCH:=$(BINARY)
endif
ifneq (,$(findstring arm,$(ARCH)))
DIST_ARCH:=arm
endif

JULIA_BINARYDIST_FILENAME := julia-$(JULIA_COMMIT)-$(DIST_OS)$(DIST_ARCH)
endif

# If we are running on ARM, set certain options automatically
ifneq (,$(findstring arm,$(ARCH)))
JCFLAGS += -fsigned-char
USE_BLAS64:=0
OPENBLAS_DYNAMIC_ARCH:=0
OPENBLAS_TARGET_ARCH:=ARMV7
endif

# If we are running on aarch64 (e.g. ARMv8 or ARM64), set certain options automatically
ifneq (,$(findstring aarch64,$(ARCH)))
OPENBLAS_DYNAMIC_ARCH:=0
OPENBLAS_TARGET_ARCH:=ARMV8
ifeq ($(OS),Darwin)
# Apple Chips are all at least A12Z
MCPU:=apple-a12
endif
endif

# Set MARCH-specific flags
ifneq ($(MARCH),)
CC += -march=$(MARCH)
CXX += -march=$(MARCH)
FC += -march=$(MARCH)
JULIA_CPU_TARGET ?= $(MARCH)
endif

# Set MCPU-specific flags
ifneq ($(MCPU),)
CC += -mcpu=$(MCPU)
CXX += -mcpu=$(MCPU)
FC += -mcpu=$(MCPU)
JULIA_CPU_TARGET ?= $(MCPU)
endif

ifneq ($(MARCH)$(MCPU),)
ifeq ($(OS),Darwin)
# on Darwin, the standalone `as` program doesn't know
# how to handle AVX instructions, but it does know how
# to dispatch to the clang assembler (if we ask it to)
ifeq ($(USECLANG),1)
CC += -integrated-as
CXX += -integrated-as
else
CC += -Wa,-q
CXX += -Wa,-q
endif
FC += -Wa,-q
AS += -q
endif
endif

JULIA_CPU_TARGET ?= native

ifneq ($(OS),WINNT)
# Windows headers with this configuration conflicts with LLVM
# (Symbol renames are done with macros)
# We mainly need this on linux for cgmemmgr so don't worry about windows for now...
JCXXFLAGS += -D_FILE_OFFSET_BITS=64
endif

# Set some ARCH-specific flags
ifneq ($(USEICC),1)
ifeq ($(ISX86),1)
CC += -m$(BINARY)
CXX += -m$(BINARY)
FC += -m$(BINARY)
CC_ARG += -m$(BINARY)
CXX_ARG += -m$(BINARY)
FC_ARG += -m$(BINARY)
endif
endif

ifeq ($(OS),WINNT)
ifneq ($(ARCH),x86_64)
ifneq ($(USECLANG),1)
JCFLAGS += -mincoming-stack-boundary=2
JCXXFLAGS += -mincoming-stack-boundary=2
endif
endif
endif

ifeq ($(USEGCC),1)
ifeq ($(ISX86),1)
  SHIPFLAGS += -momit-leaf-frame-pointer
endif
endif

ifeq ($(OS),WINNT)
LIBUNWIND:=
else ifneq ($(DISABLE_LIBUNWIND), 0)
LIBUNWIND:=
else
ifeq ($(USE_SYSTEM_LIBUNWIND), 1)
ifneq ($(OS),Darwin)
LIBUNWIND:=-lunwind-generic -lunwind
# Only for linux since we want to use not yet released libunwind features
JCFLAGS+=-DSYSTEM_LIBUNWIND
JCPPFLAGS+=-DSYSTEM_LIBUNWIND
endif
else
ifeq ($(OS),Darwin)
LIBUNWIND:=$(build_libdir)/libosxunwind.a
JCPPFLAGS+=-DLIBOSXUNWIND
else
LIBUNWIND:=$(build_libdir)/libunwind-generic.a $(build_libdir)/libunwind.a
endif
endif
endif

ifeq ($(origin LLVM_CONFIG), undefined)
ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_CONFIG := llvm-config$(EXE)
else
LLVM_CONFIG := $(build_depsbindir)/llvm-config$(EXE)
endif
endif # LLVM_CONFIG undefined

ifeq ($(USE_SYSTEM_LLVM), 1)
JCPPFLAGS+=-DSYSTEM_LLVM
endif # SYSTEM_LLVM

ifeq ($(BUILD_OS),$(OS))
LLVM_CONFIG_HOST := $(LLVM_CONFIG)
else
LLVM_CONFIG_HOST := $(basename $(LLVM_CONFIG))-host$(BUILD_EXE)
ifeq (exists, $(shell [ -f '$(LLVM_CONFIG_HOST)' ] && echo exists ))
ifeq ($(shell $(LLVM_CONFIG_HOST) --version),3.3)
# llvm-config-host <= 3.3 is broken, use llvm-config instead (in an emulator)
# use delayed expansion (= not :=) because spawn isn't defined until later
LLVM_CONFIG_HOST = $(call spawn,$(LLVM_CONFIG))
endif
else
# llvm-config-host does not exist (cmake build)
LLVM_CONFIG_HOST = $(call spawn,$(LLVM_CONFIG))
endif
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG := pcre2-config
else
PCRE_CONFIG := $(build_depsbindir)/pcre2-config
endif

ifeq ($(USE_SYSTEM_PATCHELF), 1)
PATCHELF := patchelf
else
PATCHELF := $(build_depsbindir)/patchelf
endif

# On aarch64 and powerpc64le, we assume the page size is 64K.  Our binutils linkers
# and such already assume this, but `patchelf` seems to be behind the times.  We
# explicitly tell it to use this large page size so that when we rewrite rpaths and
# such, we don't accidentally create incorrectly-aligned sections in our ELF files.
ifneq (,$(filter $(ARCH),aarch64 powerpc64le))
PATCHELF += --page-size 65536
endif

# Use ILP64 BLAS interface when building openblas from source on 64-bit architectures
ifeq ($(BINARY), 64)
ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(USE_INTEL_MKL), 1)
USE_BLAS64 ?= 1
else # non MKL system blas is most likely LP64
USE_BLAS64 ?= 0
endif
else
USE_BLAS64 ?= 1
endif
endif

ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(OS), Darwin)
USE_BLAS64 := 0
USE_SYSTEM_LAPACK := 0
LIBBLAS := -L$(build_libdir) -lgfortblas
LIBBLASNAME := libgfortblas
else
LIBBLAS ?= -lblas
LIBBLASNAME ?= libblas
endif
else
LIBBLAS := -L$(build_shlibdir) -lopenblas
LIBBLASNAME := libopenblas
endif

# OpenBLAS builds LAPACK as part of its build.
# We only need to build LAPACK if we are not using OpenBLAS.
ifeq ($(USE_SYSTEM_BLAS), 0)
LIBLAPACK := $(LIBBLAS)
LIBLAPACKNAME := $(LIBBLASNAME)
else
ifeq ($(USE_SYSTEM_LAPACK), 1)
LIBLAPACK ?= -llapack
LIBLAPACKNAME ?= liblapack
else
LIBLAPACK := -L$(build_shlibdir) -llapack $(LIBBLAS)
LIBLAPACKNAME := liblapack
endif
endif

ifeq ($(USE_SYSTEM_LIBM), 1)
LIBM := -lm
LIBMNAME := libm
else
LIBM := -lopenlibm
LIBMNAME := libopenlibm
endif

ifeq ($(USE_SYSTEM_LIBUV), 1)
  LIBUV := $(LOCALBASE)/lib/libuv-julia.a
  LIBUV_INC := $(LOCALBASE)/include
else
  LIBUV := $(build_libdir)/libuv.a
  LIBUV_INC := $(build_includedir)
endif

ifeq ($(USE_SYSTEM_UTF8PROC), 1)
  LIBUTF8PROC := -lutf8proc
  UTF8PROC_INC := $(LOCALBASE)/include
else
  LIBUTF8PROC := $(build_libdir)/libutf8proc.a
  UTF8PROC_INC := $(build_includedir)
endif

# We need python for things like BB triplet recognition.  We don't really care
# about version, generally, so just find something that works:
PYTHON := "$(shell which python 2>/dev/null || which python3 2>/dev/null || which python2 2>/dev/null || echo not found)"
PYTHON_SYSTEM := $(shell $(PYTHON) -c 'from __future__ import print_function; import platform; print(platform.system())')

# If we're running on Cygwin, but using a native-windows Python, we need to use cygpath -w
ifneq ($(and $(filter $(PYTHON_SYSTEM),Windows),$(findstring CYGWIN,$(BUILD_OS))),)
define invoke_python
$(PYTHON) "$$(cygpath -w "$(1)")"
endef
else
define invoke_python
$(PYTHON) "$(1)"
endef
endif

# BinaryBuilder options.  We default to "on" for all the projects listed in BB_PROJECTS,
# but only if contrib/normalize_triplet.py works for our requested triplet.
ifeq ($(shell $(call invoke_python,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)) >/dev/null 2>/dev/null; echo $$?),0)
USE_BINARYBUILDER ?= 1
else
ifneq ($(shell $(call invoke_python,$(JULIAHOME)/contrib/normalize_triplet.py) x86_64-linux-gnu),x86_64-linux-gnu)
$(warning normalize_triplet.py appears to be non-functional (used python interpreter "$(PYTHON)"), so BinaryBuilder disabled)
endif
USE_BINARYBUILDER ?= 0
endif

# This is the set of projects that BinaryBuilder dependencies are hooked up for.
BB_PROJECTS := OPENBLAS LLVM SUITESPARSE OPENLIBM GMP MBEDTLS LIBSSH2 MPFR CURL LIBGIT2 PCRE LIBUV LIBUNWIND DSFMT OBJCONV ZLIB P7ZIP
define SET_BB_DEFAULT
# First, check to see if BB is disabled on a global setting
ifeq ($$(USE_BINARYBUILDER),0)
USE_BINARYBUILDER_$(1) ?= 0
else
# If it's not, check to see if it's disabled by a USE_SYSTEM_xxx flag
ifeq ($$(USE_SYSTEM_$(1)),1)
USE_BINARYBUILDER_$(1) ?= 0
else
USE_BINARYBUILDER_$(1) ?= 1
endif
endif
endef
$(foreach proj,$(BB_PROJECTS),$(eval $(call SET_BB_DEFAULT,$(proj))))



# Use the Assertions build
BINARYBUILDER_LLVM_ASSERTS ?= 0



# OS specific stuff

# install_name_tool
ifeq ($(OS), Darwin)
  # must end with a / and have no trailing spaces
  INSTALL_NAME_ID_DIR := @rpath/
  INSTALL_NAME_CMD := install_name_tool -id $(INSTALL_NAME_ID_DIR)
  INSTALL_NAME_CHANGE_CMD := install_name_tool -change
ifneq (,$(findstring LLVM,$(shell dsymutil --version)))
  DSYMUTIL := dsymutil
else ifeq ($(shell test `dsymutil -v | cut -d\- -f2 | cut -d. -f1` -gt 102 && echo yes), yes)
  DSYMUTIL := dsymutil
else
  DSYMUTIL := true -ignore
endif
else
  INSTALL_NAME_ID_DIR :=
  INSTALL_NAME_CMD := true -ignore
  INSTALL_NAME_CHANGE_CMD := true -ignore
  DSYMUTIL := true -ignore
endif

# shared library runtime paths
ifneq (,$(filter $(OS),WINNT emscripten))
  RPATH :=
  RPATH_ORIGIN :=
  RPATH_ESCAPED_ORIGIN :=
  RPATH_LIB :=
else ifeq ($(OS), Darwin)
  RPATH := -Wl,-rpath,'@executable_path/$(build_libdir_rel)'
  RPATH_ORIGIN := -Wl,-rpath,'@loader_path/'
  RPATH_ESCAPED_ORIGIN := $(RPATH_ORIGIN)
  RPATH_LIB := -Wl,-rpath,'@loader_path/julia/' -Wl,-rpath,'@loader_path/'
else
  RPATH := -Wl,-rpath,'$$ORIGIN/$(build_libdir_rel)' -Wl,-rpath,'$$ORIGIN/$(build_private_libdir_rel)' -Wl,-rpath-link,$(build_shlibdir) -Wl,-z,origin
  RPATH_ORIGIN := -Wl,-rpath,'$$ORIGIN' -Wl,-z,origin
  RPATH_ESCAPED_ORIGIN := -Wl,-rpath,'\$$\$$ORIGIN' -Wl,-z,origin -Wl,-rpath-link,$(build_shlibdir)
  RPATH_LIB := -Wl,-rpath,'$$ORIGIN/julia' -Wl,-rpath,'$$ORIGIN' -Wl,-z,origin
endif

# --whole-archive
ifeq ($(OS), Darwin)
  WHOLE_ARCHIVE := -Xlinker -all_load
  NO_WHOLE_ARCHIVE :=
else ifneq ($(USEMSVC), 1)
  WHOLE_ARCHIVE := -Wl,--whole-archive
  NO_WHOLE_ARCHIVE := -Wl,--no-whole-archive
endif

ifeq ($(OS), Linux)
OSLIBS += -Wl,--no-as-needed -ldl -lrt -lpthread -Wl,--export-dynamic,--as-needed,--no-whole-archive
# Detect if ifunc is supported
IFUNC_DETECT_SRC := 'void (*f0(void))(void) { return (void(*)(void))0L; }; void f(void) __attribute__((ifunc("f0")));'
ifeq (supported, $(shell echo $(IFUNC_DETECT_SRC) | $(CC) -Werror -x c - -S -o /dev/null > /dev/null 2>&1 && echo supported))
JCPPFLAGS += -DJULIA_HAS_IFUNC_SUPPORT=1
endif
JLDFLAGS := -Wl,-Bdynamic
ifneq ($(SANITIZE),1)
ifneq ($(SANITIZE_MEMORY),1)
ifneq ($(LLVM_SANITIZE),1)
OSLIBS += -Wl,--version-script=$(JULIAHOME)/src/julia.expmap
JLDFLAGS += -Wl,-no-undefined
endif
endif
endif
ifeq (-Bsymbolic-functions, $(shell $(LD) --help | grep -o -e "-Bsymbolic-functions"))
JLIBLDFLAGS := -Wl,-Bsymbolic-functions
else
JLIBLDFLAGS :=
endif
else ifneq ($(OS), Darwin)
JLIBLDFLAGS :=
endif

ifeq ($(OS), FreeBSD)
JLDFLAGS := -Wl,-Bdynamic
OSLIBS += -lelf -lkvm -lrt -lpthread

# Tweak order of libgcc_s in DT_NEEDED,
# make it loaded first to
# prevent from linking to outdated system libs.
# See #21788
OSLIBS += -lgcc_s

OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap \
	$(NO_WHOLE_ARCHIVE) $(LIBUNWIND)
endif

ifeq ($(OS), Darwin)
SHLIB_EXT := dylib
OSLIBS += -framework CoreFoundation $(LIBUNWIND)
WHOLE_ARCHIVE := -Xlinker -all_load
NO_WHOLE_ARCHIVE :=
JLDFLAGS :=
HAVE_SSP := 1
JLIBLDFLAGS := -Wl,-compatibility_version,$(SOMAJOR) -Wl,-current_version,$(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)
endif

ifeq ($(OS), WINNT)
ifneq ($(USEMSVC), 1)
HAVE_SSP := 1
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(JULIAHOME)/src/julia.expmap \
	$(NO_WHOLE_ARCHIVE) -lpsapi -lkernel32 -lws2_32 -liphlpapi -lwinmm -ldbghelp -luserenv -lsecur32
JLDFLAGS := -Wl,--stack,8388608
ifeq ($(ARCH),i686)
JLDFLAGS += -Wl,--large-address-aware
endif
else #USEMSVC
OSLIBS += kernel32.lib ws2_32.lib psapi.lib advapi32.lib iphlpapi.lib shell32.lib winmm.lib userenv.lib secur32.lib
JLDFLAGS := -stack:8388608
endif
JCPPFLAGS += -D_WIN32_WINNT=0x0502
UNTRUSTED_SYSTEM_LIBM := 1
endif

# Threads
ifneq ($(JULIA_THREADS), 0)
JCPPFLAGS += -DJULIA_NUM_THREADS=$(JULIA_THREADS)
endif

# Intel VTune Amplifier
ifeq ($(USE_INTEL_JITEVENTS), 1)
JCPPFLAGS += -DJL_USE_INTEL_JITEVENTS
endif

# OProfile
ifeq ($(USE_OPROFILE_JITEVENTS), 1)
JCPPFLAGS += -DJL_USE_OPROFILE_JITEVENTS
endif

ifeq ($(DISABLE_LIBUNWIND), 1)
JCPPFLAGS += -DJL_DISABLE_LIBUNWIND
endif

# perf
ifeq ($(USE_PERF_JITEVENTS), 1)
JCPPFLAGS += -DJL_USE_PERF_JITEVENTS
endif


# Intel libraries

ifeq ($(USE_INTEL_LIBM), 1)
USE_SYSTEM_LIBM := 1
LIBM := -L$(MKLROOT)/../compiler/lib/intel64 -limf
LIBMNAME := libimf
endif

ifeq ($(USE_INTEL_MKL), 1)
ifeq ($(USE_BLAS64), 1)
export MKL_INTERFACE_LAYER := ILP64
MKLLIB := $(MKLROOT)/lib/intel64
else
MKLLIB := $(MKLROOT)/lib/ia32
endif
USE_SYSTEM_BLAS:=1
USE_SYSTEM_LAPACK:=1
LIBBLASNAME := libmkl_rt
LIBLAPACKNAME := libmkl_rt
MKL_LDFLAGS := -L$(MKLLIB) -lmkl_rt
ifneq ($(strip $(MKLLIB)),)
  ifeq ($(OS), Linux)
    RPATH_MKL := -Wl,-rpath,$(MKLLIB)
    RPATH     += $(RPATH_MKL)
    MKL_LDFLAGS += $(RPATH_MKL)
  endif
endif
LIBBLAS   := $(MKL_LDFLAGS)
LIBLAPACK := $(MKL_LDFLAGS)
endif

ifeq ($(HAVE_SSP),1)
JCPPFLAGS += -DHAVE_SSP=1
ifeq ($(USEGCC),1)
OSLIBS += -lssp
endif
endif

# ATLAS

# ATLAS must have been previously  installed to usr/lib/libatlas
# (built as a shared library, for your platform, single threaded)
USE_ATLAS := 0
ATLAS_LIBDIR := $(build_libdir)
#or ATLAS_LIBDIR := /path/to/system/atlas/lib

ifeq ($(USE_ATLAS), 1)
USE_BLAS64 := 0
USE_SYSTEM_BLAS := 1
USE_SYSTEM_LAPACK := 1
LIBBLAS := -L$(ATLAS_LIBDIR) -lsatlas
LIBLAPACK := $(LIBBLAS)
LIBBLASNAME := libsatlas
LIBLAPACKNAME := $(LIBBLASNAME)
endif

# Renaming OpenBLAS symbols, see #4923 and #8734
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(USE_BLAS64), 1)
OPENBLAS_SYMBOLSUFFIX := 64_
OPENBLAS_LIBNAMESUFFIX := 64_
LIBBLAS := -L$(build_shlibdir) -lopenblas$(OPENBLAS_LIBNAMESUFFIX)
LIBLAPACK := $(LIBBLAS)
LIBBLASNAME := $(LIBBLASNAME)$(OPENBLAS_LIBNAMESUFFIX)
LIBLAPACKNAME := $(LIBBLASNAME)
endif
endif

# Custom libcxx
ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LDFLAGS += -L$(build_libdir)
CXXLDFLAGS += -L$(build_libdir) -lc++abi -lc++
ifeq ($(USECLANG),1)
CXXLDFLAGS += -stdlib=libc++
else
ifeq ($(USEGCC),1)
$(error BUILD_CUSTOM_LIBCXX is currently only supported with Clang. Try setting BUILD_CUSTOM_LIBCXX=0 or USECLANG=1)
endif
endif # Clang
CUSTOM_LD_LIBRARY_PATH := LD_LIBRARY_PATH="$(build_libdir)"
ifeq ($(USEICC),1)
CXXFLAGS += -cxxlib-nostd -static-intel
CLDFLAGS += -static-intel
LDFLAGS += -cxxlib-nostd -static-intel
endif
endif

# Some special restrictions on BB usage:
ifeq ($(USE_SYSTEM_BLAS),1)
# Since the names don't line up (`BLAS` vs. `OPENBLAS`), manually gate:
USE_BINARYBUILDER_OPENBLAS := 0
# Disable BB SuiteSparse if we're using system BLAS
USE_BINARYBUILDER_SUITESPARSE := 0
endif

ifeq ($(USE_SYSTEM_LIBM),1)
# If we're using system libm, disable BB OpenLibm
USE_BINARYBUILDER_OPENLIBM := 0
endif


# Note: we're passing *FLAGS here computed based on your system compiler to
# clang. If that causes you problems, you might want to build and/or run
# specific clang-sa-* files with clang explicitly selected:
#   make CC=~+/../usr/tools/clang CXX=~+/../usr/tools/clang USECLANG=1 analyzegc
#   make USECLANG=1 clang-sa-*
CLANGSA_FLAGS :=
CLANGSA_CXXFLAGS :=
ifeq ($(OS), Darwin) # on new XCode, the files are hidden
CLANGSA_FLAGS += -isysroot $(shell xcode-select -p)/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk
CLANGSA_CXXFLAGS += -isystem $(shell xcode-select -p)/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1
endif
ifeq ($(USEGCC),1)
# try to help clang find the c++ files for CC by guessing the value for --prefix
# by dropping lib/gcc/<platform>/<version> from the install directory it reports
CLANGSA_CXXFLAGS += --gcc-toolchain="$(abspath $(shell LANG=C $(CC) -print-search-dirs | grep '^install: ' | sed -e "s/^install: //")/../../../..)"
endif


# Make tricks

define dir_target
$$(abspath $(1)):
	@mkdir -p $$@
endef

ifeq ($(BUILD_OS), WINNT)
define mingw_to_dos
$(subst /,\\,$(subst $(shell $(2) pwd),$(shell $(2) cmd //C cd),$(abspath $(1))))
endef
endif

define symlink_target # (from, to-dir, to-name)
CLEAN_TARGETS += clean-$$(abspath $(2)/$(3))
clean-$$(abspath $(2)/$(3)):
ifeq ($(BUILD_OS), WINNT)
	-cmd //C rmdir $$(call mingw_to_dos,$(2)/$(3),cd $(2) &&)
else
	-rm -r $$(abspath $(2)/$(3))
endif
$$(abspath $(2)/$(3)): | $$(abspath $(2))
ifeq ($$(BUILD_OS), WINNT)
	@cmd //C mklink //J $$(call mingw_to_dos,$(2)/$(3),cd $(2) &&) $$(call mingw_to_dos,$(1),)
else ifneq (,$$(findstring CYGWIN,$$(BUILD_OS)))
	@cmd /C mklink /J $$(call cygpath_w,$(2)/$(3)) $$(call cygpath_w,$(1))
else ifdef JULIA_VAGRANT_BUILD
	@rm -r $$@
	@cp -R $$(abspath $(1)) $$@.tmp
	@mv $$@.tmp $$@
else
	@ln -sf $$(abspath $(1)) $$@
endif
endef

# many of the following targets must be = not := because the expansion of the makefile functions (and $1) shouldn't happen until later
ifeq ($(BUILD_OS), WINNT) # MSYS
spawn = $(1)
cygpath_w = $(1)
else ifneq (,$(findstring CYGWIN,$(BUILD_OS))) # Cygwin
spawn = $(1)
cygpath_w = `cygpath -w $(1)`
else
ifeq ($(OS), WINNT) # unix-to-Windows cross-compile
spawn = wine $(1)
cygpath_w = `winepath -w $(1)`
else # not Windows
spawn = $(1)
cygpath_w = $(1)
endif
endif

exec = $(shell $(call spawn,$(1)))

JULIA_BUILD_MODE := release
ifeq (,$(findstring release,$(MAKECMDGOALS)))
ifneq (,$(findstring debug,$(MAKECMDGOALS)))
JULIA_BUILD_MODE := debug
endif
endif

JULIA_EXECUTABLE_debug := $(build_bindir)/julia-debug$(EXE)
JULIA_EXECUTABLE_release := $(build_bindir)/julia$(EXE)
JULIA_EXECUTABLE := $(JULIA_EXECUTABLE_$(JULIA_BUILD_MODE))

JULIA_SYSIMG_debug := $(build_private_libdir)/sys-debug.$(SHLIB_EXT)
JULIA_SYSIMG_release := $(build_private_libdir)/sys.$(SHLIB_EXT)
JULIA_SYSIMG := $(JULIA_SYSIMG_$(JULIA_BUILD_MODE))

# Colors for make
ifndef VERBOSE
VERBOSE := 0
endif

WARNCOLOR:="\033[33;1m"
ENDCOLOR:="\033[0m"

ifeq ($(VERBOSE), 0)

QUIET_MAKE = -s

CCCOLOR:="\033[34m"
LINKCOLOR:="\033[34;1m"
PERLCOLOR:="\033[35m"
FLISPCOLOR:="\033[32m"
JULIACOLOR:="\033[32;1m"

SRCCOLOR:="\033[33m"
BINCOLOR:="\033[37;1m"
JULCOLOR:="\033[34;1m"

GOAL=$(subst ','\'',$(subst $(abspath $(JULIAHOME))/,,$(abspath $@)))

PRINT_CC = printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_ANALYZE = printf '    %b %b\n' $(CCCOLOR)ANALYZE$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_LINK = printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_PERL = printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_FLISP = printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_JULIA = printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)

else
QUIET_MAKE =
PRINT_CC = echo '$(subst ','\'',$(1))'; $(1)
PRINT_ANALYZE = echo '$(subst ','\'',$(1))'; $(1)
PRINT_LINK = echo '$(subst ','\'',$(1))'; $(1)
PRINT_PERL = echo '$(subst ','\'',$(1))'; $(1)
PRINT_FLISP = echo '$(subst ','\'',$(1))'; $(1)
PRINT_JULIA = echo '$(subst ','\'',$(1))'; $(1)

endif

define newline # a literal \n


endef

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
# (hardened against any special characters appearing in the output)
print-%:
	@echo '$*=$(subst ','\'',$(subst $(newline),\n,$($*)))'

# Literal values that are hard to use in Makefiles otherwise:
COMMA:=,
SPACE:=$(eval) $(eval)
