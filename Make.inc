# -*- mode: makefile -*-
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

# Set FORCE_ASSERTIONS to 1 to enable assertions in the C and C++ portions
# of the Julia code base. You may also want to set LLVM_ASSERTIONS to 1,
# which will enable assertions in LLVM.
# An "assert build" of Julia is a build that has both FORCE_ASSERTIONS=1
# and LLVM_ASSERTIONS=1.
FORCE_ASSERTIONS ?= 0

# Set BOOTSTRAP_DEBUG_LEVEL to 1 to enable Julia-level stacktrace during bootstrapping.
BOOTSTRAP_DEBUG_LEVEL ?= 0

# OPENBLAS build options
OPENBLAS_TARGET_ARCH:=
OPENBLAS_SYMBOLSUFFIX:=
OPENBLAS_LIBNAMESUFFIX:=
OPENBLAS_DYNAMIC_ARCH:=0
OPENBLAS_USE_THREAD:=1

# Flags for using libraries available on the system instead of building them.
# Please read the notes around usage of SYSTEM flags in README.md
# Issues resulting from use of SYSTEM versions will generally not be accepted.
USE_SYSTEM_CSL:=0
USE_SYSTEM_LLVM:=0
USE_SYSTEM_LIBUNWIND:=0
DISABLE_LIBUNWIND:=0
USE_SYSTEM_PCRE:=0
USE_SYSTEM_LIBM:=0
USE_SYSTEM_OPENLIBM:=0
UNTRUSTED_SYSTEM_LIBM:=0
USE_SYSTEM_DSFMT:=0
USE_SYSTEM_LIBBLASTRAMPOLINE:=0
USE_SYSTEM_BLAS:=0
USE_SYSTEM_LAPACK:=0
USE_SYSTEM_GMP:=0
USE_SYSTEM_MPFR:=0
USE_SYSTEM_LIBSUITESPARSE:=0
USE_SYSTEM_LIBUV:=0
USE_SYSTEM_UTF8PROC:=0
USE_SYSTEM_OPENSSL:=0
USE_SYSTEM_LIBSSH2:=0
USE_SYSTEM_NGHTTP2:=0
USE_SYSTEM_CURL:=0
USE_SYSTEM_LIBGIT2:=0
USE_SYSTEM_PATCHELF:=0
USE_SYSTEM_LIBWHICH:=0
USE_SYSTEM_ZLIB:=0
USE_SYSTEM_ZSTD:=0
USE_SYSTEM_P7ZIP:=0
USE_SYSTEM_LLD:=0

# Link to the LLVM shared library
USE_LLVM_SHLIB := 1

# Enable threading with one thread
JULIA_THREADS := 1

# Set to 1 to enable profiling with OProfile
USE_OPROFILE_JITEVENTS ?= 0

# USE_PERF_JITEVENTS, and USE_INTEL_JITEVENTS defined below since default is OS specific

# assume we don't have LIBSSP support in our compiler, will enable later if likely true
HAVE_SSP := 0

# GC debugging options
WITH_GC_VERIFY := 0
WITH_GC_DEBUG_ENV := 0

# Enable DTrace support
WITH_DTRACE := 0

# Enable ITTAPI integration
WITH_ITTAPI := 0

# Enable NVTX integration
WITH_NVTX := 0

# Enable Tracy support
WITH_TRACY := 0
WITH_TRACY_CALLSTACKS := 0

# Enable Timing Counts support
WITH_TIMING_COUNTS := 0

# Prevent picking up $ARCH from the environment variables
ARCH:=


# Literal values that are hard to use in Makefiles otherwise:
define newline # a literal \n


endef
COMMA:=,
SPACE:=$(eval) $(eval)

# define various helper macros for safe interpolation into various parsers
shell_escape='$(subst ','\'',$1)'
c_escape="$(subst ",\",$(subst \,\\,$1))"
julia_escape=$(call c_escape,$1)

# force a sane / stable configuration
export LC_ALL=C
export LANG=C

# Respect `FORCE_COLOR` environment variable: <https://force-color.org/>.
ifndef FORCE_COLOR
FORCE_COLOR := ""
endif

# Respect `NO_COLOR` environment variable: <https://no-color.org/>.
ifndef NO_COLOR
NO_COLOR := ""
endif

# When both `FORCE_COLOR` and `NO_COLOR` are defined, the former has precedence.
ifneq ($(FORCE_COLOR), "")
NO_COLOR = ""
endif

WARNCOLOR:="\033[33;1m"
ENDCOLOR:="\033[0m"

CCCOLOR:="\033[34m"
LINKCOLOR:="\033[34;1m"
PERLCOLOR:="\033[35m"
FLISPCOLOR:="\033[32m"
JULIACOLOR:="\033[32;1m"
DTRACECOLOR:="\033[32;1m"

SRCCOLOR:="\033[33m"
BINCOLOR:="\033[37;1m"
JULCOLOR:="\033[34;1m"

ifneq ($(NO_COLOR), "")
WARNCOLOR:=""
ENDCOLOR:=""

CCCOLOR:=""
LINKCOLOR:=""
PERLCOLOR:=""
FLISPCOLOR:=""
JULIACOLOR:=""
DTRACECOLOR:=""

SRCCOLOR:=""
BINCOLOR:=""
JULCOLOR:=""
endif

# We need python for things like BB triplet recognition and relative path computation.
# We don't really care about version, generally, so just find something that works:
PYTHON := "$(shell which python 2>/dev/null || which python3 2>/dev/null || which python2 2>/dev/null || echo "{python|python3|python2} not found")"
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
  $(info $(shell printf '$(JULIACOLOR)Building into $(BUILDROOT)$(ENDCOLOR)')) # use printf to expand the escape sequences
else
  BUILDROOT:=$(JULIAHOME)
endif
endif
export BUILDROOT
unexport O

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
JULIA_PATCH_VERSION := $(shell echo $(JULIA_VERSION) | cut -d'-' -f 1 | cut -d'+' -f 1 | cut -d'.' -f 3)

# libjulia's SONAME will follow the format libjulia.so.$(SOMAJOR). Before v1.0.0,
# somajor was a two-decimal value (e.g. libjulia.so.0.5). During v1.0.x - v1.9.x,
# somajor was simply the major version number (e.g. libjulia.so.1). Starting in
# v1.10.0, somajor is major.minor again (e.g. libjulia.so.1.10)
# The file itself will ultimately symlink to libjulia.so.$(SOMAJOR).$(SOMINOR)
SOMAJOR := $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION)
SOMINOR := $(JULIA_PATCH_VERSION)

# This suffix affects libjulia's SONAME and the symbol version associated with
# all of its exported symbols.
ifdef SYMBOL_VERSION_SUFFIX
SOMAJOR := $(SOMAJOR)_$(SYMBOL_VERSION_SUFFIX)
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

private_libexecdir := $(libexecdir)/julia
build_private_libexecdir := $(build_libexecdir)/julia

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
$(foreach D,libdir private_libdir datarootdir libexecdir private_libexecdir docdir sysconfdir includedir,$(eval $(call cache_rel_path,$(D),$(bindir))))
$(foreach D,build_libdir build_private_libdir,$(eval $(call cache_rel_path,$(D),$(build_bindir))))

# Save a special one: reverse_private_libdir_rel: usually just `../`, but good to be general:
reverse_private_libdir_rel_eval = $(call rel_path,$(private_libdir),$(libdir))
reverse_private_libdir_rel = $(call hit_cache,reverse_private_libdir_rel_eval)

INSTALL_F := $(JULIAHOME)/contrib/install.sh 644
INSTALL_M := $(JULIAHOME)/contrib/install.sh 755

# LLVM Options
LLVMROOT := $(build_prefix)
# Set LLVM_ASSERTIONS to 1 to enable assertions in LLVM.
LLVM_ASSERTIONS := 0
LLVM_DEBUG := 0
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG := 0
# set to 1 to get lldb (often does not work, no chance with llvm3.2 and earlier)
# see http://lldb.llvm.org/build.html for dependencies
BUILD_LLDB := 0
BUILD_LIBCXX := 0
BUILD_LLD := 1

# Options to enable Polly and its code-generation options
USE_POLLY := 0
USE_POLLY_OPENMP := 0  # Enable OpenMP code-generation
USE_POLLY_ACC := 0     # Enable GPU code-generation

# Options to use MLIR
USE_MLIR := 0

# Options to use RegionVectorizer
USE_RV := 0

# Use `ccache` for speeding up recompilation of the C/C++ part of Julia.
# Requires the `ccache` executable to be in the `PATH` environment variable.
USECCACHE := 0

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
HOSTCXX = $(CXX)
else
HOSTCC ?= gcc
HOSTCXX ?= g++
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

# Set to 1 to enable profiling with perf
ifeq ("$(OS)", "Linux")
USE_PERF_JITEVENTS ?= 1
ifeq ($(ARCH),x86_64)
USE_INTEL_JITEVENTS ?= 1
else # ARCH x86_64
USE_INTEL_JITEVENTS ?= 0
endif # ARCH x86_64
else # OS Linux
USE_PERF_JITEVENTS ?= 0
USE_INTEL_JITEVENTS ?= 0
endif # OS Linux

JULIACODEGEN := LLVM

# flag for disabling assertions
ifeq ($(FORCE_ASSERTIONS), 1)
# C++ code needs to include LLVM header with the same assertion flag as LLVM
# Use this flag to re-enable assertion in our code after all the LLVM headers are included
CXX_DISABLE_ASSERTION := -DJL_VERIFY_PASSES
DISABLE_ASSERTIONS := -DJL_VERIFY_PASSES
else
CXX_DISABLE_ASSERTION := -DJL_NDEBUG
DISABLE_ASSERTIONS := -DNDEBUG -DJL_NDEBUG
endif

# Compiler specific stuff

ifeq (default,$(origin CC))
CC := $(CROSS_COMPILE)$(CC) # attempt to add cross-compiler prefix, if the user
                            # is not overriding the default, to form target-triple-cc (which
                            # may not exist), and use that to decide what compiler the user
                            # is using for the target build (or default to gcc)
endif
CC_VERSION_STRING = $(shell $(CC) --version 2>/dev/null)
ifneq (,$(findstring clang,$(CC_VERSION_STRING)))
USECLANG := 1
USEGCC := 0
else
USECLANG := 0
USEGCC := 1
endif

FC := $(CROSS_COMPILE)gfortran

# Note: Supporting only macOS Mojave and above
ifeq ($(OS), Darwin)
APPLE_ARCH := $(shell uname -m)
ifneq ($(APPLE_ARCH),arm64)
MACOSX_VERSION_MIN := 10.14
else
MACOSX_VERSION_MIN := 11.0
endif
endif

JCFLAGS_COMMON    := -std=gnu11 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64 -Wformat -Wformat-security
JCFLAGS_CLANG     := $(JCFLAGS_COMMON)
JCFLAGS_GCC       := $(JCFLAGS_COMMON) -fno-gnu-unique

# These flags are needed to generate decent debug info
JCPPFLAGS_COMMON  := -fasynchronous-unwind-tables
JCPPFLAGS_CLANG   := $(JCPPFLAGS_COMMON) -mllvm -enable-tail-merge=0
JCPPFLAGS_GCC     := $(JCPPFLAGS_COMMON) -fno-tree-tail-merge

JCXXFLAGS_COMMON  := -pipe $(fPIC) -fno-rtti -std=c++17 -Wformat -Wformat-security
JCXXFLAGS_CLANG   := $(JCXXFLAGS_COMMON) -pedantic
JCXXFLAGS_GCC     := $(JCXXFLAGS_COMMON) -fno-gnu-unique

DEBUGFLAGS_COMMON := -O0 -DJL_DEBUG_BUILD -fstack-protector
DEBUGFLAGS_CLANG  := $(DEBUGFLAGS_COMMON) -g
DEBUGFLAGS_GCC    := $(DEBUGFLAGS_COMMON) -ggdb2

SHIPFLAGS_COMMON  := -O3
SHIPFLAGS_CLANG   := $(SHIPFLAGS_COMMON) -g
SHIPFLAGS_GCC     := $(SHIPFLAGS_COMMON) -ggdb2 -falign-functions

BOLT_LDFLAGS :=

BOLT_CFLAGS_GCC    :=
BOLT_CFLAGS_CLANG  :=

ifeq ($(OS), Darwin)
JCPPFLAGS_CLANG   += -D_LARGEFILE_SOURCE -D_DARWIN_USE_64_BIT_INODE=1
endif

ifneq ($(OS), WINNT)
# Do not enable on windows to avoid warnings from libuv.
JCXXFLAGS_GCC     += -pedantic
endif

ifeq ($(USEGCC),1)
CC         := $(CROSS_COMPILE)gcc
CXX        := $(CROSS_COMPILE)g++
JCFLAGS    := $(JCFLAGS_GCC)
JCPPFLAGS  := $(JCPPFLAGS_GCC)
JCXXFLAGS  := $(JCXXFLAGS_GCC)
DEBUGFLAGS := $(DEBUGFLAGS_GCC)
SHIPFLAGS  := $(SHIPFLAGS_GCC) $(BOLT_CFLAGS_GCC)
BOLT_CFLAGS  := $(BOLT_CFLAGS_GCC)
endif

ifeq ($(USECLANG),1)
CC         := $(CROSS_COMPILE)clang
CXX        := $(CROSS_COMPILE)clang++
JCFLAGS    := $(JCFLAGS_CLANG)
JCPPFLAGS  := $(JCPPFLAGS_CLANG)
JCXXFLAGS  := $(JCXXFLAGS_CLANG)
DEBUGFLAGS := $(DEBUGFLAGS_CLANG)
SHIPFLAGS  := $(SHIPFLAGS_CLANG) $(BOLT_CFLAGS_CLANG)
BOLT_CFLAGS  := $(BOLT_CFLAGS_CLANG)

ifeq ($(OS), Darwin)
CC += -mmacosx-version-min=$(MACOSX_VERSION_MIN)
CXX += -mmacosx-version-min=$(MACOSX_VERSION_MIN)
FC += -mmacosx-version-min=$(MACOSX_VERSION_MIN)
# export MACOSX_DEPLOYMENT_TARGET so that ld picks it up, especially for deps
export MACOSX_DEPLOYMENT_TARGET=$(MACOSX_VERSION_MIN)
endif
endif

# Conditional setting of RELRO flag for enhanced security on Linux builds.
# RELRO (Read-Only Relocations) is a security feature that marks certain sections
# of the binary as read-only to prevent exploitation techniques like
# GOT (Global Offset Table) overwriting attacks.
ifeq ($(OS),Linux)
    RELRO_FLAG := -Wl,-z,relro
else
    RELRO_FLAG :=
endif

JLDFLAGS := $(RELRO_FLAG)

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
# https://petereisentraut.blogspot.be/2011/05/ccache-and-clang.html
CC += -Qunused-arguments
CXX += -Qunused-arguments
# https://petereisentraut.blogspot.be/2011/09/ccache-and-clang-part-2.html
export CCACHE_CPP2 := yes
endif
else #USECCACHE
CC_BASE := $(shell printf "%s\n" $(call shell_escape,$(CC)) | cut -d' ' -f1)
CC_ARG := $(shell printf "%s\n" $(call shell_escape,$(CC)) | cut -s -d' ' -f2-)
CXX_BASE := $(shell printf "%s\n" $(call shell_escape,$(CXX)) | cut -d' ' -f1)
CXX_ARG := $(shell printf "%s\n" $(call shell_escape,$(CXX)) | cut -s -d' ' -f2-)
FC_BASE := $(shell printf "%s\n" $(call shell_escape,$(FC)) 2>/dev/null | cut -d' ' -f1)
FC_ARG := $(shell printf "%s\n" $(call shell_escape,$(FC)) 2>/dev/null | cut -s -d' ' -f2-)
endif

JFFLAGS := -O2 $(fPIC)
CPP := $(CC) -E
AR := $(CROSS_COMPILE)ar
AS := $(CROSS_COMPILE)as
LD := $(CROSS_COMPILE)ld
RANLIB := $(CROSS_COMPILE)ranlib
OBJCOPY := $(CROSS_COMPILE)objcopy

CPP_STDOUT := $(CPP) -P

# file extensions
ifeq ($(OS), WINNT)
  SHLIB_EXT := dll
  PATHSEP := ;
else ifeq ($(OS), Darwin)
  SHLIB_EXT := dylib
  PATHSEP := :
else
  SHLIB_EXT := so
  PATHSEP := :
endif

ifeq ($(OS),WINNT)
define versioned_libname
$(if $(2),$(1)-$(2).$(SHLIB_EXT),$(1).$(SHLIB_EXT))
endef
else ifeq ($(OS),Darwin)
define versioned_libname
$(if $(2),$(1).$(2).$(SHLIB_EXT),$(1).$(SHLIB_EXT))
endef
else
define versioned_libname
$(if $(2),$(1).$(SHLIB_EXT).$(2),$(1).$(SHLIB_EXT))
endef
endif


ifeq ($(SHLIB_EXT), so)
define SONAME_FLAGS
  -Wl,-soname=$1
endef
else
define SONAME_FLAGS
endef
endif

ifeq ($(OS),WINNT)
define IMPLIB_FLAGS
  -Wl,--out-implib,$(build_libdir)/$(notdir $1).a
endef
else
define IMPLIB_FLAGS
endef
endif

# On Windows, we want shared library files to end up in $(build_bindir), instead of $(build_libdir)
# We also don't really have a private bindir on windows right now, due to lack of RPATH.
ifeq ($(OS),WINNT)
shlibdir := $(bindir)
private_shlibdir := $(bindir)
build_shlibdir := $(build_bindir)
build_private_shlibdir := $(build_bindir)
else
shlibdir := $(libdir)
private_shlibdir := $(private_libdir)
build_shlibdir := $(build_libdir)
build_private_shlibdir := $(build_private_libdir)
endif

# If we're on windows, don't do versioned shared libraries.  If we're on OSX,
# put the version number before the .dylib.  Otherwise, put it after.
ifeq ($(OS), WINNT)
JL_MAJOR_MINOR_SHLIB_EXT := $(SHLIB_EXT)
JL_MAJOR_SHLIB_EXT := $(SHLIB_EXT)
else
ifeq ($(OS), Darwin)
JL_MAJOR_MINOR_SHLIB_EXT := $(SOMAJOR).$(SOMINOR).$(SHLIB_EXT)
JL_MAJOR_SHLIB_EXT := $(SOMAJOR).$(SHLIB_EXT)
else
JL_MAJOR_MINOR_SHLIB_EXT := $(SHLIB_EXT).$(SOMAJOR).$(SOMINOR)
JL_MAJOR_SHLIB_EXT := $(SHLIB_EXT).$(SOMAJOR)
endif
endif

ifneq ($(findstring $(OS),FreeBSD OpenBSD),)
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
_GCCMAJOR := $(shell $(FC) -dumpversion 2>/dev/null | cut -d'.' -f1)
_GCCMINOR := $(shell $(FC) -dumpversion 2>/dev/null | cut -d'.' -f2)

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

ifneq ($(CC_BASE)$(CXX_BASE),$(shell printf "%s\n" $(call shell_escape,$(CC)) | cut -d' ' -f1)$(shell printf "%s\n" $(call shell_escape,$(CXX)) | cut -d' ' -f1))
    $(error Forgot override directive on CC or CXX in Make.user? Cowardly refusing to build)
endif

ifeq ($(DARWIN_FRAMEWORK),1)
ifneq ($(OS), Darwin)
	$(error Darwin framework cannot be enabled for non-Darwin OS)
endif
endif

ifeq ($(SANITIZE),1)
SANITIZE_OPTS :=
SANITIZE_LDFLAGS :=
ifeq ($(SANITIZE_MEMORY),1)
SANITIZE_OPTS += -fsanitize=memory -fsanitize-memory-track-origins -fno-omit-frame-pointer
SANITIZE_LDFLAGS += $(SANITIZE_OPTS)
ifneq ($(findstring $(OS),Linux FreeBSD OpenBSD),)
SANITIZE_LDFLAGS += -Wl,--warn-unresolved-symbols
endif # OS Linux or FreeBSD
endif # SANITIZE_MEMORY=1
ifeq ($(SANITIZE_ADDRESS),1)
SANITIZE_OPTS += -fsanitize=address
SANITIZE_LDFLAGS += -fsanitize=address -shared-libasan
endif
ifeq ($(SANITIZE_THREAD),1)
SANITIZE_OPTS += -fsanitize=thread
SANITIZE_LDFLAGS += -fsanitize=thread
endif
ifeq ($(SANITIZE_OPTS),)
$(error SANITIZE=1, but no sanitizer selected, set either SANITIZE_MEMORY, SANITIZE_THREAD, or SANITIZE_ADDRESS)
endif
JCXXFLAGS += $(SANITIZE_OPTS)
JCFLAGS += $(SANITIZE_OPTS)
JLDFLAGS += $(SANITIZE_LDFLAGS)
endif # SANITIZE

TAR := $(shell which gtar 2>/dev/null || which tar 2>/dev/null)
TAR_TEST := $(shell $(TAR) --help 2>&1  | grep -E 'bsdtar|strip-components')
ifeq (,$(findstring components,$(TAR_TEST)))
ifneq (bsdtar,$(findstring bsdtar,$(TAR_TEST)))
$(error "please install either GNU tar or bsdtar")
endif
endif

ifeq ($(WITH_GC_VERIFY), 1)
JCXXFLAGS += -DGC_VERIFY
JCFLAGS += -DGC_VERIFY
endif

ifneq ($(JL_STACK_SIZE),)
JCXXFLAGS += -DJL_STACK_SIZE=$(JL_STACK_SIZE)
JCFLAGS += -DJL_STACK_SIZE=$(JL_STACK_SIZE)
endif


ifeq ($(WITH_GC_DEBUG_ENV), 1)
JCXXFLAGS += -DGC_DEBUG_ENV
JCFLAGS += -DGC_DEBUG_ENV
endif

# By default we use the stock GC
WITH_THIRD_PARTY_GC ?= 0

# Convert to lowercase
USE_THIRD_PARTY_GC := $(shell echo $(WITH_THIRD_PARTY_GC) | tr A-Z a-z)

# NB: When adding a new GC, make sure to add -DWITH_THIRD_PARTY_HEAP=<#NEW_GC>
# to the variables JCFLAGS and JCXXFLAGS according to the mapping (MMTK=1, #NEW_GC=2)

ifeq (${USE_THIRD_PARTY_GC},mmtk)
JCXXFLAGS += -DWITH_THIRD_PARTY_HEAP=1
JCFLAGS += -DWITH_THIRD_PARTY_HEAP=1

# Must specify a supported MMTk Plan: Immix or StickyImmix
ifeq (${MMTK_PLAN},Immix)
JCXXFLAGS += -DMMTK_PLAN_IMMIX
JCFLAGS += -DMMTK_PLAN_IMMIX
else ifeq (${MMTK_PLAN},StickyImmix)
JCXXFLAGS += -DMMTK_PLAN_STICKYIMMIX
JCFLAGS += -DMMTK_PLAN_STICKYIMMIX
else
$(error "Unsupported MMTk plan: $(MMTK_PLAN). Supported plan(s): Immix or StickyImmix.")
endif

# Do a release build on the binding by default
MMTK_BUILD ?= release

# Location of mmtk-julia binding
# (needed for api/*.h and .so file)
MMTK_JULIA_DIR ?= $(BUILDROOT)/usr/lib/mmtk_julia
MMTK_DIR = ${MMTK_JULIA_DIR}/mmtk
MMTK_API_INC = ${MMTK_DIR}/api
MMTK_LIB := -lmmtk_julia

# Must specify a supported third-party GC or use the stock GC.
# Supported third-party GCs: mmtk
else ifneq (${USE_THIRD_PARTY_GC},0)
$(error "Unsupported third-party GC: $(WITH_THIRD_PARTY_GC). Supported option(s): mmtk")
endif

ifeq ($(WITH_DTRACE), 1)
JCXXFLAGS += -DUSE_DTRACE
JCFLAGS += -DUSE_DTRACE
DTRACE := dtrace
endif

ifeq ($(WITH_ITTAPI), 1)
JCXXFLAGS += -DUSE_ITTAPI
JCFLAGS += -DUSE_ITTAPI
LIBITTAPI:=-littnotify
endif

ifeq ($(WITH_TRACY), 1)
JCXXFLAGS += -DUSE_TRACY -DTRACY_ENABLE -DTRACY_FIBERS
JCFLAGS += -DUSE_TRACY -DTRACY_ENABLE -DTRACY_FIBERS
LIBTRACYCLIENT:=-lTracyClient
endif
ifeq ($(WITH_TRACY_CALLSTACKS), 1)
JCXXFLAGS += -DTRACY_CALLSTACK=32
JCFLAGS += -DTRACY_CALLSTACK=32
LIBTRACYCLIENT:=-lTracyClient
endif

ifeq ($(WITH_TIMING_COUNTS), 1)
JCXXFLAGS += -DUSE_TIMING_COUNTS
JCFLAGS += -DUSE_TIMING_COUNTS
endif

ifeq ($(WITH_NVTX), 1)
JCXXFLAGS += -DUSE_NVTX
JCFLAGS += -DUSE_NVTX
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

# Normalize ppc64le to powerpc64le
ifeq ($(ARCH), ppc64le)
override ARCH := powerpc64le
endif

ifeq ($(ARCH),mingw32)
$(error "the mingw32 compiler you are using fails the openblas testsuite. please see the README.windows document for a replacement")
else ifeq (cygwin, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with cygwin-target compilers. set XC_HOST to i686-w64-mingw32 or x86_64-w64-mingw32 for mingw cross-compile")
else ifeq (msys, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with msys-target compilers. please see the README.windows document for instructions on setting up mingw-w64 compilers")
else ifneq (,$(findstring MSYS,$(shell uname)))
$(error "cannot build julia from a msys shell. please launch a mingw shell instead by setting MSYSTEM=MINGW64")
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


ifeq (${USE_THIRD_PARTY_GC},mmtk)
# MMTk is only available on x86_64 Linux for now
ifeq ($(OS),Linux)
MMTK_LIB_NAME := libmmtk_julia.so
else
$(error "Unsupported OS for MMTk")
endif
ifneq ($(ARCH),x86_64)
$(error "Unsupported build architecture for MMTk")
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


#If nothing is set default to native unless we are cross-compiling
ifeq ($(MARCH)$(MCPU)$(MTUNE)$(JULIA_CPU_TARGET)$(XC_HOST),)
ifeq ($(ARCH),aarch64)
# ARM recommends only setting MCPU for AArch64
MCPU=native
else ifneq (,$(findstring riscv64,$(ARCH)))
# RISC-V doesn't have a native option
$(error Building for RISC-V requires a specific MARCH to be set))
else
MARCH=native
MTUNE=native
endif
endif

# If we are running on x86 or x86_64, set certain options automatically
ifeq (1,$(ISX86))
OPENBLAS_DYNAMIC_ARCH:=1
endif

# If we are running on powerpc64le or ppc64le, set certain options automatically
ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
JCFLAGS += -fsigned-char
OPENBLAS_DYNAMIC_ARCH:=1
OPENBLAS_TARGET_ARCH:=POWER8
BINARY:=64
# GCC doesn't do -march= on ppc64le
MARCH=
endif

# Allow Clang to use CRC instructions (only applicable on AArch64)
ifneq (,$(findstring aarch64,$(ARCH)))
ifeq ($(USECLANG),1)
ifeq (,$(MARCH))
JCFLAGS += -mcrc
endif
endif
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
# on x86 make sure not to use 80 bit math when we want 64 bit math.
ifeq (32,$(BINARY))
JCFLAGS += -mfpmath=sse
endif
DIST_ARCH:=$(BINARY)
endif
ifneq (,$(findstring arm,$(ARCH)))
DIST_ARCH:=arm
endif
ifneq (,$(findstring riscv64,$(ARCH)))
DIST_ARCH:=riscv64
endif

JULIA_BINARYDIST_FILENAME := julia-$(JULIA_COMMIT)-$(DIST_OS)$(DIST_ARCH)
endif

# If we are running on ARM, set certain options automatically
ifneq (,$(findstring arm,$(ARCH)))
JCFLAGS += -fsigned-char
OPENBLAS_TARGET_ARCH:=ARMV7
BINARY:=32
endif

# If we are running on aarch64 (e.g. ARMv8 or ARM64), set certain options automatically
ifneq (,$(findstring aarch64,$(ARCH)))
OPENBLAS_DYNAMIC_ARCH:=1
OPENBLAS_TARGET_ARCH:=ARMV8
BINARY:=64
endif

# If we are running on riscv64, set certain options automatically
ifneq (,$(findstring riscv64,$(ARCH)))
OPENBLAS_DYNAMIC_ARCH:=1
BINARY:=64
endif

# Set MARCH-specific flags
ifneq ($(MARCH),)
CC += -march=$(MARCH)
CXX += -march=$(MARCH)
FC += -march=$(MARCH)
# On RISC-V, don't forward the MARCH ISA string to JULIA_CPU_TARGET,
# as it's always incompatible with LLVM's CPU target name parser.
ifeq (,$(findstring riscv64,$(ARCH)))
JULIA_CPU_TARGET ?= $(MARCH)
endif
endif

# Set MCPU-specific flags
ifneq ($(MCPU),)
CC += -mcpu=$(MCPU)
CXX += -mcpu=$(MCPU)
FC += -mcpu=$(MCPU)
JULIA_CPU_TARGET ?= $(MCPU)
endif

# Set MTUNE-specific flags
ifneq ($(MTUNE),)
CC += -mtune=$(MTUNE)
CXX += -mtune=$(MTUNE)
FC += -mtune=$(MTUNE)
JULIA_CPU_TARGET ?= $(MTUNE)
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
ifeq ($(ISX86),1)
CC += -m$(BINARY)
CXX += -m$(BINARY)
FC += -m$(BINARY)
CC_ARG += -m$(BINARY)
CXX_ARG += -m$(BINARY)
FC_ARG += -m$(BINARY)
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
LIBUNWIND:=-lunwind
ifneq ($(findstring $(OS),Darwin OpenBSD),)
JCPPFLAGS+=-DLLVMLIBUNWIND
else ifeq ($(USE_SYSTEM_LIBUNWIND), 1)
# Only for linux and freebsd since we want to use not yet released gnu libunwind features
JCFLAGS+=-DSYSTEM_LIBUNWIND
JCPPFLAGS+=-DSYSTEM_LIBUNWIND
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

# Windows builds need a little help finding the LLVM libraries for llvm-config
# use delayed expansion (= not :=) because spawn isn't defined until later
# WINEPATH is only needed for a wine-based cross compile
LLVM_CONFIG_PATH_FIX =
ifeq ($(OS),WINNT)
LLVM_CONFIG_PATH_FIX = PATH="$(build_bindir):$(PATH)" WINEPATH="$(call cygpath_w,$(build_bindir));$(WINEPATH)"
endif

ifeq ($(BUILD_OS),$(OS))
LLVM_CONFIG_HOST = $(LLVM_CONFIG_PATH_FIX) $(LLVM_CONFIG)
else
LLVM_CONFIG_HOST := $(basename $(LLVM_CONFIG))-host$(BUILD_EXE)
ifneq (exists, $(shell [ -f '$(LLVM_CONFIG_HOST)' ] && echo exists ))
# llvm-config-host does not exist (cmake build)
LLVM_CONFIG_HOST = $(LLVM_CONFIG_PATH_FIX) $(call spawn,$(LLVM_CONFIG))
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
# In the standard build system we want to patch files with `--set-rpath`, but downstream
# packagers like Spack may want to use `--add-rpath` instead, leave them the possibility to
# choose the command.
PATCHELF_SET_RPATH_ARG := --set-rpath

ifeq ($(USE_SYSTEM_LIBWHICH), 1)
LIBWHICH := libwhich
else
LIBWHICH := $(build_depsbindir)/libwhich
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
USE_BLAS64 ?= 0
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
PYTHON := $(shell which python 2>/dev/null || which python3 2>/dev/null || which python2 2>/dev/null || echo not found)
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

# Auto-detect triplet once, create different versions that we use as defaults below for each BB install target
FC_VERSION := $(shell $(FC) -dM -E - < /dev/null 2>/dev/null | grep __GNUC__ | cut -d' ' -f3)
ifeq ($(USEGCC)$(FC_VERSION),1)
FC_OR_CC_VERSION := $(shell $(CC) -dumpfullversion -dumpversion 2>/dev/null | cut -d'.' -f1)
# n.b. clang's __GNUC__ macro pretends to be gcc 4.2.1, so leave it as the empty string here if the compiler is not certain to be GCC
endif
BB_TRIPLET_LIBGFORTRAN_CXXABI := $(shell $(call invoke_python,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)) "$(FC_OR_CC_VERSION)" "$(or $(shell echo '\#include <string>' | $(CXX) $(CXXFLAGS) -x c++ -dM -E - | grep _GLIBCXX_USE_CXX11_ABI | awk '{ print $$3 }' ),1)")
BB_TRIPLET_LIBGFORTRAN := $(subst $(SPACE),-,$(filter-out cxx%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN_CXXABI))))
BB_TRIPLET_CXXABI := $(subst $(SPACE),-,$(filter-out libgfortran%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN_CXXABI))))
BB_TRIPLET := $(subst $(SPACE),-,$(filter-out cxx%,$(filter-out libgfortran%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN_CXXABI)))))

LIBGFORTRAN_VERSION := $(subst libgfortran,,$(filter libgfortran%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN))))

# CSL_NEXT_GLIBCXX_VERSION is a triple of the symbols representing support for whatever
# the next libstdc++ version would be. This is used for two things.
# 1. Whether the system libraries are new enough, if we need to use the libs bundled with CSL
# 2. To know which libstdc++ to load at runtime
# We want whichever libstdc++ library is newer, because if we don't it can cause problems.
# While what CSL bundles is quite bleeding-edge compared to what most distros ship, if someone
# tries to build an older branch of Julia, the version of CSL that ships with it may be
# relatively old. This is not a problem for code that is built in BB, but when we build Julia
# with the system compiler, that compiler uses the version of `libstdc++` that it is bundled
# with, and we can get linker errors when trying to run that `julia` executable with the
# `libstdc++` that comes from the (now old) BB-built CSL.
# To fix this, we take note when the system `libstdc++.so` is newer than whatever we
# would get from CSL (by searching for a `GLIBCXX_X.Y.Z` symbol that does not exist
# in our CSL, but would in a newer one), and default to `USE_BINARYBUILDER_CSL=0` in
# this case. This ensures that we link against a version with the symbols required.
# We also check the system libstdc++ at runtime in the cli loader library, and
# load it if it contains the version symbol that indicates that it is newer than the one
# shipped with CSL. Although we do not depend on any of the symbols, it is entirely
# possible that a user might choose to install a library which depends on symbols provided
# by a newer libstdc++. Without runtime detection, those libraries would break.
CSL_NEXT_GLIBCXX_VERSION=GLIBCXX_3\.4\.34|GLIBCXX_3\.5\.|GLIBCXX_4\.


# This is the set of projects that BinaryBuilder dependencies are hooked up for.
# Note: we explicitly _do not_ define `CSL` here, since it requires some more
# advanced techniques to decide whether it should be installed from a BB source
# or not.  See `deps/csl.mk` for more detail.
BB_PROJECTS := BLASTRAMPOLINE OPENBLAS LLVM LIBSUITESPARSE OPENLIBM GMP OPENSSL LIBSSH2 NGHTTP2 MPFR CURL LIBGIT2 PCRE LIBUV LIBUNWIND DSFMT OBJCONV ZLIB ZSTD P7ZIP LLD LIBTRACYCLIENT BOLT

ifeq (${USE_THIRD_PARTY_GC},mmtk)
BB_PROJECTS += MMTK_JULIA
endif

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


# Warn if the user tries to build something that requires `gfortran` but they don't have it installed.
ifeq ($(FC_VERSION),)
ifneq ($(USE_BINARYBUILDER_OPENBLAS)$(USE_BINARYBUILDER_LIBSUITESPARSE),11)
$(error "Attempting to build OpenBLAS or SuiteSparse without a functioning fortran compiler!")
endif
endif


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
else ifeq ($(OS), Darwin)
  RPATH := -Wl,-rpath,'@executable_path/$(build_libdir_rel)'
  RPATH_ORIGIN := -Wl,-rpath,'@loader_path/'
  RPATH_ESCAPED_ORIGIN := $(RPATH_ORIGIN)
else
  RPATH := -Wl,-rpath,'$$ORIGIN/$(build_libdir_rel)' -Wl,-rpath,'$$ORIGIN/$(build_private_libdir_rel)' -Wl,-rpath-link,$(build_shlibdir) -Wl,-z,origin -Wl,--enable-new-dtags
  RPATH_ORIGIN := -Wl,-rpath,'$$ORIGIN' -Wl,-z,origin -Wl,--enable-new-dtags
  RPATH_ESCAPED_ORIGIN := -Wl,-rpath,'\$$\$$ORIGIN' -Wl,-z,origin -Wl,-rpath-link,$(build_shlibdir) -Wl,--enable-new-dtags
endif
RPATH_LIB := $(RPATH_ORIGIN)

# --whole-archive
ifeq ($(OS), Darwin)
  WHOLE_ARCHIVE := -Xlinker -all_load
  NO_WHOLE_ARCHIVE :=
else
  WHOLE_ARCHIVE := -Wl,--whole-archive
  NO_WHOLE_ARCHIVE := -Wl,--no-whole-archive
endif

# Initialize these once, then add to them in OS-specific blocks
JLIBLDFLAGS :=

ifeq ($(OS), Linux)
OSLIBS += -Wl,--no-as-needed -ldl -lrt -lpthread -latomic -Wl,--export-dynamic,--as-needed,--no-whole-archive
# Detect if ifunc is supported
IFUNC_DETECT_SRC := 'void (*f0(void))(void) { return (void(*)(void))0L; }; void f(void) __attribute__((ifunc("f0")));'
ifeq (supported, $(shell echo $(IFUNC_DETECT_SRC) | $(CC) -Werror -x c - -S -o /dev/null > /dev/null 2>&1 && echo supported))
JCPPFLAGS += -DJULIA_HAS_IFUNC_SUPPORT=1
endif
JLDFLAGS += -Wl,-Bdynamic
OSLIBS += -Wl,--version-script=$(BUILDROOT)/src/julia.expmap
ifneq ($(SANITIZE),1)
JLDFLAGS += -Wl,-no-undefined
endif
ifeq (-Bsymbolic-functions, $(shell $(LD) --help | grep -o -e "-Bsymbolic-functions"))
JLIBLDFLAGS += -Wl,-Bsymbolic-functions
endif
ifeq (--enable-new-dtags, $(shell $(LD) --help | grep -o -e "--enable-new-dtags"))
JLIBLDFLAGS += -Wl,--enable-new-dtags
endif

# Linker doesn't detect automatically that Julia doesn't need executable stack
JLIBLDFLAGS += -Wl,-z,noexecstack
endif

ifeq ($(OS), FreeBSD)
JLDFLAGS += -Wl,-Bdynamic
OSLIBS += -lelf -lkvm -lrt -lpthread -latomic

# Tweak order of libgcc_s in DT_NEEDED,
# make it loaded first to
# prevent from linking to outdated system libs.
# See #21788
# TODO: Determine whether the condition here on AArch64 (added in #55089) should actually
# be `ifneq ($(USE_BINARYBUILDER),0)`. We vendor a correctly versioned libgcc_s when using
# BinaryBuilder which we want to link in early as noted above, but it could be the case
# that without BinaryBuilder, regardless of architecture, we need to delay linking libgcc_s
# to avoid getting the system one.
ifeq (,$(findstring aarch64,$(ARCH)))
OSLIBS += -lgcc_s
endif

OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(BUILDROOT)/src/julia.expmap \
	$(NO_WHOLE_ARCHIVE)
endif

ifeq ($(OS), OpenBSD)
JLDFLAGS += -Wl,--Bdynamic
ifneq ($(SANITIZE),1)
JLDFLAGS += -Wl,-no-undefined
endif

JLIBLDFLAGS += -Wl,-Bsymbolic-functions

OSLIBS += -Wl,--no-as-needed -lpthread -lm -lc++abi -lc
OSLIBS += -Wl,--whole-archive -lcompiler_rt -Wl,--no-whole-archive
OSLIBS += -Wl,--export-dynamic,--as-needed,--version-script=$(BUILDROOT)/src/julia.expmap
endif

ifeq ($(OS), Darwin)
SHLIB_EXT := dylib
OSLIBS += -framework CoreFoundation
WHOLE_ARCHIVE := -Xlinker -all_load
NO_WHOLE_ARCHIVE :=
HAVE_SSP := 1
JLIBLDFLAGS += -Wl,-compatibility_version,$(SOMAJOR) -Wl,-current_version,$(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)
endif

ifeq ($(OS), WINNT)
HAVE_SSP := 1
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(BUILDROOT)/src/julia.expmap \
	$(NO_WHOLE_ARCHIVE) -lpsapi -lkernel32 -lws2_32 -liphlpapi -lwinmm -ldbghelp -luserenv -lsecur32 -latomic -lole32
JLDFLAGS += -Wl,--stack,8388608 --disable-auto-import --disable-runtime-pseudo-reloc
ifeq ($(ARCH),i686)
JLDFLAGS += -Wl,--large-address-aware
endif
JCPPFLAGS += -D_WIN32_WINNT=0x0502
UNTRUSTED_SYSTEM_LIBM := 1
# Use hard links for files on windows, rather than soft links
#   https://stackoverflow.com/questions/3648819/how-to-make-a-symbolic-link-with-cygwin-in-windows-7
# Usage: $(WIN_MAKE_HARD_LINK) <source> <target>
WIN_MAKE_HARD_LINK := cp --dereference --link --force
else
WIN_MAKE_HARD_LINK := true -ignore
endif # $(OS) == WINNT

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

ifeq ($(HAVE_SSP),1)
JCPPFLAGS += -DHAVE_SSP=1
ifeq ($(USEGCC),1)
OSLIBS += -lssp
endif
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
$(error BUILD_CUSTOM_LIBCXX is currently not supported, BUILD_LIBCXX will provide LIBCXX but not link it)
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
endif

# Some special restrictions on BB usage:
ifeq ($(USE_SYSTEM_BLAS),1)
# Since the names don't line up (`BLAS` vs. `OPENBLAS`), manually gate:
USE_BINARYBUILDER_OPENBLAS := 0
# Disable BB LIBSUITESPARSE if we're using system BLAS
USE_BINARYBUILDER_LIBSUITESPARSE := 0
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
 CLANGSA_FLAGS += -isysroot $(shell xcrun --show-sdk-path -sdk macosx)
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

ifeq ($(OS), WINNT)
normalize_path = $(subst /,\,$1)
else
normalize_path = $1
endif

define symlink_target # (from, to-dir, to-name)
CLEAN_TARGETS += clean-$$(abspath $(2)/$(3))
clean-$$(abspath $(2)/$(3)):
ifeq ($(BUILD_OS), WINNT)
	-cmd //C rmdir $$(call cygpath_w,$(2)/$(3))
else
	rm -rf $$(abspath $(2)/$(3))
endif
$$(abspath $(2)/$(3)): | $$(abspath $(2))
ifeq ($$(BUILD_OS), WINNT)
	@cmd //C mklink //J $$(call cygpath_w,$(2)/$(3)) $$(call cygpath_w,$(1))
else ifneq (,$$(findstring CYGWIN,$$(BUILD_OS)))
	@cmd /C mklink /J $$(call cygpath_w,$(2)/$(3)) $$(call cygpath_w,$(1))
else ifdef JULIA_VAGRANT_BUILD
	@rm -rf $$@
	@cp -R $$(abspath $(1)) $$@.tmp
	@mv $$@.tmp $$@
else
	@ln -sf $$(abspath $(1)) $$@
endif
endef

# Overridable in Make.user
WINE ?= wine

ifeq ($(BINARY),32)
HEAPLIM := --heap-size-hint=1000M
else
HEAPLIM :=
endif

# many of the following targets must be = not := because the expansion of the makefile functions (and $1) shouldn't happen until later
ifeq ($(BUILD_OS), WINNT) # MSYS
spawn = $(1)
cygpath_w = `cygpath -w $(1)`
else ifneq (,$(findstring CYGWIN,$(BUILD_OS))) # Cygwin
spawn = $(1)
cygpath_w = `cygpath -w $(1)`
else
ifeq ($(OS), WINNT) # unix-to-Windows cross-compile
spawn = $(WINE) $(1)
cygpath_w = `$(WINE) winepath.exe -w $(1)`
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

define dep_lib_path
$(call normalize_path,$(shell $(PYTHON) $(call python_cygpath,$(JULIAHOME)/contrib/relative_path.py) $(1) $(2)))
endef

LIBJULIAINTERNAL_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/libjulia-internal.$(JL_MAJOR_SHLIB_EXT))
LIBJULIAINTERNAL_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/libjulia-internal.$(JL_MAJOR_SHLIB_EXT))

LIBJULIAINTERNAL_DEBUG_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_SHLIB_EXT))
LIBJULIAINTERNAL_DEBUG_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_SHLIB_EXT))

LIBJULIACODEGEN_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/libjulia-codegen.$(JL_MAJOR_SHLIB_EXT))
LIBJULIACODEGEN_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/libjulia-codegen.$(JL_MAJOR_SHLIB_EXT))

LIBJULIACODEGEN_DEBUG_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/libjulia-codegen-debug.$(JL_MAJOR_SHLIB_EXT))
LIBJULIACODEGEN_DEBUG_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/libjulia-codegen-debug.$(JL_MAJOR_SHLIB_EXT))

ifeq ($(OS),WINNT)
ifeq ($(BINARY),32)
LIBGCC_NAME := libgcc_s_sjlj-1.$(SHLIB_EXT)
else
LIBGCC_NAME := libgcc_s_seh-1.$(SHLIB_EXT)
endif
endif
# On macOS, libgcc_s has soversion 1.1 always on aarch64 and only for GCC 12+
# (-> libgfortran 5) on x86_64
ifeq ($(OS),Darwin)
ifeq ($(ARCH),aarch64)
LIBGCC_NAME := libgcc_s.1.1.$(SHLIB_EXT)
else
ifeq ($(LIBGFORTRAN_VERSION),5)
LIBGCC_NAME := libgcc_s.1.1.$(SHLIB_EXT)
else
LIBGCC_NAME := libgcc_s.1.$(SHLIB_EXT)
endif
endif
endif
ifneq ($(findstring $(OS),Linux FreeBSD),)
LIBGCC_NAME := libgcc_s.$(SHLIB_EXT).1
endif

# USE_SYSTEM_CSL causes it to get symlinked into build_private_shlibdir
ifeq ($(USE_SYSTEM_CSL),1)
LIBGCC_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_private_shlibdir)/$(LIBGCC_NAME))
else
LIBGCC_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/$(LIBGCC_NAME))
endif
LIBGCC_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/$(LIBGCC_NAME))

# We only bother to define this on Linux, as that's the only platform that does libstdc++ probing
# On all other platforms, the LIBSTDCXX_*_DEPLIB variables will be empty.
ifeq ($(OS),Linux)
LIBSTDCXX_NAME := libstdc++.so.6
ifeq ($(USE_SYSTEM_CSL),1)
LIBSTDCXX_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_private_shlibdir)/$(LIBSTDCXX_NAME))
else
LIBSTDCXX_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/$(LIBSTDCXX_NAME))
endif
LIBSTDCXX_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/$(LIBSTDCXX_NAME))
endif


# USE_SYSTEM_LIBM and USE_SYSTEM_OPENLIBM causes it to get symlinked into build_private_shlibdir
ifeq ($(USE_SYSTEM_LIBM),1)
LIBM_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_private_shlibdir)/$(LIBMNAME).$(SHLIB_EXT))
else ifeq ($(USE_SYSTEM_OPENLIBM),1)
LIBM_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_private_shlibdir)/$(LIBMNAME).$(SHLIB_EXT))
else
LIBM_BUILD_DEPLIB := $(call dep_lib_path,$(build_shlibdir),$(build_shlibdir)/$(LIBMNAME).$(SHLIB_EXT))
endif
LIBM_INSTALL_DEPLIB := $(call dep_lib_path,$(shlibdir),$(private_shlibdir)/$(LIBMNAME).$(SHLIB_EXT))

# We list:
#  * libgcc_s, because FreeBSD needs to load ours, not the system one.
#  * libopenlibm, because Windows has an untrustworthy libm, and we want to use ours more than theirs
#  * libstdc++, because while performing `libstdc++` probing we need to
#    know the path to the bundled `libstdc++` library.
#  * libjulia-internal, which must always come second-to-last.
#  * libjulia-codegen, which must always come last
#
# We need these four separate variables because:
#  * debug builds must link against libjuliadebug, not libjulia
#  * install time relative paths are not equal to build time relative paths (../lib vs. ../lib/julia)
# That second point will no longer be true for most deps once they are placed within Artifacts directories.
# Note that we prefix `libjulia-codegen` and `libjulia-internal` with `@` to signify to the loader that it
# should not automatically dlopen() it in its loading loop, it is "special" and should happen later.
# We do the same for `libstdc++`, and explicitly place it _after_ `libgcc_s`, and `libm` since `libstdc++`
# may depend on those libraries (e.g. when USE_SYSTEM_LIBM=1)

# Helper function to join a list with colons, then place an extra at the end.
define build_deplibs
$(subst $(SPACE),:,$(strip $(1))):
endef

LOADER_BUILD_DEP_LIBS = $(call build_deplibs, \
    $(LIBGCC_BUILD_DEPLIB) \
    $(LIBM_BUILD_DEPLIB) \
    @$(LIBSTDCXX_BUILD_DEPLIB) \
    @$(LIBJULIAINTERNAL_BUILD_DEPLIB) \
    @$(LIBJULIACODEGEN_BUILD_DEPLIB) \
)

LOADER_DEBUG_BUILD_DEP_LIBS = $(call build_deplibs, \
   $(LIBGCC_BUILD_DEPLIB) \
   $(LIBM_BUILD_DEPLIB) \
   @$(LIBSTDCXX_BUILD_DEPLIB) \
   @$(LIBJULIAINTERNAL_DEBUG_BUILD_DEPLIB) \
   @$(LIBJULIACODEGEN_DEBUG_BUILD_DEPLIB) \
)

LOADER_INSTALL_DEP_LIBS = $(call build_deplibs, \
    $(LIBGCC_INSTALL_DEPLIB) \
    $(LIBM_INSTALL_DEPLIB) \
    @$(LIBSTDCXX_INSTALL_DEPLIB) \
    @$(LIBJULIAINTERNAL_INSTALL_DEPLIB) \
    @$(LIBJULIACODEGEN_INSTALL_DEPLIB) \
)
LOADER_DEBUG_INSTALL_DEP_LIBS = $(call build_deplibs, \
    $(LIBGCC_INSTALL_DEPLIB) \
    $(LIBM_INSTALL_DEPLIB) \
    @$(LIBSTDCXX_INSTALL_DEPLIB) \
    @$(LIBJULIAINTERNAL_DEBUG_INSTALL_DEPLIB) \
    @$(LIBJULIACODEGEN_DEBUG_INSTALL_DEPLIB) \
)

# Colors for make
ifndef VERBOSE
VERBOSE := 0
endif

ifeq ($(VERBOSE), 0)

QUIET_MAKE = -s

GOAL=$(call shell_escape,$(subst $(abspath $(JULIAHOME))/,,$(abspath $@)))

PRINT_CC = printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_ANALYZE = printf '    %b %b\n' $(CCCOLOR)ANALYZE$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_LINK = printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_PERL = printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_FLISP = printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_JULIA = printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_DTRACE = printf '    %b %b\n' $(DTRACECOLOR)DTRACE$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)

else
QUIET_MAKE =
PRINT_CC = printf "%s\n" $(call shell_escape,$(1)); $(1)
PRINT_ANALYZE = printf "%s\n" $(call shell_escape,$(1)); $(1)
PRINT_LINK = printf "%s\n" $(call shell_escape,$(1)); $(1)
PRINT_PERL = printf "%s\n" $(call shell_escape,$(1)); $(1)
PRINT_FLISP = printf "%s\n" $(call shell_escape,$(1)); $(1)
PRINT_JULIA = printf "%s\n" $(call shell_escape,$(1)); $(1)
PRINT_DTRACE = printf "%s\n" $(call shell_escape,$(1)); $(1)

endif # VERBOSE

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
# (hardened against any special characters appearing in the output)
print-%:
	@printf "%s\n" $(call shell_escape,$*)=$(call shell_escape,$(subst $(newline),\n,$($*)))
