# -*- mode: makefile-gmake -*-

## Note:
##   It is generally preferable to change these options, for
##   your local machine, in a file named `Make.user` in this directory

# OPENBLAS build options
OPENBLAS_DYNAMIC_ARCH=1
OPENBLAS_USE_THREAD=1
OPENBLAS_TARGET_ARCH=

# Use libraries available on the system instead of building them
USE_SYSTEM_LLVM=0
USE_SYSTEM_LIBUNWIND=0
USE_SYSTEM_READLINE=0
USE_SYSTEM_PCRE=0
USE_SYSTEM_LIBM=0
USE_SYSTEM_OPENLIBM=0
UNTRUSTED_SYSTEM_LIBM=0
USE_SYSTEM_BLAS=0
USE_SYSTEM_LAPACK=0
USE_SYSTEM_FFTW=0
USE_SYSTEM_GMP=0
USE_SYSTEM_MPFR=0
USE_SYSTEM_ARPACK=0
USE_SYSTEM_SUITESPARSE=0
USE_SYSTEM_ZLIB=0
USE_SYSTEM_GRISU=0
USE_SYSTEM_RMATH=0
USE_SYSTEM_LIBUV=0

USE_MKL = 0
MKLLIB = $(MKLROOT)/lib/intel64

# Set to 1 to enable profiling with Intel VTune Amplifier
USE_INTEL_JITEVENTS = 0

USE_LIBCPP = 0

# we include twice to pickup user definitions better
ifeq (exists, $(shell [ -e $(JULIAHOME)/Make.user ] && echo exists ))
include $(JULIAHOME)/Make.user
endif

# disable automatic Makefile rules
.SUFFIXES:

JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)

# LLVM Options
LLVMROOT = $(BUILD)
LLVM_ASSERTIONS = 0
LLVM_DEBUG = 0
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG = 0
# set to 1 to get lldb (often does not work, no chance with llvm3.2 and earlier)
# see http://lldb.llvm.org/build.html for dependancies
BUILD_LLDB = 0

BUILD = $(JULIAHOME)/usr

# Directories where said libraries get installed to
JL_PRIVATE_LIBDIR = lib/julia
JL_LIBDIR = lib

# This used for debian packaging, to conform to library layout guidelines
ifeq ($(MULTIARCH_INSTALL), 1)
MULTIARCH = $(shell gcc -print-multiarch)
JL_PRIVATE_LIBDIR = lib/$(MULTIARCH)/julia
JL_LIBDIR = lib/$(MULTIARCH)/
endif

# Cross-compile
#XC_HOST = i686-w64-mingw32
#XC_HOST = x86_64-w64-mingw32

ifeq ($(XC_HOST),)
CROSS_COMPILE=
HOSTCC = $(CC)
else
HOSTCC = gcc
override OPENBLAS_DYNAMIC_ARCH = 1
override CROSS_COMPILE=$(XC_HOST)-
ifneq (,$(findstring mingw,$(XC_HOST)))
override OS := WINNT
export STD_LIB_PATH := $(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep programs | sed "s/^programs: =//" | xargs -d":" winepath -w | tr '\n' ';')
export STD_LIB_PATH := $(STD_LIB_PATH);$(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep libraries | sed "s/^libraries: =//" | xargs -d":" winepath -w | tr '\n' ';')
else
$(error "unknown XC_HOST variable set")
endif
endif

JLDOWNLOAD = $(JULIAHOME)/deps/jldownload

# Figure out OS and architecture
BUILD_OS := $(shell uname)
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

ifeq ($(OS), WINNT)
fPIC =
ifeq ($(BUILD_OS), WINNT)
PATH := ${PATH}:${BUILD}/${JL_LIBDIR}:${BUILD}/${JL_PRIVATE_LIBDIR}:/c/Program Files/7-zip
endif
EXE = .exe
else
fPIC = -fPIC
EXE =
endif

DEFAULT_REPL = readline
JULIAGC = MARKSWEEP
USE_COPY_STACKS = 1

# Compiler specific stuff

ifeq ($(CC), clang)
USEGCC = 0
USECLANG = 1
else
USEGCC = 1
USECLANG = 0
endif

ifeq ($(OS), Darwin)
DARWINVER = $(shell uname -r | cut -b 1-2)
OSXVER = $(shell sw_vers -productVersion | cut -b 1-4)
ifeq ($(DARWINVER), 10) # Snow Leopard specific configuration
USEGCC = 1
USECLANG = 0
OPENBLAS_TARGET_ARCH=NEHALEM
OPENBLAS_DYNAMIC_ARCH=0
USE_SYSTEM_LIBUNWIND=1
else
ifeq ($(DARWINVER),13)
USE_LIBCPP = 1
else 
USE_LIBCPP = 0
endif
USEGCC = 0
USECLANG = 1
endif
endif

FC = $(CROSS_COMPILE)gfortran

ifeq ($(USEGCC),1)
ifeq ($(USE_LIBCPP),1)
$(error USE_LIBCPP only supported with clang. Try setting USE_LIBCPP=0)
endif
CC = $(CROSS_COMPILE)gcc
CXX = $(CROSS_COMPILE)g++
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -O0 -ggdb3 -DDEBUG -fstack-protector-all
SHIPFLAGS = -O3 -falign-functions
endif

ifeq ($(USECLANG),1)
CC = $(CROSS_COMPILE)clang
CXX = $(CROSS_COMPILE)clang++
JCFLAGS = -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
ifeq ($(USE_LIBCPP),1)
CXX += -stdlib=libc++
else
CXX += -stdlib=stdlibc++
endif
DEBUGFLAGS = -O0 -g -DDEBUG -fstack-protector-all
SHIPFLAGS = -O3
ifeq ($(OS), Darwin)
ifeq ($(USE_LIBCPP), 1)
CC += -mmacosx-version-min=10.7
CXX += -mmacosx-version-min=10.7
FC += -mmacosx-version-min=10.7
else
CC += -mmacosx-version-min=10.6
CXX += -mmacosx-version-min=10.6
endif
JCFLAGS += -D_LARGEFILE_SOURCE -D_DARWIN_USE_64_BIT_INODE=1
endif
endif

JFFLAGS = -O2 $(fPIC) -mmacosx-version-min=10.8
CPP = $(CC) -E
AR := $(CROSS_COMPILE)ar
AS := $(CROSS_COMPILE)as
LD := $(CROSS_COMPILE)ld
RANLIB := $(CROSS_COMPILE)ranlib

# if not absolute, then relative to JULIA_HOME
JCFLAGS += '-DJL_SYSTEM_IMAGE_PATH="../$(JL_PRIVATE_LIBDIR)/sys.ji"'

ifeq (exists, $(shell [ -e $(JULIAHOME)/Make.user ] && echo exists ))
include $(JULIAHOME)/Make.user
endif

# ===========================================================================

BUILD_MACHINE := $(shell $(HOSTCC) -dumpmachine)
ARCH := $(shell $(CC) -dumpmachine | sed "s/\([^-]*\).*$$/\1/")
ifeq ($(ARCH),mingw32)
$(error "the mingw32 compiler you are using fails the openblas testsuite. please see the README.windows document for a replacement")
endif
ifeq ($(BUILD_OS),Darwin)
## Mac is a rather amazing 64-bit user-space on 32-bit kernel architecture, so to determine arch we detect
## the capabilities of the hardware, rather than the compiler or kernel, and make a substitution
ifeq ($(ARCH),x86_64)
ARCH = i686
else ifeq ($(ARCH),i386)
ARCH = i686
endif
ifeq ($(ARCH),i686)
ifeq ($(shell sysctl -n hw.cpu64bit_capable),1)
ARCH = x86_64
endif
BUILD_MACHINE := $(ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
endif
endif

ifeq ($(USEGCC),1)
ifneq ($(ARCH), ppc64)
  SHIPFLAGS += -momit-leaf-frame-pointer
endif
endif

ifeq ($(USE_SYSTEM_LIBUNWIND), 1)
ifneq ($(OS),Darwin)
LIBUNWIND=-lunwind-generic -lunwind
endif
else
ifeq ($(OS),Darwin)
LIBUNWIND=$(BUILD)/$(JL_LIBDIR)/libosxunwind.a
JCFLAGS+=-DLIBOSXUNWIND
else
LIBUNWIND=$(BUILD)/$(JL_LIBDIR)/libunwind-generic.a $(BUILD)/$(JL_LIBDIR)/libunwind.a
endif
endif

ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_CONFIG ?= llvm-config$(EXE)
else
LLVM_CONFIG=$(LLVMROOT)/bin/llvm-config$(EXE)
endif

ifeq ($(USE_SYSTEM_READLINE), 1)
READLINE = -lreadline
else
ifeq ($(OS),WINNT)
READLINE = $(BUILD)/lib/libreadline.a
else
READLINE = $(BUILD)/$(JL_LIBDIR)/libreadline.a
endif
endif

ifeq ($(OS),WINNT)
READLINE += $(BUILD)/lib/libhistory.a
else
READLINE += -lncurses
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre-config
else
PCRE_CONFIG = $(BUILD)/bin/pcre-config
endif

# Use 64-bit libraries by default on 64-bit architectures
ifeq ($(ARCH), x86_64)
USE_BLAS64 ?= 1
endif

ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(OS), Darwin)
USE_BLAS64 = 0
USE_SYSTEM_LAPACK = 0
LIBBLAS = -L$(BUILD)/$(JL_LIBDIR) -lgfortblas
LIBBLASNAME = libgfortblas
else
LIBBLAS ?= -lblas
LIBBLASNAME ?= libblas
endif
else
LIBBLAS = -L$(BUILD)/$(JL_LIBDIR) -lopenblas
LIBBLASNAME = libopenblas
endif

# OpenBLAS builds LAPACK as part of its build.
# We only need to build LAPACK if we are not using OpenBLAS.
ifeq ($(USE_SYSTEM_BLAS), 0)
LIBLAPACK = $(LIBBLAS)
LIBLAPACKNAME = $(LIBBLASNAME)
else
ifeq ($(USE_SYSTEM_LAPACK), 1)
LIBLAPACK = -llapack
LIBLAPACKNAME = liblapack
else
LIBLAPACK = -L$(BUILD)/$(JL_LIBDIR) -llapack $(LIBBLAS)
LIBLAPACKNAME = liblapack
endif
endif

ifeq ($(USE_SYSTEM_LIBM), 1)
LIBM = -lm
LIBMNAME = libm
else
LIBM = -lopenlibm
LIBMNAME = libopenlibm
endif

ifeq ($(USE_SYSTEM_LIBUV), 1)
  LIBUV = /usr/lib/libuv-julia.a
  LIBUV_INC = /usr/include
else
ifeq ($(OS),WINNT)
  LIBUV = $(BUILD)/lib/libuv.a
else
  LIBUV = $(BUILD)/$(JL_LIBDIR)/libuv.a
endif
  LIBUV_INC = $(JULIAHOME)/deps/libuv/include
endif

# OS specific stuff

# install_name_tool
ifeq ($(OS), Darwin)
  # must end with a / and have no trailing spaces
  INSTALL_NAME_ID_DIR = @rpath/
  INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
  INSTALL_NAME_CHANGE_CMD = install_name_tool -change
else
  INSTALL_NAME_ID_DIR =
  INSTALL_NAME_CMD = true -ignore
  INSTALL_NAME_CHANGE_CMD = true -ignore
endif

# shared library runtime paths
ifeq ($(OS), WINNT)
  RPATH =
  RPATH_ORIGIN =
else ifeq ($(OS), Darwin)
  RPATH = -Wl,-rpath,'@executable_path/../$(JL_PRIVATE_LIBDIR)' -Wl,-rpath,'@executable_path/../$(JL_LIBDIR)'
  RPATH_ORIGIN =
else
  RPATH = -Wl,-rpath,'$$ORIGIN/../$(JL_PRIVATE_LIBDIR)' -Wl,-rpath,'$$ORIGIN/../$(JL_LIBDIR)' -Wl,-z,origin
  RPATH_ORIGIN = -Wl,-rpath,'$$ORIGIN' -Wl,-z,origin
endif

# file extensions
ifeq ($(OS), WINNT)
  SHLIB_EXT = dll
  SHELL_EXT = bat
else ifeq ($(OS), Darwin)
  SHLIB_EXT = dylib
  SHELL_EXT = sh
else
  SHLIB_EXT = so
  SHELL_EXT = sh
endif

# --whole-archive
ifeq ($(OS), Darwin)
  WHOLE_ARCHIVE = -Xlinker -all_load
  NO_WHOLE_ARCHIVE =
else
  WHOLE_ARCHIVE = -Wl,--whole-archive
  NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
endif

ifeq ($(OS), Linux)
OSLIBS += -ldl -lrt -lpthread -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap -Wl,--no-whole-archive $(LIBUNWIND)
JLDFLAGS = -Wl,-Bdynamic
endif

ifeq ($(OS), FreeBSD)
JLDFLAGS = -Wl,-Bdynamic
OSLIBS += -lkvm -lrt -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(NO_WHOLE_ARCHIVE) $(LIBUNWIND)
endif

ifeq ($(OS), Darwin)
INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
INSTALL_NAME_CHANGE_CMD = install_name_tool -change
RPATH = -Wl,-rpath,'@executable_path/../$(JL_LIBDIR)' -Wl,-rpath,'@executable_path/../$(JL_PRIVATE_LIBDIR)'
SHLIB_EXT = dylib
OSLIBS += -ldl -Wl,-w -framework CoreFoundation -framework CoreServices $(LIBUNWIND)
WHOLE_ARCHIVE = -Xlinker -all_load
NO_WHOLE_ARCHIVE =
JLDFLAGS =
endif

ifeq ($(OS), WINNT)
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(JULIAHOME)/src/julia.expmap \
	$(NO_WHOLE_ARCHIVE) -lpsapi -lkernel32 -lws2_32 -liphlpapi -lwinmm -ldbghelp -lssp
JCFLAGS += -D_WIN32_WINNT=0x0600
JCXXFLAGS += -D_WIN32_WINNT=0x0600
JLDFLAGS = -Wl,--stack,8388608
ifeq ($(ARCH),i686)
JLDFLAGS += -Wl,--large-address-aware
endif
UNTRUSTED_SYSTEM_LIBM = 1
JL_PRIVATE_LIBDIR = bin
JL_LIBDIR = bin
endif

# Intel libraries

ifeq ($(USE_INTEL_LIBM), 1)
USE_SYSTEM_LIBM = 1
LIBM = -L$(MKLROOT)/../compiler/lib/intel64 -limf
LIBMNAME = libimf
endif

ifeq ($(USE_MKL), 1)
USE_SYSTEM_BLAS=1
USE_SYSTEM_LAPACK=1
LIBBLASNAME = libmkl_rt
LIBLAPACKNAME = libmkl_rt
MKL_LDFLAGS = -L$(MKLLIB) -lmkl_rt
ifneq ($(strip $(MKLLIB)),)
  ifeq ($(OS), Linux)
    RPATH_MKL = -Wl,-rpath,$(MKLLIB)
    RPATH     += $(RPATH_MKL)
    MKL_LDFLAGS += $(RPATH_MKL)
  endif
endif
LIBBLAS   = $(MKL_LDFLAGS)
LIBLAPACK = $(MKL_LDFLAGS)
endif

# ATLAS

# ATLAS must have been previously built with "make -C deps compile-atlas" (without -jN),
# or installed to usr/lib/libatlas from some another source (built as
# a shared library, for your platform, single threaded)
USE_ATLAS = 0
ATLAS_LIBDIR = $(BUILD)/$(JL_LIBDIR)
#or ATLAS_LIBDIR = /path/to/system/atlas/lib

ifeq ($(USE_ATLAS), 1)
USE_BLAS64 = 0
USE_SYSTEM_BLAS = 1
USE_SYSTEM_LAPACK = 1
LIBBLAS = -L$(ATLAS_LIBDIR) -lsatlas
LIBLAPACK = $(LIBBLAS)
LIBBLASNAME = libsatlas
LIBLAPACKNAME = $(LIBBLASNAME)
endif

# Make tricks

define dir_target
$$(subst $$(abspath $(JULIAHOME))/,,$$(abspath $(1))): $$(abspath $(1))
$$(abspath $(1)):
	@mkdir -p $$@
endef

ifeq ($(BUILD_OS), WINNT)
define mingw_to_dos
$(subst /,\\,$(subst $(shell $(2) pwd),$(shell $(2) cmd //C cd),$(abspath $(1))))
endef
endif

define symlink_target
CLEAN_TARGETS += clean-$(2)/$(1)
clean-$$(abspath $(2)/$(1)):
ifeq ($(BUILD_OS), WINNT)
	@-cmd //C rmdir $$(call mingw_to_dos,$(2)/$(1),cd $(2) &&)
else
	@-rm $$(abspath $(2)/$(1))
endif
$$(subst $$(abspath $(JULIAHOME))/,,$$(abspath $(2)/$(1))): $$(abspath $(2)/$(1))
$$(abspath $(2)/$(1)): | $$(abspath $(2))
ifeq ($(BUILD_OS), WINNT)
	@cmd //C mklink //J $$(call mingw_to_dos,$(2)/$(1),cd $(2) &&) $$(call mingw_to_dos,$(1),)
else ifdef JULIA_VAGRANT_BUILD
	@cp -R $$(abspath $(1)) $$@
else
	@ln -sf $$(abspath $(1)) $$@
endif
endef

ifeq ($(BUILD_OS), WINNT)
spawn = $(1)
else
ifeq ($(OS), WINNT)
spawn = wine $(1)
else
spawn = $(1)
endif
endif

exec = $(shell $(call spawn,$(1)))

wine_pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(shell printf %s\n '$(2)' | xargs -d";" winepath -u | tr '\n' ' '))))
pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))

JULIA_EXECUTABLE_debug = $(BUILD)/bin/julia-debug-$(DEFAULT_REPL)$(EXE)
JULIA_EXECUTABLE_release = $(BUILD)/bin/julia-$(DEFAULT_REPL)$(EXE)

ifeq ($(OS), WINNT)
JULIA_EXECUTABLE = $(JULIA_EXECUTABLE_release)
else ifdef JULIA_VAGRANT_BUILD
JULIA_EXECUTABLE = $(JULIA_EXECUTABLE_release)
else
JULIA_EXECUTABLE = $(JULIAHOME)/julia
endif

# Colors for make
ifndef VERBOSE
VERBOSE = 0
endif

ifeq ($(VERBOSE), 0)

QUIET_MAKE = -s

CCCOLOR="\033[34m"
LINKCOLOR="\033[34;1m"
PERLCOLOR="\033[35m"
FLISPCOLOR="\033[32m"
JULIACOLOR="\033[32;1m"

SRCCOLOR="\033[33m"
BINCOLOR="\033[37;1m"
JULCOLOR="\033[34;1m"
ENDCOLOR="\033[0m"

GOAL=$(subst $(abspath $(JULIAHOME))/,,$(abspath $@))

PRINT_CC = printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_LINK = printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_PERL = printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_FLISP = printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_JULIA = printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)

else
QUIET_MAKE =
PRINT_CC = echo $(1); $(1)
PRINT_LINK = echo $(1); $(1)
PRINT_PERL = echo $(1); $(1)
PRINT_FLISP = echo $(1); $(1)
PRINT_JULIA = echo $(1); $(1)

endif
