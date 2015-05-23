# -*- mode: makefile-gmake -*-

## Note:
##   It is generally preferable to change these options, for
##   your local machine, in a file named `Make.user` in this directory

# OPENBLAS build options
OPENBLAS_TARGET_ARCH=
OPENBLAS_SYMBOLSUFFIX=

# If OPENBLAS_TARGET_ARCH is set, we default to disabling OPENBLAS_DYNAMIC_ARCH
ifneq ($(OPENBLAS_TARGET_ARCH),)
OPENBLAS_DYNAMIC_ARCH=0
else
OPENBLAS_DYNAMIC_ARCH=1
endif
OPENBLAS_USE_THREAD=1

# Use libraries available on the system instead of building them
USE_SYSTEM_LLVM=0
USE_SYSTEM_LIBUNWIND=0
USE_SYSTEM_PCRE=0
USE_SYSTEM_LIBM=0
USE_SYSTEM_OPENLIBM=0
UNTRUSTED_SYSTEM_LIBM=0
USE_SYSTEM_OPENSPECFUN=0
USE_SYSTEM_DSFMT=0
USE_SYSTEM_BLAS=0
USE_SYSTEM_LAPACK=0
USE_SYSTEM_FFTW=0
USE_SYSTEM_GMP=0
USE_SYSTEM_MPFR=0
USE_SYSTEM_ARPACK=0
USE_SYSTEM_SUITESPARSE=0
USE_SYSTEM_RMATH=0
USE_SYSTEM_LIBUV=0
USE_SYSTEM_UTF8PROC=0
USE_SYSTEM_LIBGIT2=0
USE_SYSTEM_PATCHELF=0

# Link to the LLVM shared library
USE_LLVM_SHLIB = 0

## Settings for various Intel tools
# Set to 1 to use MKL
USE_INTEL_MKL ?= 0
# Set to 1 to use MKL FFT
USE_INTEL_MKL_FFT ?= 0
# Set to 1 to use Intel LIBM
USE_INTEL_LIBM ?= 0
# Set to 1 to enable profiling with Intel VTune Amplifier
USE_INTEL_JITEVENTS ?= 0
# Set to 1 to use Intel C, C++, and FORTRAN compilers
USEICC  ?= 0
USEIFC  ?= 0

ifeq ($(USE_MKL), 1)
$(warning "The julia make variable USE_MKL has been renamed to USE_INTEL_MKL")
USE_INTEL_MKL = 1
endif

# libc++ is standard on OS X 10.9, but not for earlier releases
USE_LIBCPP = 0

# assume we don't have LIBSSP support in our compiler, will enable later if likely true
HAVE_SSP = 0

# Prevent picking up $ARCH from the environment variables
ARCH=

# we include twice to pickup user definitions better
ifeq (exists, $(shell [ -e $(JULIAHOME)/Make.user ] && echo exists ))
include $(JULIAHOME)/Make.user
endif

# disable automatic Makefile rules
.SUFFIXES:

# find out if git repository is available
ifeq ($(shell [ -e $(JULIAHOME)/.git ] && echo true || echo "Warning: git information unavailable; versioning information limited" >&2), true)
NO_GIT = 0
else
NO_GIT = 1
endif

JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
ifneq ($(NO_GIT), 1)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)
else
JULIA_COMMIT = $(JULIA_VERSION)
endif

# Whether to use GPL libraries or not.
USE_GPL_LIBS ?= 1

# Directories where said libraries get installed to
prefix ?= $(abspath julia-$(JULIA_COMMIT))
bindir = $(prefix)/bin
libdir = $(prefix)/lib
private_libdir = $(libdir)/julia
libexecdir = $(prefix)/libexec
datarootdir = $(prefix)/share
docdir = $(datarootdir)/doc/julia
mandir = $(datarootdir)/man
man1dir = $(mandir)/man1
includedir = $(prefix)/include
sysconfdir = $(prefix)/etc

# Directories where things get built into
build_prefix = $(JULIAHOME)/usr
build_staging = $(build_prefix)-staging
build_bindir = $(build_prefix)/bin
build_libdir = $(build_prefix)/lib
build_private_libdir = $(build_prefix)/lib/julia
build_libexecdir = $(build_prefix)/libexec
build_datarootdir = $(build_prefix)/share
build_docdir = $(build_datarootdir)/doc/julia
build_mandir = $(build_datarootdir)/man
build_man1dir = $(build_mandir)/man1
build_includedir = $(build_prefix)/include
build_sysconfdir = $(build_prefix)/etc


# Calculate relative paths to libdir, private_libdir, datarootdir, and sysconfdir
build_libdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(build_bindir) $(build_libdir))
libdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(bindir) $(libdir))
build_private_libdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(build_bindir) $(build_private_libdir))
private_libdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(bindir) $(private_libdir))
datarootdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(bindir) $(datarootdir))
docdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(bindir) $(docdir))
sysconfdir_rel = $(shell $(JULIAHOME)/contrib/relative_path.sh $(bindir) $(sysconfdir))

INSTALL_F = $(JULIAHOME)/contrib/install.sh 644
INSTALL_M = $(JULIAHOME)/contrib/install.sh 755

# This used for debian packaging, to conform to library layout guidelines
ifeq ($(MULTIARCH_INSTALL), 1)
MULTIARCH = $(shell gcc -print-multiarch)
private_libdir = $(prefix)/lib/$(MULTIARCH)/julia
libdir = $(prefix)/lib/$(MULTIARCH)/
endif


# LLVM Options
LLVMROOT = $(build_prefix)
LLVM_ASSERTIONS = 0
LLVM_DEBUG = 0
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG = 0
# set to 1 to get lldb (often does not work, no chance with llvm3.2 and earlier)
# see http://lldb.llvm.org/build.html for dependencies
BUILD_LLDB = 0

# Cross-compile
#XC_HOST = i686-w64-mingw32
#XC_HOST = x86_64-w64-mingw32

# Path to cmake (override in Make.user if needed)
CMAKE ?= cmake

# Figure out OS and architecture
BUILD_OS := $(shell uname)

ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
XC_HOST ?= $(shell uname -m)-w64-mingw32
endif

ifeq ($(XC_HOST),)
CROSS_COMPILE=
HOSTCC = $(CC)
else
HOSTCC = gcc
override OPENBLAS_DYNAMIC_ARCH = 1
override CROSS_COMPILE=$(XC_HOST)-
ifneq (,$(findstring mingw,$(XC_HOST)))
override OS := WINNT
ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
export STD_LIB_PATH := $(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep programs | sed -e "s/^programs: =//" -e "s!/lib/!/bin/!g")
export STD_LIB_PATH := $(STD_LIB_PATH):$(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep libraries | sed -e "s/^libraries: =//" -e "s!/lib/!/bin/!g")
else
export STD_LIB_PATH := $(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep programs | sed "s/^programs: =//" | xargs -d":" winepath -w | tr '\n' ';')
export STD_LIB_PATH := $(STD_LIB_PATH);$(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep libraries | sed "s/^libraries: =//" | xargs -d":" winepath -w | tr '\n' ';')
endif
else
$(error "unknown XC_HOST variable set")
endif
endif

JLDOWNLOAD = $(JULIAHOME)/deps/jldownload
JLCHECKSUM = $(JULIAHOME)/deps/jlchecksum

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

ifeq ($(OS), WINNT)
fPIC =
ifeq ($(BUILD_OS), WINNT)
PATH := $(PATH):$(build_libdir):$(build_private_libdir):/c/Program Files/7-zip
endif
EXE = .exe
else
fPIC = -fPIC
EXE =
endif

JULIAGC = MARKSWEEP
USE_COPY_STACKS = 1

# Compiler specific stuff

ifeq ($(USEMSVC), 1)
USEGCC = 0
USECLANG = 0
USEICC = 0
else
ifeq ($(USECLANG), 1)
USEGCC = 0
USEICC = 0
else
ifeq ($(USEICC), 1)
USEGCC = 0
USECLANG = 0
else  # default to gcc
USEGCC = 1
USECLANG = 0
USEICC = 0
endif
endif
endif

ifeq ($(USEIFC), 1)
FC = ifort
else
FC = $(CROSS_COMPILE)gfortran
endif

STDLIBCPP_FLAG =

ifeq ($(OS), Darwin)
DARWINVER := $(shell uname -r | cut -b 1-2)
DARWINVER_GTE13 := $(shell expr `uname -r | cut -b 1-2` \>= 13)
OSXVER := $(shell sw_vers -productVersion | cut -b 1-4)
ifeq ($(DARWINVER), 10) # Snow Leopard specific configuration
USEGCC = 1
USECLANG = 0
OPENBLAS_TARGET_ARCH=NEHALEM
OPENBLAS_DYNAMIC_ARCH=0
USE_SYSTEM_LIBUNWIND=1
else
ifeq ($(DARWINVER_GTE13),1)
USE_LIBCPP = 1
STDLIBCPP_FLAG = -stdlib=libstdc++
else
USE_LIBCPP = 0
endif
USEGCC = 0
USECLANG = 1
endif
endif

ifeq ($(USEGCC),1)
ifeq ($(USE_LIBCPP),1)
$(error USE_LIBCPP only supported with clang. Try setting USE_LIBCPP=0)
endif
ifeq ($(SANITIZE),1)
$(error Address Sanitizer only supported with clang. Try setting SANITIZE=0)
endif
CC = $(CROSS_COMPILE)gcc
CXX = $(CROSS_COMPILE)g++
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCPPFLAGS =
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -O0 -ggdb3 -DJL_DEBUG_BUILD -fstack-protector-all
SHIPFLAGS = -O3 -ggdb3 -falign-functions
endif

ifeq ($(USECLANG),1)
CC = $(CROSS_COMPILE)clang
CXX = $(CROSS_COMPILE)clang++
JCFLAGS = -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCPPFLAGS =
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -O0 -g -DJL_DEBUG_BUILD -fstack-protector-all
SHIPFLAGS = -O3 -g
ifeq ($(OS), Darwin)
ifeq ($(USE_LIBCPP), 1)
CC += -stdlib=libc++ -mmacosx-version-min=10.7
CXX += -stdlib=libc++ -mmacosx-version-min=10.7
FC += -mmacosx-version-min=10.7
else
CC += $(STDLIBCPP_FLAG) -mmacosx-version-min=10.6
CXX += $(STDLIBCPP_FLAG) -mmacosx-version-min=10.6
endif
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
CC  = icc
CXX = icpc
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64 -fp-model precise -fp-model except -no-ftz
JCPPFLAGS =
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -O0 -g -DJL_DEBUG_BUILD -fstack-protector-all
SHIPFLAGS = -O3 -g -falign-functions
endif

ifeq ($(USECCACHE), 1)
# expand CC and CXX at declaration time because we will redefine them
CC_ARG   := $(CC)         # Expand CC and CXX here already because we want
CXX_ARG  := $(CXX)        # the original definition and not the ccache version.
CC_FULL  := ccache $(CC)  # Expand CC and CXX here already to avoid recursive
CXX_FULL := ccache $(CXX) # referencing.
CC        = $(CC_FULL)    # Add an extra indirection to make CC/CXX non-simple
CXX       = $(CXX_FULL)   # vars (because of how -m$(BINARY) is added later on).
CC_BASE   = ccache
CXX_BASE  = ccache
ifeq ($(USECLANG),1)
# ccache and Clang don't do well together
# http://petereisentraut.blogspot.be/2011/05/ccache-and-clang.html
CC += -Qunused-arguments -fcolor-diagnostics
CXX += -Qunused-arguments -fcolor-diagnostics
# http://petereisentraut.blogspot.be/2011/09/ccache-and-clang-part-2.html
export CCACHE_CPP2 := yes
endif
else #USECCACHE
CC_BASE = $(shell echo $(CC) | cut -d' ' -f1)
CXX_BASE = $(shell echo $(CXX) | cut -d' ' -f1)
endif

ifeq ($(LLVM_VER),svn)
JCXXFLAGS += -std=c++11
endif

JFFLAGS = -O2 $(fPIC)
JF2CFLAGS = -ff2c -fno-second-underscore
ifneq ($(USEMSVC),1)
CPP = $(CC) -E
AR := $(CROSS_COMPILE)ar
AS := $(CROSS_COMPILE)as
LD := $(CROSS_COMPILE)ld
else #USEMSVC
CPP = $(CC) -EP
AR := lib
ifeq ($(ARCH),x86_64)
AS := ml64
else
AS := ml
endif #ARCH
LD := link
endif #USEMSVC
RANLIB := $(CROSS_COMPILE)ranlib


# if not absolute, then relative to the directory of the julia executable
JCPPFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys.ji\""

# On Windows, we want shared library files to end up in $(build_bindir), instead of $(build_libdir)
ifeq ($(OS),WINNT)
build_shlibdir = $(build_bindir)
else
build_shlibdir = $(build_libdir)
endif

# If we are running on ARM, set certain options automatically
ifneq (,$(findstring arm,$(ARCH)))
LLVM_ASSERTIONS=1
LLVM_FLAGS+="--with-cpu=cortex-a9 --with-float=hard --with-abi=aapcs-vfp --enable-assertions"

OPENBLAS_DYNAMIC_ARCH=0
OPENBLAS_TARGET_ARCH=ARMV7
USE_BLAS64=0

LLVM_VER=3.6.0

USE_SYSTEM_LIBM=1
USE_SYSTEM_FFTW=1
USE_SYSTEM_GMP=1
USE_SYSTEM_MPFR=1

JCFLAGS += -fsigned-char
endif


ifeq (exists, $(shell [ -e $(JULIAHOME)/Make.user ] && echo exists ))
include $(JULIAHOME)/Make.user
endif

ifeq ($(SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
SANITIZE_OPTS = -fsanitize=memory -fsanitize-memory-track-origins -fno-omit-frame-pointer
else
SANITIZE_OPTS = -fsanitize=address -mllvm -asan-stack=0
endif
JCXXFLAGS += $(SANITIZE_OPTS)
JCFLAGS += $(SANITIZE_OPTS)
LDFLAGS += $(SANITIZE_OPTS)
DEPS_CFLAGS += $(SANITIZE_OPTS)
DEPS_CXXFLAGS += $(SANITIZE_OPTS)
endif

TAR=`which gtar 2>/dev/null || which tar 2>/dev/null`
TAR_TEST := $(shell $(TAR) --help 2>&1  | egrep 'bsdtar|strip-components')
ifeq (,$(findstring components,$(TAR_TEST)))
ifneq (bsdtar,$(findstring bsdtar,$(TAR_TEST)))
$(error "please install either GNU tar or bsdtar")
endif
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
ifeq ($(ARCH),)
override ARCH := $(shell $(CC) -dumpmachine | sed "s/\([^-]*\).*$$/\1/")
ifeq ($(ARCH),mingw32)
$(error "the mingw32 compiler you are using fails the openblas testsuite. please see the README.windows document for a replacement")
else ifeq (cygwin, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with cygwin-target compilers. set XC_HOST to i686-w64-mingw32 or x86_64-w64-mingw32 for mingw cross-compile")
else ifeq (msys, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with msys-target compilers. please see the README.windows document for instructions on setting up mingw-w64 compilers")
endif
ifeq ($(BUILD_OS),Darwin)
## Mac is a rather amazing 64-bit user-space on 32-bit kernel architecture, so to determine arch we detect
## the capabilities of the hardware, rather than the compiler or kernel, and make a substitution
ifeq ($(ARCH),x86_64)
override ARCH = i686
else ifeq ($(ARCH),i386)
override ARCH = i686
endif
ifeq ($(ARCH),i686)
ifeq ($(shell sysctl -n hw.cpu64bit_capable),1)
override ARCH = x86_64
endif
BUILD_MACHINE := $(ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
endif
endif
else
XC_HOST := $(ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
MARCH = $(ARCH)
endif

ifneq ($(MARCH),)
CC += -march=$(MARCH)
CXX += -march=$(MARCH)
FC += -march=$(MARCH)
JULIA_CPU_TARGET ?= $(MARCH)
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
JCPPFLAGS += -DJULIA_TARGET_ARCH=$(JULIA_CPU_TARGET)

# We map amd64 to x86_64 for compatibility with systems that identify 64-bit systems as such
ifeq ($(ARCH),amd64)
override ARCH = x86_64
endif

ifeq ($(ARCH),i386)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i387)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i486)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i586)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i686)
BINARY=32
ISX86=1
else ifeq ($(ARCH),x86_64)
BINARY=64
ISX86=1
else ifneq (,$(findstring arm,$(ARCH)))
ISX86=0
else ifneq (,$(findstring powerpc,$(ARCH)))
ISX86=0
else ifneq (,$(findstring ppc,$(ARCH)))
ISX86=0
else
$(error "unknown word-size for arch: $(ARCH)")
endif

# Set some ARCH-specific flags
ifneq ($(USEICC),1)
ifeq ($(ISX86),1)
CC += -m$(BINARY)
CXX += -m$(BINARY)
FC += -m$(BINARY)
CC_ARG += -m$(BINARY)
CXX_ARG += -m$(BINARY)
endif
endif

ifeq ($(USEGCC),1)
ifeq ($(ISX86),1)
  SHIPFLAGS += -momit-leaf-frame-pointer
endif
endif

ifeq ($(OS),WINNT)
LIBUNWIND=
else
ifeq ($(USE_SYSTEM_LIBUNWIND), 1)
ifneq ($(OS),Darwin)
LIBUNWIND=-lunwind-generic -lunwind
endif
else
ifeq ($(OS),Darwin)
LIBUNWIND=$(build_libdir)/libosxunwind.a
JCPPFLAGS+=-DLIBOSXUNWIND
else
LIBUNWIND=$(build_libdir)/libunwind-generic.a $(build_libdir)/libunwind.a
endif
endif
endif

ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_CONFIG ?= llvm-config$(EXE)
LLVM_LLC ?= llc$(EXE)
JCPPFLAGS+=-DSYSTEM_LLVM
LLVM_VER = $(shell $(LLVM_CONFIG) --version)
else
LLVM_CONFIG=$(build_bindir)/llvm-config$(EXE)
LLVM_LLC=$(build_bindir)/llc$(EXE)
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre-config
else
PCRE_CONFIG = $(build_bindir)/pcre-config
endif

# Use 64-bit libraries by default on 64-bit architectures
ifeq ($(BINARY), 64)
USE_BLAS64 ?= 1
endif

ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(OS), Darwin)
USE_BLAS64 = 0
USE_SYSTEM_LAPACK = 0
LIBBLAS = -L$(build_libdir) -lgfortblas
LIBBLASNAME = libgfortblas
else
LIBBLAS ?= -lblas
LIBBLASNAME ?= libblas
endif
else
LIBBLAS = -L$(build_shlibdir) -lopenblas
LIBBLASNAME = libopenblas
endif

# OpenBLAS builds LAPACK as part of its build.
# We only need to build LAPACK if we are not using OpenBLAS.
ifeq ($(USE_SYSTEM_BLAS), 0)
LIBLAPACK = $(LIBBLAS)
LIBLAPACKNAME = $(LIBBLASNAME)
else
ifeq ($(USE_SYSTEM_LAPACK), 1)
LIBLAPACK ?= -llapack
LIBLAPACKNAME ?= liblapack
else
LIBLAPACK = -L$(build_shlibdir) -llapack $(LIBBLAS)
LIBLAPACKNAME = liblapack
endif
endif

ifeq ($(OS), WINNT)
LIBFFTWNAME = libfftw3
LIBFFTWFNAME = libfftw3f
else
LIBFFTWNAME = libfftw3_threads
LIBFFTWFNAME = libfftw3f_threads
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
  LIBUV = $(build_libdir)/libuv.a
  LIBUV_INC = $(build_includedir)
endif

ifeq ($(USE_SYSTEM_UTF8PROC), 1)
  LIBUTF8PROC = -lutf8proc
  UTF8PROC_INC = /usr/include
else
  LIBUTF8PROC = $(build_libdir)/libutf8proc.a
  UTF8PROC_INC = $(build_includedir)
endif

# OS specific stuff

# install_name_tool
ifeq ($(OS), Darwin)
  # must end with a / and have no trailing spaces
  INSTALL_NAME_ID_DIR = @rpath/
  INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
  INSTALL_NAME_CHANGE_CMD = install_name_tool -change
ifeq ($(shell test `dsymutil -v | cut -d\- -f2 | cut -d. -f1` -gt 102 && echo yes), yes)
  DSYMUTIL = dsymutil
else
  DSYMUTIL = true -ignore
endif
else
  INSTALL_NAME_ID_DIR =
  INSTALL_NAME_CMD = true -ignore
  INSTALL_NAME_CHANGE_CMD = true -ignore
  DSYMUTIL = true -ignore
endif

# shared library runtime paths
ifeq ($(OS), WINNT)
  RPATH =
  RPATH_ORIGIN =
else ifeq ($(OS), Darwin)
  RPATH = -Wl,-rpath,'@executable_path/$(build_private_libdir_rel)' -Wl,-rpath,'@executable_path/$(build_libdir_rel)'
  RPATH_ORIGIN = -Wl,-rpath,'@loader_path/'
else
  RPATH = -Wl,-rpath,'$$ORIGIN/$(build_private_libdir_rel)' -Wl,-rpath,'$$ORIGIN/$(build_libdir_rel)' -Wl,-z,origin
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
else ifneq ($(USEMSVC), 1)
  WHOLE_ARCHIVE = -Wl,--whole-archive
  NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
endif

ifeq ($(OS), Linux)
OSLIBS += -ldl -lrt -lpthread -Wl,--export-dynamic -Wl,--no-whole-archive $(LIBUNWIND)
ifneq ($(SANITIZE),1)
ifneq ($(SANITIZE_MEMORY),1)
ifneq ($(LLVM_SANITIZE),1)
OSLIBS += -Wl,--version-script=$(JULIAHOME)/src/julia.expmap
endif
endif
endif
JLDFLAGS = -Wl,-Bdynamic
ifeq (-Bsymbolic-functions, $(shell $(LD) --help | grep -o -e "-Bsymbolic-functions"))
JLIBLDFLAGS = -Wl,-Bsymbolic-functions
else
JLIBLDFLAGS =
endif
else #Linux
JLIBLDFLAGS =
endif

ifeq ($(OS), FreeBSD)
JLDFLAGS = -Wl,-Bdynamic
OSLIBS += -lkvm -lrt -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(NO_WHOLE_ARCHIVE) $(LIBUNWIND)
endif

ifeq ($(OS), Darwin)
INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
INSTALL_NAME_CHANGE_CMD = install_name_tool -change
SHLIB_EXT = dylib
OSLIBS += -ldl -Wl,-w -framework CoreFoundation -framework CoreServices $(LIBUNWIND)
WHOLE_ARCHIVE = -Xlinker -all_load
NO_WHOLE_ARCHIVE =
JLDFLAGS =
HAVE_SSP = 1
endif

ifeq ($(OS), WINNT)
ifneq ($(USEMSVC), 1)
HAVE_SSP = 1
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(JULIAHOME)/src/julia.expmap \
	$(NO_WHOLE_ARCHIVE) -lpsapi -lkernel32 -lws2_32 -liphlpapi -lwinmm -ldbghelp
JLDFLAGS = -Wl,--stack,8388608
ifeq ($(ARCH),i686)
JLDFLAGS += -Wl,--large-address-aware
endif
else #USEMSVC
OSLIBS += kernel32.lib ws2_32.lib psapi.lib advapi32.lib iphlpapi.lib shell32.lib winmm.lib
JLDFLAGS = -stack:8388608
endif
JCPPFLAGS += -D_WIN32_WINNT=0x0502
UNTRUSTED_SYSTEM_LIBM = 1
endif

# Intel VTune Amplifier
ifeq ($(USE_INTEL_JITEVENTS), 1)
JCPPFLAGS += -DJL_USE_INTEL_JITEVENTS
endif

# Intel libraries

ifeq ($(USE_INTEL_LIBM), 1)
USE_SYSTEM_LIBM = 1
LIBM = -L$(MKLROOT)/../compiler/lib/intel64 -limf
LIBMNAME = libimf
endif

ifeq ($(USE_INTEL_MKL), 1)
ifeq ($(USE_BLAS64), 1)
export MKL_INTERFACE_LAYER := ILP64
MKLLIB = $(MKLROOT)/lib/intel64
else
MKLLIB = $(MKLROOT)/lib/ia32
endif
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

ifeq ($(USE_INTEL_MKL_FFT), 1)
USE_SYSTEM_FFTW = 1
LIBFFTWNAME = libmkl_rt
LIBFFTWFNAME = libmkl_rt
endif

ifeq ($(HAVE_SSP),1)
JCPPFLAGS += -DHAVE_SSP=1
ifeq ($(USEGCC),1)
OSLIBS += -lssp
endif
endif

# ATLAS

# ATLAS must have been previously built with "make -C deps compile-atlas" (without -jN),
# or installed to usr/lib/libatlas from some another source (built as
# a shared library, for your platform, single threaded)
USE_ATLAS = 0
ATLAS_LIBDIR = $(build_libdir)
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

# Renaming OpenBLAS symbols, see #4923 and #8734
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(USE_BLAS64), 1)
OPENBLAS_SYMBOLSUFFIX = 64_
endif
endif

# Custom libcxx
ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LDFLAGS += -L$(build_libdir) -lc++abi
CXXLDFLAGS += -L$(build_libdir) -lc++abi -stdlib=libc++ -lc++
CPPFLAGS += -I$(build_includedir)/c++/v1
CUSTOM_LD_LIBRARY_PATH = LD_LIBRARY_PATH="$(build_libdir)"
ifeq ($(USEICC),1)
CXXFLAGS += -cxxlib-nostd -static-intel
CLDFLAGS += -static-intel
LDFLAGS += -cxxlib-nostd -static-intel
endif
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
else ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
	@cmd /C mklink /J $$(call cygpath_w,$(2)/$(1)) $$(call cygpath_w,$(1))
else ifdef JULIA_VAGRANT_BUILD
	@cp -R $$(abspath $(1)) $$@
else
	@ln -sf $$(abspath $(1)) $$@
endif
endef

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

ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
wine_pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))
else
wine_pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(shell printf %s\n '$(2)' | xargs -d";" winepath -u | tr '\n' ' '))))
endif
pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))

JULIA_BUILD_MODE = release
ifeq (,$(findstring release,$(MAKECMDGOALS)))
ifneq (,$(findstring debug,$(MAKECMDGOALS)))
JULIA_BUILD_MODE = debug
endif
endif

JULIA_EXECUTABLE_debug = $(build_bindir)/julia-debug$(EXE)
JULIA_EXECUTABLE_release = $(build_bindir)/julia$(EXE)
JULIA_EXECUTABLE = $(JULIA_EXECUTABLE_$(JULIA_BUILD_MODE))

# Colors for make
ifndef VERBOSE
VERBOSE = 0
endif

WARNCOLOR="\033[33;1m"

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

GOAL=$(subst ','\'',$(subst $(abspath $(JULIAHOME))/,,$(abspath $@)))

PRINT_CC = printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_LINK = printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_PERL = printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_FLISP = printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_JULIA = printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)

else
QUIET_MAKE =
PRINT_CC = echo '$(subst ','\'',$(1))'; $(1)
PRINT_LINK = echo '$(subst ','\'',$(1))'; $(1)
PRINT_PERL = echo '$(subst ','\'',$(1))'; $(1)
PRINT_FLISP = echo '$(subst ','\'',$(1))'; $(1)
PRINT_JULIA = echo '$(subst ','\'',$(1))'; $(1)

endif
