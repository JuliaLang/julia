# -*- mode: makefile-gmake -*-

# include twice to pickup user definitions
ifeq (exists, $(shell [ -e $(JULIAHOME)/Make.user ] && echo exists ))
include $(JULIAHOME)/Make.user 
endif

.SUFFIXES:

JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)

LLVMROOT = $(BUILD)
BUILD = $(JULIAHOME)/usr
# Directories where said libraries get installed to
JL_PRIVATE_LIBDIR = lib/julia
JL_LIBDIR = lib

# If we're on debian, default to arch-dependent library dirs
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
else
ifeq ($(XC_HOST),i686-w64-mingw32)
override OS = WINNT
override ARCH = i686
override OPENBLAS_DYNAMIC_ARCH = 1
override CROSS_COMPILE=$(XC_HOST)-
export WINEPATH := '$(BUILD)/lib/julia;$(BUILD)/lib;$(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep programs | sed "s/^programs: =//" | sed "s/:/;/g");C:\\MinGW\\bin;C:\\MinGW\\msys\\1.0\\bin;$(BUILD)/Git/bin'
else ifeq ($(XC_HOST),x86_64-w64-mingw32)
override OS = WINNT
override ARCH = x86_64
override OPENBLAS_DYNAMIC_ARCH = 1
override CROSS_COMPILE=$(XC_HOST)-
export WINEPATH := '$(BUILD)/lib/julia;$(BUILD)/lib;$(shell $(CROSS_COMPILE)gcc -print-search-dirs | grep programs | sed "s/^programs: =//" | sed "s/:/;/g");C:\\MinGW\\bin;C:\\MinGW\\msys\\1.0\\bin;$(BUILD)/Git/bin'
else
$(error "unknown XC_HOST variable set")
endif
endif

# Figure out OS and architecture
BUILD_OS = $(shell uname)
OS = $(BUILD_OS)
ARCH = $(shell uname -m)

# Use 64-bit libraries by default on 64-bit architectures
ifeq ($(ARCH), x86_64)
USE_LIB64 = 1
endif

USE_MKL = 0
MKLLIB = /path/to/mkl/lib/intel64

# ATLAS must have been previously built with "make -C deps compile-atlas" (without -jN),
# or installed to usr/lib/libatlas from some another source (built as
# a shared library, for your platform, single threaded)
USE_ATLAS = 0
ATLAS_LIBDIR = $(BUILD)/lib
#or ATLAS_LIBDIR = /path/to/system/atlas/lib

ifeq ($(OS), MINGW32_NT-6.1)
OS=WINNT
endif

ifeq ($(OS), WINNT)
fPIC = 
PATH := ${PATH}:${BUILD}/lib:${BUILD}/lib/julia
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
ifeq ($(DARWINVER), 10)
USEGCC = 1
USECLANG = 0
else
USEGCC = 0
USECLANG = 1
endif
endif

ifeq ($(USEGCC),1)
CC = $(CROSS_COMPILE)gcc
CXX = $(CROSS_COMPILE)g++
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -ggdb3 -DDEBUG -fstack-protector-all
SHIPFLAGS = -O3 -DNDEBUG -falign-functions
ifneq ($(ARCH), ppc64)
  SHIPFLAGS += -momit-leaf-frame-pointer
endif
endif

ifeq ($(USECLANG),1)
CC = $(CROSS_COMPILE)clang
CXX = $(CROSS_COMPILE)clang++
JCFLAGS = -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -g -DDEBUG -fstack-protector-all
SHIPFLAGS = -O3 -DNDEBUG
ifeq ($(OS), Darwin)
CC += -mmacosx-version-min=10.6
CXX += -mmacosx-version-min=10.6
JCFLAGS += -D_LARGEFILE_SOURCE -D_DARWIN_USE_64_BIT_INODE=1
endif
endif

FC = $(CROSS_COMPILE)gfortran
JFFLAGS = -O2 $(fPIC)
CPP = $(CC) -E
AR := $(CROSS_COMPILE)ar
AS := $(CROSS_COMPILE)as
LD := $(CROSS_COMPILE)ld
RANLIB := $(CROSS_COMPILE)ranlib

# if not absolute, then relative to JULIA_HOME
JCFLAGS += '-DJL_SYSTEM_IMAGE_PATH="../$(JL_PRIVATE_LIBDIR)/sys.ji"'

# OPENBLAS build options
OPENBLAS_DYNAMIC_ARCH=0
OPENBLAS_USE_THREAD=1
OPENBLAS_TARGET_ARCH=

# Use libraries available on the system instead of building them

USE_SYSTEM_LLVM=0
USE_SYSTEM_LIGHTTPD=0
USE_SYSTEM_LIBUNWIND=0
USE_SYSTEM_READLINE=0
USE_SYSTEM_PCRE=0
USE_SYSTEM_LIBM=0
USE_SYSTEM_BLAS=0
USE_SYSTEM_LAPACK=0
USE_SYSTEM_FFTW=0
USE_SYSTEM_GMP=0
USE_SYSTEM_ARPACK=0
USE_SYSTEM_SUITESPARSE=0
USE_SYSTEM_LIGHTTPD=0
USE_SYSTEM_NGINX=0
USE_SYSTEM_ZLIB=0

ifeq (exists, $(shell [ -e $(JULIAHOME)/Make.user ] && echo exists ))
include $(JULIAHOME)/Make.user
endif

# Snow Leopard specific configuration
ifeq ($(OS), Darwin)
ifeq ($(OSXVER), 10.6)
USE_SYSTEM_LIBM = 1
USE_SYSTEM_BLAS = 1
USE_SYSTEM_LAPACK = 1
endif
endif

# ===========================================================================

ifeq ($(USE_SYSTEM_LIBUNWIND), 1)
LIBUNWIND=-lunwind-generic -lunwind
else
LIBUNWIND=$(BUILD)/lib/libunwind-generic.a $(BUILD)/lib/libunwind.a
endif

ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_CONFIG=llvm-config
else
LLVM_CONFIG=$(LLVMROOT)/bin/llvm-config
endif

ifeq ($(USE_SYSTEM_READLINE), 1)
READLINE = -lreadline 
else
READLINE = $(BUILD)/lib/libreadline.a
endif

ifneq ($(OS),WINNT)
READLINE += -lncurses
else
READLINE += $(BUILD)/lib/libhistory.a
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre-config
else
PCRE_CONFIG = $(BUILD)/bin/pcre-config
endif

ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(OS), Darwin)
LIBBLAS = -framework vecLib -lBLAS
LIBBLASNAME = libblas
else
LIBBLAS = -lblas
LIBBLASNAME = libblas
endif
else
LIBBLAS = -L$(BUILD)/lib -lopenblas
LIBBLASNAME = libopenblas
endif

ifeq ($(USE_SYSTEM_LAPACK), 1)
ifeq ($(OS), Darwin)
LIBLAPACK = -framework vecLib -lLAPACK
LIBLAPACKNAME = liblapack
else
LIBLAPACK = -llapack
LIBLAPACKNAME = liblapack
endif
else
LIBLAPACK = $(LIBBLAS)
LIBLAPACKNAME = $(LIBBLASNAME)
endif

ifeq ($(USE_SYSTEM_LIBM), 1)
LIBM = -lm
LIBMNAME = libm
else
LIBM = -lopenlibm
LIBMNAME = libopenlibm
endif

ifeq ($(USE_MKL), 1)
  USE_LIB64 = 0
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
OSLIBS += -ldl -Wl,-w -framework CoreFoundation -framework CoreServices
WHOLE_ARCHIVE = -Xlinker -all_load
NO_WHOLE_ARCHIVE =
JLDFLAGS =
endif

ifeq ($(OS), WINNT)
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(NO_WHOLE_ARCHIVE) -lpsapi -lkernel32 -lws2_32 -liphlpapi
JCFLAGS += -D_WIN32_WINNT=0x0600
JCXXFLAGS += -D_WIN32_WINNT=0x0600
LLVM_CONFIG := WINEPATH="$(WINEPATH)" $(LLVM_CONFIG).exe
JLDFLAGS = -Wl,--stack,8388608
endif

# MKL

ifeq ($(USE_MKL), 1)
USE_SYSTEM_BLAS=1
USE_SYSTEM_LAPACK=1
LIBBLAS   = -L$(MKLLIB) -lmkl_rt
LIBLAPACK = -L$(MKLLIB) -lmkl_rt
LIBBLASNAME = libmkl_rt
LIBLAPACKNAME = libmkl_rt
ifneq ($(strip $(MKLLIB)),)
  ifeq ($(OS), Linux)
    RPATH += -Wl,-rpath,$(MKLLIB)
  endif
endif
endif

ifeq ($(USE_ATLAS), 1)
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
define symlink_target
CLEAN_TARGETS += clean-$(2)/$(1)
clean-$$(abspath $(2)/$(1)): 
	@-cmd //C rmdir $$(call mingw_to_dos,$(2)/$(1),cd $(2) &&)
$$(subst $$(abspath $(JULIAHOME))/,,$$(abspath $(2)/$(1))): $$(abspath $(2)/$(1))
$$(abspath $(2)/$(1)): | $$(abspath $(2))
	@cmd //C mklink //J $$(call mingw_to_dos,$(2)/$(1),cd $(2) &&) $$(call mingw_to_dos,$(1),) 
endef
else
define symlink_target
CLEAN_TARGETS += clean-$(2)/$(1)
clean-$$(abspath $(2)/$(1)): 
	@-rm $$(abspath $(2)/$(1))
$$(subst $$(abspath $(JULIAHOME))/,,$$(abspath $(2)/$(1))): $$(abspath $(2)/$(1))
$$(abspath $(2)/$(1)): | $$(abspath $(2))
	@ln -sf $$(abspath $(1)) $$@ 
endef
endif

JULIA_EXECUTABLE_debug = $(BUILD)/bin/julia-debug-$(DEFAULT_REPL)$(EXE)
JULIA_EXECUTABLE_release = $(BUILD)/bin/julia-release-$(DEFAULT_REPL)$(EXE)

ifeq ($(OS), WINNT)
JULIA_EXECUTABLE = $(JULIA_EXECUTABLE_release)
else
JULIA_EXECUTABLE = $(JULIAHOME)/julia
endif

# Colors for make
ifndef USE_QUIET
USE_QUIET = 1
endif
ifeq ($(USE_QUIET), 1)

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

QUIET_CC = @printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_LINK = @printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_PERL = @printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_FLISP = @printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_JULIA = @printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);

else

QUIET_CC =
QUIET_LINK =
QUIET_PERL =
QUIET_FLISP =
QUIET_JULIA =

endif
