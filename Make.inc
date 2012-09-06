# -*- mode: makefile-gmake -*-

JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)

USR = $(JULIAHOME)/usr
USRLIB = $(USR)/lib
USRBIN = $(USR)/bin
USRINC = $(USR)/include
LLVMROOT = $(USR)
BUILD = $(USR)

OS = $(shell uname)
ARCH = $(shell uname -m)

USE_MKL = 0
MKLLIB = /path/to/mkl/lib/intel64

USE_ATLAS = 0
ATLAS_LIBDIR = /path/to/atlas/build/lib

ifeq ($(OS), MINGW32_NT-6.1)
OS=WINNT
endif

ifeq ($(OS), WINNT)
fPIC = 
else
fPIC = -fPIC
endif

DEFAULT_REPL = readline
JULIAGC = MARKSWEEP
USE_COPY_STACKS = 1

# Compiler specific stuff

FC = gfortran
JFFLAGS = -O2 $(fPIC)

USEGCC = 1
USECLANG = 0

ifeq ($(OS), Darwin)
OSVER = $(shell uname -r | cut -b 1-2)
ifeq ($(OSVER), 10)
USEGCC = 1
USECLANG = 0
else
USEGCC = 0
USECLANG = 1
endif
endif

ifeq ($(USEGCC),1)
CC = gcc
CXX = g++
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -ggdb3 -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG -falign-functions
ifneq ($(ARCH), ppc64)
  SHIPFLAGS += -momit-leaf-frame-pointer
endif
endif

ifeq ($(USECLANG),1)
CC = clang
CXX = clang++
JCFLAGS = -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -g -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG
ifeq ($(OS), Darwin)
CC += -mmacosx-version-min=10.6
CXX += -mmacosx-version-min=10.6
JCFLAGS += -D_LARGEFILE_SOURCE -D_DARWIN_USE_64_BIT_INODE=1
endif
endif

# if not absolute, then relative to JULIA_HOME
JCFLAGS += '-DJL_SYSTEM_IMAGE_PATH="../lib/julia/sys.ji"'

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
USE_SYSTEM_BLAS=0
USE_SYSTEM_LAPACK=0
USE_SYSTEM_FFTW=0
USE_SYSTEM_GMP=0
USE_SYSTEM_ARPACK=0
USE_SYSTEM_SUITESPARSE=0
USE_SYSTEM_GLPK=0
USE_SYSTEM_NGINX=0
USE_SYSTEM_ZLIB=0

ifeq ($(USE_DEBIAN), 1)
USE_SYSTEM_LLVM=1
USE_SYSTEM_LIGHTTPD=1
USE_SYSTEM_LIBUNWIND=1
USE_SYSTEM_READLINE=1
USE_SYSTEM_PCRE=1
USE_SYSTEM_BLAS=1
USE_SYSTEM_LAPACK=1
USE_SYSTEM_FFTW=1
USE_SYSTEM_GMP=1
USE_SYSTEM_ARPACK=1
USE_SYSTEM_SUITESPARSE=1
USE_SYSTEM_GLPK=1
USE_SYSTEM_ZLIB=1
endif

-include $(JULIAHOME)/Make.user

# ===========================================================================

ifeq ($(USE_SYSTEM_LIBUNWIND), 1)
LIBUNWIND=-lunwind-generic -lunwind
else
LIBUNWIND=$(USRLIB)/libunwind-generic.a $(USRLIB)/libunwind.a
endif

ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_CONFIG=llvm-config
else
LLVM_CONFIG=$(LLVMROOT)/bin/llvm-config
endif

ifeq ($(USE_SYSTEM_READLINE), 1)
READLINE = -lreadline 
else
READLINE = $(USR)/lib/libreadline.a
endif

ifneq ($(OS),WINNT)
READLINE += -lncurses
else
READLINE += $(USR)/lib/libhistory.a
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre-config
else
PCRE_CONFIG = $(USR)/bin/pcre-config
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
ifeq ($(OS), WINNT)
LIBBLAS = $(USRLIB)/libopenblas-r0.2.2.$(SHLIB_EXT) #necessary due to some stupid Windows behavoir - will try yto fix soon
LIBBLASNAME = libopenblas-r0.2.2
else
LIBBLAS = -L$(USRLIB) -lopenblas
LIBBLASNAME = libopenblas
endif
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

# OS specific stuff

# must end with a / and have no trailing spaces
INSTALL_NAME_ID_DIR = @rpath/

RPATH = 
RPATH_ORIGIN = 

ifeq ($(OS), WINNT)
SHELL_EXT = bat
else
SHELL_EXT = sh
endif

ifeq ($(OS), Linux)
INSTALL_NAME_CMD = true -ignore
INSTALL_NAME_CHANGE_CMD = true -ignore
SHLIB_EXT = so
RPATH = -Wl,-rpath,'$$ORIGIN/../lib'
RPATH_ORIGIN = -Wl,-rpath,'$$ORIGIN'
OSLIBS += -ldl -lrt -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap -Wl,--no-whole-archive $(LIBUNWIND)
WHOLE_ARCHIVE = -Wl,--whole-archive
NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
JLDFLAGS = -Wl,-Bdynamic
endif

ifeq ($(OS), FreeBSD)
INSTALL_NAME_CMD = true -ignore
INSTALL_NAME_CHANGE_CMD = true -ignore
SHLIB_EXT = so
RPATH = -Wl,-rpath,'$$ORIGIN/../lib' -Wl,-z,origin
RPATH_ORIGIN = -Wl,-rpath,'$$ORIGIN' -Wl,-z,origin
WHOLE_ARCHIVE = -Wl,--whole-archive
NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
JLDFLAGS = -Wl,-Bdynamic
OSLIBS += -lkvm -lrt -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(NO_WHOLE_ARCHIVE) $(LIBUNWIND)
endif

ifeq ($(OS), Darwin)
INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
INSTALL_NAME_CHANGE_CMD = install_name_tool -change
RPATH = -Wl,-rpath,@executable_path/../lib
SHLIB_EXT = dylib
OSLIBS += -ldl -Wl,-w -framework ApplicationServices
WHOLE_ARCHIVE = -Xlinker -all_load
NO_WHOLE_ARCHIVE =
JLDFLAGS =
endif

ifeq ($(OS), WINNT)
INSTALL_NAME_CMD = true -ignore
INSTALL_NAME_CHANGE_CMD = true -ignore
SHLIB_EXT = dll
WHOLE_ARCHIVE = -Wl,--whole-archive
NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(JULIAHOME)/src/julia.expmap -lpthread $(NO_WHOLE_ARCHIVE) -lPsapi -lkernel32 -lWs2_32 -lIphlpapi
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

ifeq ($(OS), WINNT)
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

JULIA_EXECUTABLE_debug = $(BUILD)/bin/julia-debug-$(DEFAULT_REPL)
JULIA_EXECUTABLE_release = $(BUILD)/bin/julia-release-$(DEFAULT_REPL)

ifeq ($(OS), WINNT)
JULIA_EXECUTABLE = $(JULIA_EXECUTABLE_release)
else
JULIA_EXECUTABLE = $(JULIAHOME)/julia
endif


# Colors for make

USE_QUIET = 1
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
