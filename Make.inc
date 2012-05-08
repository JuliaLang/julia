# -*- mode: makefile-gmake -*-

JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)

USR = $(JULIAHOME)/usr
USRLIB = $(USR)/lib
USRBIN = $(USR)/bin
USRINC = $(USR)/include
LLVMROOT = $(USR)

OS = $(shell uname)
ARCH = $(shell uname -m)

USE_MKL = 0
MKLLIB = /path/to/mkl/lib/intel64

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

ifeq ($(USEGCC),1)
CC = gcc
CXX = g++
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing -D_FILE_OFFSET_BITS=64
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -ggdb3 -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG -falign-functions -momit-leaf-frame-pointer
endif

ifeq ($(USECLANG),1)
CC = clang
CXX = clang++
JCFLAGS = -std=gnu99 -pipe $(fPIC) -fno-strict-aliasing
JCXXFLAGS = -pipe $(fPIC) -fno-rtti
DEBUGFLAGS = -g -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG
endif

# if not absolute, then relative to JULIA_HOME
JCFLAGS += '-DJL_SYSTEM_IMAGE_PATH="sys.ji"'

# If OpenBLAS doesn't build, find your architechure at the following URL and enter it below
# https://github.com/xianyi/OpenBLAS/blob/master/GotoBLAS_01Readme.txt
TARGET_OPENBLAS_ARCH=

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
endif

#ifeq ($(OS), Darwin)
#USE_SYSTEM_BLAS=1
#USE_SYSTEM_LAPACK=1
#endif

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
READLINE += -lncurses -lcurses
else
READLINE += $(USR)/lib/libhistory.a
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre-config
else
PCRE_CONFIG = $(USR)/bin/pcre-config
endif

LIBBLASNAME = libopenblas
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
LIBBLAS = deps/openblas-$(OPENBLAS_VER)/libopenblas.lib
else
LIBBLAS = -L$(USRLIB) -lopenblas
endif
endif

LIBLAPACKNAME = libopenblas
ifeq ($(USE_SYSTEM_LAPACK), 1)
ifeq ($(OS), Darwin)
LIBLAPACK = -framework vecLib -lLAPACK
LIBLAPACKNAME = liblapack
else
LIBLAPACK = -llapack
LIBLAPACKNAME = liblapack
endif
else
LIBLAPACK = -L$(USRLIB) -lopenblas
endif

# OS specific stuff

ifeq ($(OS), Linux)
SHLIB_EXT = so
RPATH = -Wl,-rpath,'$$ORIGIN/usr/lib'
OSLIBS += -ldl -lrt -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(LIBUNWIND)
WHOLE_ARCHIVE = -Wl,--whole-archive
NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
endif

ifeq ($(OS), FreeBSD)
SHLIB_EXT = so
OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap -lutil -lm  $(LIBUNWIND)
RPATH = -Wl,-rpath,'$$ORIGIN/usr/lib'
WHOLE_ARCHIVE = -Wl,--whole-archive
NO_WHOLE_ARCHIVE = -Wl,--no-whole-archive
endif

ifeq ($(OS), Darwin)
SHLIB_EXT = dylib
RPATH = 
OSLIBS += -ldl -Wl,-w -framework ApplicationServices
WHOLE_ARCHIVE = -Xlinker -all_load
NO_WHOLE_ARCHIVE = 
endif

ifeq ($(OS), WINNT)
SHLIB_EXT = dll
RPATH = 
OSLIBS += -Wl,--export-all-symbols -Wl,--version-script=$(JULIAHOME)/src/julia.expmap -L"F:\Program Files\Microsoft SDKs\Windows\v7.1\Lib" -lntdll -lkernel32 -lWs2_32 -lIphlpapi
WHOLE_ARCHIVE =
NO_WHOLE_ARCHIVE =
endif

# MKL

ifeq ($(USE_MKL), 1)
USE_SYSTEM_BLAS=1
USE_SYSTEM_LAPACK=1
LIBBLAS   = -L$(MKLLIB) -lmkl_rt
LIBLAPACK = -L$(MKLLIB) -lmkl_rt
LIBBLASNAME = libmkl_rt
LIBLAPACKNAME = libmkl_rt
endif

# Libraries to link
LIBS = $(shell $(LLVM_CONFIG) --libfiles) $(JULIAHOME)/src/flisp/libflisp.a $(JULIAHOME)/src/support/libsupport.a -L$(USR)/lib $(USRLIB)/uv.a $(OSLIBS) -lpthread $(shell $(LLVM_CONFIG) --ldflags)

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

GOAL=$(subst $(abspath $(JULIAHOME))/,,$(abspath $(CURDIR)/$@))

QUIET_CC = @printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_LINK = @printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_PERL = @printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_FLISP = @printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
QUIET_JULIA = @printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR);
endif
