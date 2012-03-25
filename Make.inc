# -*- mode: makefile-gmake -*-

JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)

JULIALIB = $(JULIAHOME)/lib
EXTROOT = $(JULIAHOME)/external/root
EXTROOTLIB = $(EXTROOT)/lib
LLVMROOT = $(EXTROOT)

OS = $(shell uname)
ARCH = $(shell uname -m)

DEFAULT_REPL = readline
JULIAGC = MARKSWEEP
USE_COPY_STACKS = 1

ifdef PARALLEL_BUILD_JOBS
  jPARALLEL_BUILD_JOBS = -j$(PARALLEL_BUILD_JOBS)
endif

# Compiler specific stuff

FC = gfortran
JFFLAGS = -O2 -fPIC

USEGCC = 1
USECLANG = 0

ifeq ($(USEGCC),1)
CC = gcc
CXX = g++
JCFLAGS = -std=gnu99 -pipe -fPIC -fno-strict-aliasing
JCXXFLAGS = -pipe -fPIC -fno-rtti
DEBUGFLAGS = -ggdb3 -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG -falign-functions -momit-leaf-frame-pointer
endif

ifeq ($(USECLANG),1)
CC = clang
CXX = clang++
JCFLAGS = -std=gnu99 -pipe -fPIC -fno-strict-aliasing
JCXXFLAGS = -pipe -fPIC -fno-rtti
DEBUGFLAGS = -g -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG
endif

# If OpenBLAS doesn't build, find your architechure at the following URL and enter it below
# https://github.com/xianyi/OpenBLAS/blob/master/GotoBLAS_01Readme.txt
TARGET_OPENBLAS_ARCH=

# Use libraries available on the system instead of building them

USE_SYSTEM_LLVM=0
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
LIBUNWIND=$(EXTROOT)/lib/libunwind-generic.a $(EXTROOT)/lib/libunwind.a
endif

ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_CONFIG=llvm-config
else
LLVM_CONFIG=$(LLVMROOT)/bin/llvm-config
endif

ifeq ($(USE_SYSTEM_READLINE), 1)
READLINE = -lreadline -lncurses -lcurses
else
READLINE = $(EXTROOT)/lib/libreadline.a -lncurses -lcurses
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre-config
else
PCRE_CONFIG = $(EXTROOT)/bin/pcre-config
endif

ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(OS), Darwin)
LIBBLAS = -framework vecLib -lBLAS
else
LIBBLAS = -lblas
endif
else
LIBBLAS = $(JULIAHOME)/external/openblas-$(OPENBLAS_VER)/libopenblas.a
endif

ifeq ($(USE_SYSTEM_LAPACK), 1)
ifeq ($(OS), Darwin)
LIBLAPACK = -framework vecLib -lLAPACK
else
LIBLAPACK = -llapack
endif
else
LIBLAPACK = $(JULIAHOME)/external/lapack-$(LAPACK_VER)/liblapack.a
endif

# OS specific stuff

ifeq ($(OS), Linux)
SHLIB_EXT = so
JLDFLAGS = -Xlinker -rpath $(JULIAHOME)/lib
OSLIBS += -ldl -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(LIBUNWIND)
endif

ifeq ($(OS), FreeBSD)
SHLIB_EXT = so
JLDFLAGS = -Xlinker -rpath $(JULIAHOME)/lib
OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(LIBUNWIND)
endif

ifeq ($(OS), Darwin)
SHLIB_EXT = dylib
JLDFLAGS = 
OSLIBS += -ldl -Wl,-w -framework ApplicationServices 
endif

# Libraries to link
LIBS = $(shell $(LLVM_CONFIG) --libfiles) $(JULIAHOME)/src/flisp/libflisp.a $(JULIAHOME)/src/support/libsupport.a -L$(EXTROOT)/lib -lutil -lm $(OSLIBS) -lpthread $(shell $(LLVM_CONFIG) --ldflags)

# Colors for make

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


