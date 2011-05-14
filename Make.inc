JULIALIB = $(JULIAHOME)/lib
EXTROOT = $(JULIAHOME)/external/root
EXTROOTLIB = $(EXTROOT)/lib
LLVMROOT = $(EXTROOT)
OS = $(shell uname)
DOWNLOAD = curl -O
#DOWNLOAD = wget

CC = gcc 
CXX = g++
CFLAGS = -std=gnu99 -fPIC -D_FORTIFY_SOURCE=0
CXXFLAGS = -fPIC -D_FORTIFY_SOURCE=0
FC = gfortran
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

# Different REPLs. Currently, only one of these can be chosen
DEFAULT_REPL = readline
REPL_READLINE = 1
#REPL_BASIC = 1
#REPL_CLOUD = 1

JULIAGC = MARKSWEEP
USE_COPY_STACKS = 1

# OS specific stuff

ifeq ($(OS), Linux)
SHLIB_EXT = so
OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap
endif

ifeq ($(OS), Darwin)
SHLIB_EXT = dylib
OSLIBS += -Wl,-w -framework ApplicationServices -framework vecLib
endif

# Library versions for building external libraries

LLVM_VER = 2.9
READLINE_VER = 6.2
FDLIBM_VER = 5.3
PCRE_VER = 8.12
LAPACK_VER = 3.3.1
ARPACK_VER = 96
MONGOOSE_VER = 3.0
