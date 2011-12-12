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

# Compiler specific stuff

USEGCC = 1
USECLANG = 0

ifeq ($(USEGCC),1)
CC = gcc
CXX = g++
FC = gfortran
CFLAGS = -pipe -std=gnu99 -fPIC
CXXFLAGS = -pipe -fPIC -fno-rtti
DEBUGFLAGS = -ggdb3 -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG -falign-functions -momit-leaf-frame-pointer
endif

ifeq ($(USECLANG),1)
CC = clang
CXX = clang++
CFLAGS = -fPIC -fexceptions -pipe
CXXFLAGS = -fPIC -fno-rtti -fexceptions -pipe
DEBUGFLAGS = -g -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG
endif

# OS specific stuff

USE_SYSTEM_READLINE=1

# If OpenBLAS doesn't build, find your architechure at the following URL and enter it below
# https://github.com/xianyi/OpenBLAS/blob/master/GotoBLAS_01Readme.txt
TARGET_OPENBLAS_ARCH=NEHALEM


ifeq ($(OS), Linux)
SHLIB_EXT = so
OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(EXTROOT)/lib/libunwind-generic.a $(EXTROOT)/lib/libunwind.a
endif

ifeq ($(OS), Darwin)
SHLIB_EXT = dylib
OSLIBS += -Wl,-w -framework ApplicationServices
#CFLAGS += -fno-optimize-sibling-calls -fno-inline-functions
endif

# Libraries to link
LIBS = $(shell $(LLVMROOT)/bin/llvm-config --libfiles) $(JULIAHOME)/src/flisp/libflisp.a $(JULIAHOME)/src/support/libsupport.a -L$(EXTROOT)/lib -lutil -ldl -lm $(OSLIBS) -lpthread $(shell $(LLVMROOT)/bin/llvm-config --ldflags)
