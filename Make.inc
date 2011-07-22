JULIALIB = $(JULIAHOME)/lib
EXTROOT = $(JULIAHOME)/external/root
EXTROOTLIB = $(EXTROOT)/lib
LLVMROOT = $(EXTROOT)
OS = $(shell uname)

USEGCC = 1
USECLANG = 0

ifeq ($(USEGCC),1)
CC = gcc
CXX = g++
FC = gfortran
CFLAGS = -pipe -std=gnu99 -fPIC
CXXFLAGS = -pipe -fPIC -fno-rtti
DEBUGFLAGS = -ggdb3 -DDEBUG
SHIPFLAGS = -O3 -DNDEBUG
endif

ifeq ($(USECLANG),1)
CC = clang
CXX = clang++
CFLAGS = -fPIC -fexceptions -pipe
CXXFLAGS = -fPIC -fno-rtti -fexceptions -pipe
DEBUGFLAGS = -g -DDEBUG
SHIPFLAGS = -O4 -DNDEBUG
endif

DEFAULT_REPL = readline
JULIAGC = MARKSWEEP
USE_COPY_STACKS = 1

# OS specific stuff

ifeq ($(OS), Linux)
SHLIB_EXT = so
OSLIBS += -Wl,--export-dynamic -Wl,--version-script=$(JULIAHOME)/src/julia.expmap $(EXTROOT)/lib/libunwind-generic.a $(EXTROOT)/lib/libunwind.a
endif

ifeq ($(OS), Darwin)
SHLIB_EXT = dylib
OSLIBS += -Wl,-w -framework ApplicationServices
endif
