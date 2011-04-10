NAME = julia
JULIALIB = $(JULIAHOME)/lib
EXTROOT = $(JULIAHOME)/ext/root
EXTROOTLIB = $(EXTROOT)/lib
LLVMROOT = $(EXTROOT)

NBITS = $(shell (test -e $(JULIAHOME)/nbits || $(CC) $(JULIAHOME)/nbits.c -o $(JULIAHOME)/nbits) && $(JULIAHOME)/nbits)
include $(JULIAHOME)/Make.inc.$(shell uname)
