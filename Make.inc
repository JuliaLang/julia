NAME = julia
JULIALIB = $(JULIAHOME)/lib
EXTROOT = $(JULIAHOME)/ext/root
EXTROOTLIB = $(EXTROOT)/lib
LLVMROOT = $(EXTROOT)

include $(JULIAHOME)/Make.inc.$(shell uname)
