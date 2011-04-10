NAME = julia
JULIALIB = $$JULIAHOME/lib
EXTROOT = $$JULIAHOME/ext/root
EXTROOTLIB = $(EXTROOT)/lib
LLVMROOT = $(EXTROOT)

include $(JULIAHOME)/Make.inc.$(shell uname)

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-I$(FLISPDIR) -I$(LLTDIR) $(HFILEDIRS:%=-I%) $(LIBDIRS:%=-L%) \
	$(CONFIG) -I$(shell $(LLVMROOT)/bin/llvm-config --includedir) \
	-fvisibility=hidden
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

LIBFILES = $(FLISP) $(LLT)
LIBS = $(LIBFILES) -L$(ROOT)/lib -lutil -ldl -lm -lreadline $(OSLIBS) \
	$(shell $(LLVMROOT)/bin/llvm-config --ldflags --libs engine) -lpthread
