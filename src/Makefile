JULIAHOME = $(shell pwd)/..
include $(JULIAHOME)/Make.inc

SRCS = jltypes gf ast builtins module codegen interpreter alloc dlload \
	sys init task array dump
OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.do)
LLTDIR = ./support
FLISPDIR = ./flisp
LLT = $(LLTDIR)/libllt.a
FLISP = $(FLISPDIR)/libflisp.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-I$(FLISPDIR) -I$(LLTDIR) $(HFILEDIRS:%=-I%) $(LIBDIRS:%=-L%) \
	-I$(shell $(LLVMROOT)/bin/llvm-config --includedir) -fvisibility=hidden
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

LIBFILES = $(FLISP) $(LLT)
LIBS = $(LIBFILES) -L$(EXTROOT)/lib -lutil -ldl -lm $(OSLIBS) \
	$(shell $(LLVMROOT)/bin/llvm-config --ldflags --libs engine) -lpthread 

ifeq ($(JULIAGC),MARKSWEEP)
SRCS += gc
endif

ifeq ($(USE_COPY_STACKS),1)
CFLAGS += -DCOPY_STACKS
endif

%.o: %.c julia.h
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c julia.h
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@
%.o: %.cpp julia.h
	$(CXX) $(CXXFLAGS) $(SHIPFLAGS) $(shell $(LLVMROOT)/bin/llvm-config --cppflags) -c $< -o $@
%.do: %.cpp julia.h
	$(CXX) $(CXXFLAGS) $(DEBUGFLAGS) $(shell $(LLVMROOT)/bin/llvm-config --cppflags) -c $< -o $@

ast.o ast.do: julia_flisp.boot.inc boot.j.inc
julia_flisp.boot.inc: julia_flisp.boot $(FLISP)
	$(FLISPDIR)/flisp ./bin2hex.scm < $< > $@
julia_flisp.boot: julia-parser.scm julia-syntax.scm \
	match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm $(FLISP)
	$(FLISPDIR)/flisp ./mk_julia_flisp_boot.scm

boot.j.inc: boot.j preparse.scm julia_flisp.boot
	$(FLISPDIR)/flisp ./preparse.scm < $< | $(FLISPDIR)/flisp ./bin2hex.scm > $@

codegen.o codegen.do: intrinsics.cpp
builtins.o builtins.do: table.c

$(LLT): $(LLTDIR)/*.h $(LLTDIR)/*.c
	cd $(LLTDIR) && $(MAKE)

$(FLISP): $(FLISPDIR)/*.h $(FLISPDIR)/*.c $(LLT)
	cd $(FLISPDIR) && $(MAKE)

libjulia-debug: libjulia-debug.$(SHLIB_EXT)
libjulia-debug.$(SHLIB_EXT): $(DOBJS) $(LIBFILES)
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -shared -o $(JULIAHOME)/libjulia-debug.$(SHLIB_EXT) $(LIBS)

libjulia-release: libjulia-release.$(SHLIB_EXT)
libjulia-release.$(SHLIB_EXT): $(OBJS) $(LIBFILES)
	$(CXX) $(SHIPFLAGS) $(OBJS) -shared -o $(JULIAHOME)/libjulia-release.$(SHLIB_EXT) $(LIBS)


debug release: %: julia-%

clean:
	rm -f $(JULIAHOME)/libjulia*.$(SHLIB_EXT)
	rm -f julia_flisp.boot
	rm -f julia_flisp.boot.inc
	rm -f boot.j.inc
	rm -f *.do
	rm -f *.o
	rm -f *.ji
	rm -f *~ *#

cleanother:
	$(MAKE) -C $(LLTDIR) clean
	$(MAKE) -C $(FLISPDIR) clean

cleanall: clean cleanother

.PHONY: debug release clean cleanall cleanother
