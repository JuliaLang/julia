JULIAHOME = $(shell pwd)/..
include $(JULIAHOME)/Make.inc

SRCS = jltypes gf ast builtins module codegen interpreter alloc dlload \
	sys init task array dump
OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.do)
SUPPORTDIR = ./support
FLISPDIR = ./flisp
SUPPORT = $(SUPPORTDIR)/libsupport.a
FLISP = $(FLISPDIR)/libflisp.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(FLISPDIR) -I$(SUPPORTDIR) -fvisibility=hidden -fno-common \
	-I$(shell $(LLVMROOT)/bin/llvm-config --includedir)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

ifeq ($(JULIAGC),MARKSWEEP)
SRCS += gc
endif

ifeq ($(USE_COPY_STACKS),1)
CFLAGS += -DCOPY_STACKS
endif

default: release

debug release: %: libjulia-%

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
	$(FLISPDIR)/flisp ./preparse.scm $< | $(FLISPDIR)/flisp ./bin2hex.scm > $@

codegen.o codegen.do: intrinsics.cpp debuginfo.cpp
builtins.o builtins.do: table.c

$(SUPPORT): $(SUPPORTDIR)/*.h $(SUPPORTDIR)/*.c
	cd $(SUPPORTDIR) && $(MAKE)

$(FLISP): $(FLISPDIR)/*.h $(FLISPDIR)/*.c $(SUPPORT)
	cd $(FLISPDIR) && $(MAKE)

$(JULIAHOME)/libjulia-debug.$(SHLIB_EXT): $(DOBJS) $(FLISP) $(SUPPORT)
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -shared -o $(JULIAHOME)/libjulia-debug.$(SHLIB_EXT) $(LIBS)
$(JULIAHOME)/libjulia-debug.a: $(DOBJS) $(FLISP) $(SUPPORT)
	ar -rcs libjulia-debug.a $(DOBJS) 
#libjulia-debug: $(JULIAHOME)/libjulia-debug.$(SHLIB_EXT)
libjulia-debug: $(JULIAHOME)/libjulia-debug.a

$(JULIAHOME)/libjulia-release.$(SHLIB_EXT): $(OBJS) $(FLISP) $(SUPPORT)
	$(CXX) $(SHIPFLAGS) $(OBJS) -shared -o $(JULIAHOME)/libjulia-release.$(SHLIB_EXT) $(LIBS)
$(JULIAHOME)/libjulia-release.a: $(OBJS)
	ar -rcs libjulia-release.a $(OBJS) 
#libjulia-release: $(JULIAHOME)/libjulia-release.$(SHLIB_EXT)
libjulia-release: $(JULIAHOME)/libjulia-release.a

clean:
	rm -f $(JULIAHOME)/libjulia*.$(SHLIB_EXT)
	rm -f julia_flisp.boot
	rm -f julia_flisp.boot.inc
	rm -f boot.j.inc
	rm -f *.do
	rm -f *.o
	rm -f *.a
	rm -f *~ *#

clean-flisp:
	$(MAKE) -C $(FLISPDIR) clean

clean-support:
	$(MAKE) -C $(SUPPORTDIR) clean

cleanall: clean clean-flisp clean-support

.PHONY: debug release clean cleanall clean-*
