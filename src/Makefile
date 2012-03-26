JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

SRCS = \
	jltypes gf ast builtins module codegen interpreter \
	alloc dlload sys init task array dump
FLAGS = \
    -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-Iflisp -Isupport -fvisibility=hidden -fno-common \
	-I$(shell $(LLVM_CONFIG) --includedir) \
	-I$(EXTROOT)/include

OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.do)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

ifeq ($(JULIAGC),MARKSWEEP)
SRCS += gc
endif

ifeq ($(USE_COPY_STACKS),1)
JCFLAGS += -DCOPY_STACKS
endif

default: release

release debug: %: libjulia-%

%.o: %.c julia.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c julia.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@
%.o: %.cpp julia.h
	$(QUIET_CC) $(CXX) $(CXXFLAGS) $(SHIPFLAGS) $(shell $(LLVM_CONFIG) --cppflags) -c $< -o $@
%.do: %.cpp julia.h
	$(QUIET_CC) $(CXX) $(CXXFLAGS) $(DEBUGFLAGS) $(shell $(LLVM_CONFIG) --cppflags) -c $< -o $@

ast.o ast.do: julia_flisp.boot.inc

julia_flisp.boot.inc: julia_flisp.boot flisp/libflisp.a
	$(QUIET_FLISP) flisp/flisp ./bin2hex.scm < $< > $@

julia_flisp.boot: julia-parser.scm julia-syntax.scm \
	match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm flisp/libflisp.a
	$(QUIET_FLISP) flisp/flisp ./mk_julia_flisp_boot.scm

codegen.o codegen.do: intrinsics.cpp debuginfo.cpp cgutils.cpp ccall.cpp
builtins.o builtins.do: table.c

support/libsupport.a: support/*.h support/*.c
	$(MAKE) -C support

flisp/libflisp.a: flisp/*.h flisp/*.c support/libsupport.a
	$(MAKE) -C flisp

../libjulia-debug.$(SHLIB_EXT): $(DOBJS) flisp/libflisp.a support/libsupport.a
	$(QUIET_LINK) $(CXX) $(DEBUGFLAGS) $(DOBJS) -shared -o ../libjulia-debug.$(SHLIB_EXT) $(LIBS)
libjulia-debug.a: $(DOBJS) flisp/libflisp.a support/libsupport.a
	rm -f $@
	$(QUIET_LINK) ar -rcs $@ $(DOBJS)
libjulia-debug: libjulia-debug.a ../libjulia-debug.$(SHLIB_EXT)

../libjulia-release.$(SHLIB_EXT): $(OBJS) flisp/libflisp.a support/libsupport.a
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $(OBJS) -shared -o ../libjulia-release.$(SHLIB_EXT) $(LIBS)
libjulia-release.a: $(OBJS) flisp/libflisp.a support/libsupport.a
	rm -f $@
	$(QUIET_LINK) ar -rcs $@ $(OBJS)
libjulia-release: libjulia-release.a ../libjulia-release.$(SHLIB_EXT)

clean:
	rm -f libjulia*.$(SHLIB_EXT)
	rm -f libjulia*.a
	rm -f ../libjulia*.$(SHLIB_EXT)
	rm -f julia_flisp.boot
	rm -f julia_flisp.boot.inc
	rm -f *.do
	rm -f *.o
	rm -f *~ *#

clean-flisp:
	$(MAKE) -C flisp clean

clean-support:
	$(MAKE) -C support clean

cleanall: clean clean-flisp clean-support

.PHONY: debug release clean cleanall clean-*
