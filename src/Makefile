JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

SRCS = \
	jltypes gf ast builtins module codegen interpreter \
	alloc dlload sys init task array dump toplevel wrapper

FLAGS = \
	-D_GNU_SOURCE \
	-Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-Iflisp -Isupport -fvisibility=hidden -fno-common \
	-I$(shell $(LLVM_CONFIG) --includedir) \
	-I$(JULIAHOME)/deps/libuv/include -I$(JULIAHOME)/usr/include

LIBS = $(WHOLE_ARCHIVE) $(JULIAHOME)/src/flisp/libflisp.a $(WHOLE_ARCHIVE) $(JULIAHOME)/src/support/libsupport.a -L$(BUILD)/lib $(BUILD)/lib/libuv.a $(shell $(LLVM_CONFIG) --libs) $(NO_WHOLE_ARCHIVE) $(shell $(LLVM_CONFIG) --ldflags) $(OSLIBS)

ifneq ($(MAKECMDGOALS),debug)
TARGET =
else
TARGET = "debug"
endif

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

HEADERS = julia.h $(wildcard support/*.h) $(JULIAHOME)/deps/libuv/include/uv.h

%.o: %.c $(HEADERS)
	$(QUIET_CC) $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c $(HEADERS)
	$(QUIET_CC) $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@
%.o: %.cpp $(HEADERS)
	$(QUIET_CC) $(CXX) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) $(shell $(LLVM_CONFIG) --cppflags) -c $< -o $@
%.do: %.cpp $(HEADERS)
	$(QUIET_CC) $(CXX) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) $(shell $(LLVM_CONFIG) --cppflags) -c $< -o $@

ast.o ast.do: julia_flisp.boot.inc flisp/*.h

julia_flisp.boot.inc: julia_flisp.boot flisp/libflisp.a
	$(QUIET_FLISP) ./flisp/flisp ./bin2hex.scm < $< > $@

julia_flisp.boot: julia-parser.scm julia-syntax.scm \
	match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm flisp/libflisp.a
	$(QUIET_FLISP) ./flisp/flisp ./mk_julia_flisp_boot.scm

codegen.o codegen.do: intrinsics.cpp debuginfo.cpp cgutils.cpp ccall.cpp
builtins.o builtins.do: table.c

support/libsupport.a: support/*.h support/*.c
	$(MAKE) -C support $(TARGET)

flisp/libflisp.a: flisp/*.h flisp/*.c support/libsupport.a
	$(MAKE) -C flisp

$(BUILD)/$(JL_LIBDIR)/libjulia-debug.$(SHLIB_EXT): julia.expmap $(DOBJS) flisp/libflisp.a support/libsupport.a $(BUILD)/lib/libuv.a
	$(QUIET_LINK) $(CXX) $(DEBUGFLAGS) $(DOBJS) $(RPATH_ORIGIN) -shared -o $@ $(LDFLAGS) $(LIBS)
	$(INSTALL_NAME_CMD)libjulia-debug.$(SHLIB_EXT) $@
libjulia-debug.a: julia.expmap $(DOBJS) flisp/libflisp.a support/libsupport.a
	rm -f $@
	$(QUIET_LINK) ar -rcs $@ $(DOBJS)
libjulia-debug: $(BUILD)/$(JL_LIBDIR)/libjulia-debug.$(SHLIB_EXT)

ifeq ($(SHLIB_EXT), so)
  SONAME = -Wl,-soname=libjulia-release.so
else
  SONAME =
endif

$(BUILD)/$(JL_LIBDIR)/libjulia-release.$(SHLIB_EXT): julia.expmap $(OBJS) flisp/libflisp.a support/libsupport.a $(BUILD)/lib/libuv.a
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $(OBJS) $(RPATH_ORIGIN) -shared -o $@ $(LDFLAGS) $(LIBS) $(SONAME)
	$(INSTALL_NAME_CMD)libjulia-release.$(SHLIB_EXT) $@
libjulia-release.a: julia.expmap $(OBJS) flisp/libflisp.a support/libsupport.a
	rm -f $@
	$(QUIET_LINK) ar -rcs $@ $(OBJS)
libjulia-release: $(BUILD)/$(JL_LIBDIR)/libjulia-release.$(SHLIB_EXT)

clean:
	-rm -f $(BUILD)/$(JL_LIBDIR)/libjulia*
	-rm -f julia_flisp.boot julia_flisp.boot.inc
	-rm -f *.do *.o *~ *# *.$(SHLIB_EXT) *.a h2j

clean-flisp:
	-$(MAKE) -C flisp clean

clean-support:
	-$(MAKE) -C support clean

cleanall: clean clean-flisp clean-support

.PHONY: debug release clean cleanall clean-*


h2j: h2j.cpp
	$(QUIET_CC) $(CXX) $(CFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) $(shell $(LLVM_CONFIG) --cppflags) $< -o $@ \
		-lclangFrontendTool -lclangBasic -lclangLex -lclangDriver -lclangFrontend -lclangParse \
		-lclangAST -lclangSema -lclangAnalysis -lclangEdit \
		-lclangRewrite -lclangSerialization -lclangStaticAnalyzerCheckers \
		-lclangStaticAnalyzerCore -lclangStaticAnalyzerFrontend -lclangTooling \
		-lclangCodeGen -lclangARCMigrate -lclang  $(LIBS) $(LDFLAGS)
	
