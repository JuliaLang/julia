JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/Versions.make

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

SRCS = \
	jltypes gf ast builtins module codegen interpreter \
	alloc dlload sys init task array dump toplevel jl_uv jlapi profile llvm-simdloop

FLAGS = \
	-D_GNU_SOURCE -Iflisp -Isupport \
	-I$(call exec,$(LLVM_CONFIG) --includedir) \
	-I$(LIBUV_INC) -I$(build_includedir) -DLIBRARY_EXPORTS
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -fvisibility=hidden -fno-common
endif

LLVMLINK = $(call exec,$(LLVM_CONFIG) --libs) $(call exec,$(LLVM_CONFIG) --system-libs 2> /dev/null)
ifeq ($(USE_LLVM_SHLIB),1)
LLVMLINK = -lLLVM-$(LLVM_VER)
endif

COMMON_LIBS = -L$(build_shlibdir) -L$(build_libdir) $(LIBUV) $(LIBUTF8PROC) $(NO_WHOLE_ARCHIVE) $(call exec,$(LLVM_CONFIG) --ldflags) $(LLVMLINK) $(OSLIBS)
DEBUG_LIBS = $(WHOLE_ARCHIVE) $(JULIAHOME)/src/flisp/libflisp-debug.a $(WHOLE_ARCHIVE) $(JULIAHOME)/src/support/libsupport-debug.a $(COMMON_LIBS)
RELEASE_LIBS = $(WHOLE_ARCHIVE) $(JULIAHOME)/src/flisp/libflisp.a $(WHOLE_ARCHIVE) $(JULIAHOME)/src/support/libsupport.a $(COMMON_LIBS)

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

HEADERS = julia.h julia_internal.h options.h $(wildcard support/*.h) $(LIBUV_INC)/uv.h

%.o: %.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -DNDEBUG -c $< -o $@)
%.do: %.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)
%.o: %.cpp $(HEADERS)
	@$(call PRINT_CC, $(CXX) $(call exec,$(LLVM_CONFIG) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) -c $< -o $@)
%.do: %.cpp $(HEADERS)
	@$(call PRINT_CC, $(CXX) $(call exec,$(LLVM_CONFIG) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@)

ast.o ast.do: julia_flisp.boot.inc flisp/*.h

julia_flisp.boot.inc: julia_flisp.boot flisp/libflisp.a
	@$(call PRINT_FLISP, $(call spawn,./flisp/flisp) ./bin2hex.scm < $< > $@)

julia_flisp.boot: julia-parser.scm julia-syntax.scm \
	match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm flisp/libflisp.a
	@$(call PRINT_FLISP, $(call spawn,./flisp/flisp) ./mk_julia_flisp_boot.scm)

codegen.o codegen.do: intrinsics.cpp debuginfo.cpp cgutils.cpp ccall.cpp disasm.cpp
builtins.o builtins.do: table.c

support/libsupport.a: support/*.h support/*.c
	$(MAKE) -C support

support/libsupport-debug.a: support/*.h support/*.c
	$(MAKE) -C support debug

flisp/libflisp.a: flisp/*.h flisp/*.c support/libsupport.a
	$(MAKE) -C flisp

flisp/libflisp-debug.a: flisp/*.h flisp/*.c support/libsupport-debug.a
	$(MAKE) -C flisp debug

ifneq ($(USEMSVC), 1)
CXXLD = $(CXX) -shared
else
CXXLD = $(LD) -dll -export:jl_setjmp -export:jl_longjmp
endif

$(build_shlibdir)/libjulia-debug.$(SHLIB_EXT): julia.expmap $(DOBJS) flisp/libflisp-debug.a support/libsupport-debug.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(DEBUGFLAGS) $(DOBJS) $(RPATH_ORIGIN) -o $@ $(LDFLAGS) $(DEBUG_LIBS))
	$(INSTALL_NAME_CMD)libjulia-debug.$(SHLIB_EXT) $@
libjulia-debug.a: julia.expmap $(DOBJS) flisp/libflisp-debug.a support/libsupport-debug.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(DOBJS))
libjulia-debug: $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT)

ifeq ($(SHLIB_EXT), so)
  SONAME = -Wl,-soname=libjulia.so
else
  SONAME =
endif

$(build_shlibdir)/libjulia.$(SHLIB_EXT): julia.expmap $(OBJS) flisp/libflisp.a support/libsupport.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(SHIPFLAGS) $(OBJS) $(RPATH_ORIGIN) -o $@ $(LDFLAGS) $(RELEASE_LIBS) $(SONAME))
	$(INSTALL_NAME_CMD)libjulia.$(SHLIB_EXT) $@
libjulia.a: julia.expmap $(OBJS) flisp/libflisp.a support/libsupport.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(OBJS))
libjulia-release: $(build_shlibdir)/libjulia.$(SHLIB_EXT)

clean:
	-rm -f $(build_shlibdir)/libjulia*
	-rm -f julia_flisp.boot julia_flisp.boot.inc
	-rm -f *.do *.o *~ *# *.$(SHLIB_EXT) *.a h2j

clean-flisp:
	-$(MAKE) -C flisp clean

clean-support:
	-$(MAKE) -C support clean

cleanall: clean clean-flisp clean-support

.PHONY: debug release clean cleanall clean-*


h2j: h2j.cpp
	@$(call PRINT_CC, $(CXX) $(CFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) $(call exec,$(LLVM_CONFIG) --cppflags) $< -o $@ \
		-lclangFrontendTool -lclangBasic -lclangLex -lclangDriver -lclangFrontend -lclangParse \
		-lclangAST -lclangSema -lclangAnalysis -lclangEdit \
		-lclangRewrite -lclangSerialization -lclangStaticAnalyzerCheckers \
		-lclangStaticAnalyzerCore -lclangStaticAnalyzerFrontend -lclangTooling \
		-lclangCodeGen -lclangARCMigrate -lclang  $(LIBS) $(LDFLAGS))
