SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc

ifeq ($(USE_COPY_STACKS),1)
JCFLAGS += -DCOPY_STACKS
endif
override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

# -I BUILDDIR comes before -I SRCDIR so that the user can override <options.h> on a per-build-directory basis
#  for gcc/clang, suggested content is:
#  #include_next <options.h>
#  #define ARGUMENT_TO_OVERRIDE 1
FLAGS := \
	-D_GNU_SOURCE -I$(BUILDDIR) -I$(SRCDIR) \
	-I$(SRCDIR)/flisp -I$(SRCDIR)/support \
	-I$(LIBUV_INC) -I$(build_includedir) -DLIBRARY_EXPORTS \
	-I$(JULIAHOME)/deps/valgrind
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -fvisibility=hidden -fno-common
endif


SRCS := \
	jltypes gf ast builtins module interpreter \
	alloc dlload sys init task array dump toplevel jl_uv jlapi signal-handling \
	simplevector APInt-C runtime_intrinsics runtime_ccall
ifeq ($(JULIAGC),MARKSWEEP)
SRCS += gc
endif

ifeq ($(JULIACODEGEN),LLVM)
SRCS += codegen disasm debuginfo llvm-simdloop
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --includedir)
LLVM_LIBS := all
else
SRCS += anticodegen
LLVM_LIBS := support
endif

HEADERS := $(addprefix $(SRCDIR)/,julia.h julia_internal.h options.h) $(BUILDDIR)/julia_version.h $(wildcard $(SRCDIR)/support/*.h) $(LIBUV_INC)/uv.h

# In LLVM < 3.4, --ldflags includes both options and libraries, so use it both before and after --libs
# In LLVM >= 3.4, --ldflags has only options, and --system-libs has the libraries.
ifneq ($(USE_LLVM_SHLIB),1)
LLVMLINK := $(shell $(LLVM_CONFIG_HOST) --ldflags) $(shell $(LLVM_CONFIG_HOST) --libs $(LLVM_LIBS)) $(shell $(LLVM_CONFIG_HOST) --ldflags) $(shell $(LLVM_CONFIG_HOST) --system-libs 2> /dev/null)
else
ifeq ($(LLVM_USE_CMAKE),1)
LLVMLINK := $(shell $(LLVM_CONFIG_HOST) --ldflags) -lLLVM
else
LLVMLINK := $(shell $(LLVM_CONFIG_HOST) --ldflags) -lLLVM-$(shell $(LLVM_CONFIG_HOST) --version)
endif
FLAGS += -DLLVM_SHLIB
endif

COMMON_LIBS := -L$(build_shlibdir) -L$(build_libdir) $(LIBUV) $(LIBUTF8PROC) $(NO_WHOLE_ARCHIVE) $(LLVMLINK) $(OSLIBS)
DEBUG_LIBS := $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp-debug.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport-debug.a $(COMMON_LIBS)
RELEASE_LIBS := $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport.a $(COMMON_LIBS)

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

# if not absolute, then relative to the directory of the julia executable
SHIPFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys.$(SHLIB_EXT)\""
DEBUGFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys-debug.$(SHLIB_EXT)\""

FLISP_EXECUTABLE_debug := $(BUILDDIR)/flisp/flisp-debug
FLISP_EXECUTABLE_release := $(BUILDDIR)/flisp/flisp
ifeq ($(OS),WINNT)
FLISP_EXECUTABLE := $(FLISP_EXECUTABLE_release)
else
FLISP_EXECUTABLE := $(FLISP_EXECUTABLE_$(JULIA_BUILD_MODE))
endif

default: $(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: debug release

release debug: %: libjulia-%

$(BUILDDIR):
	mkdir $(BUILDDIR)

LLVM_CONFIG_ABSOLUTE := $(shell which $(LLVM_CONFIG))

$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.cpp $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@)

libccalltest: $(build_shlibdir)/libccalltest.$(SHLIB_EXT)
$(build_shlibdir)/libccalltest.$(SHLIB_EXT): $(SRCDIR)/ccalltest.c
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(CPPFLAGS) $(DEBUGFLAGS) -O3 $< $(fPIC) -shared -o $@ $(LDFLAGS))


$(BUILDDIR)/julia_flisp.boot.inc: $(BUILDDIR)/julia_flisp.boot $(FLISP_EXECUTABLE)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE)) $(call cygpath_w,$(SRCDIR)/bin2hex.scm) < $< > $@)

$(BUILDDIR)/julia_flisp.boot: $(addprefix $(SRCDIR)/,jlfrontend.scm \
		julia-parser.scm julia-syntax.scm match.scm utils.scm mk_julia_flisp_boot.scm) \
		$(FLISP_EXECUTABLE)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE)) \
		$(call cygpath_w,$(SRCDIR)/mk_julia_flisp_boot.scm) $(call cygpath_w,$(dir $<)) $(notdir $<) $(call cygpath_w,$@))

$(BUILDDIR)/ast.o $(BUILDDIR)/ast.dbg.obj: $(BUILDDIR)/julia_flisp.boot.inc $(SRCDIR)/flisp/*.h
$(BUILDDIR)/codegen.o $(BUILDDIR)/codegen.dbg.obj: $(addprefix $(SRCDIR)/,intrinsics.cpp intrinsics.h cgutils.cpp ccall.cpp abi_*.cpp)
$(BUILDDIR)/anticodegen.o $(BUILDDIR)/anticodegen.dbg.obj: $(SRCDIR)/intrinsics.h
$(BUILDDIR)/builtins.o $(BUILDDIR)/builtins.dbg.obj: $(SRCDIR)/table.c
$(BUILDDIR)/gc.o $(BUILDDIR)/gc.dbg.obj: $(SRCDIR)/gc-debug.c
$(BUILDDIR)/signal-handling.o $(BUILDDIR)/signal-handling.dbg.obj: $(addprefix $(SRCDIR)/,signals-*.c)

$(BUILDDIR)/support/libsupport.a: $(SRCDIR)/support/*.h $(SRCDIR)/support/*.c
	$(MAKE) -C $(SRCDIR)/support BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(BUILDDIR)/support/libsupport-debug.a: $(SRCDIR)/support/*.h $(SRCDIR)/support/*.c
	$(MAKE) -C $(SRCDIR)/support debug BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(FLISP_EXECUTABLE_release): $(BUILDDIR)/flisp/libflisp.a
$(BUILDDIR)/flisp/libflisp.a: $(addprefix $(SRCDIR)/,flisp/*.h flisp/*.c) $(BUILDDIR)/support/libsupport.a
	$(MAKE) -C $(SRCDIR)/flisp BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

$(FLISP_EXECUTABLE_debug): $(BUILDDIR)/flisp/libflisp-debug.a
$(BUILDDIR)/flisp/libflisp-debug.a: $(addprefix $(SRCDIR)/,flisp/*.h flisp/*.c) $(BUILDDIR)/support/libsupport-debug.a
	$(MAKE) -C $(SRCDIR)/flisp debug BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

$(BUILDDIR)/julia_version.h: $(JULIAHOME)/VERSION
	@echo "// This is an autogenerated header file" > $@.$(JULIA_BUILD_MODE).tmp
	@echo "#ifndef JULIA_VERSION_H" >> $@.$(JULIA_BUILD_MODE).tmp
	@echo "#define JULIA_VERSION_H" >> $@.$(JULIA_BUILD_MODE).tmp
	@echo "#define JULIA_VERSION_STRING" \"$(JULIA_VERSION)\" >> $@.$(JULIA_BUILD_MODE).tmp
	@echo $(JULIA_VERSION) | awk 'BEGIN {FS="[.,-]"} \
	{print "#define JULIA_VERSION_MAJOR " $$1 "\n" \
	"#define JULIA_VERSION_MINOR " $$2 "\n" \
	"#define JULIA_VERSION_PATCH " $$3 ; \
	if (NF<4) print "#define JULIA_VERSION_IS_RELEASE 1" ; else print  "#define JULIA_VERSION_IS_RELEASE 0"}' >> $@.$(JULIA_BUILD_MODE).tmp
	@echo "#endif" >> $@.$(JULIA_BUILD_MODE).tmp
	mv $@.$(JULIA_BUILD_MODE).tmp $@

ifneq ($(USEMSVC), 1)
CXXLD = $(CXX) -shared
ifeq ($(OS),WINNT)
CXXLD += -Wl,--out-implib,$(build_libdir)/$(notdir $@).a
endif
else
CXXLD = $(LD) -dll -export:jl_setjmp -export:jl_longjmp
endif

$(build_shlibdir)/libjulia-debug.$(SHLIB_EXT): $(SRCDIR)/julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(DEBUGFLAGS) $(DOBJS) $(RPATH_ORIGIN) -o $@ $(LDFLAGS) $(JLIBLDFLAGS) $(DEBUG_LIBS))
	$(INSTALL_NAME_CMD)libjulia-debug.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@
$(BUILDDIR)/libjulia-debug.a: $(SRCDIR)/julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(DOBJS))
libjulia-debug: $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT)

ifeq ($(SHLIB_EXT), so)
  SONAME := -Wl,-soname=libjulia.so
else
  SONAME :=
endif

$(build_shlibdir)/libjulia.$(SHLIB_EXT): $(SRCDIR)/julia.expmap $(OBJS) $(BUILDDIR)/flisp/libflisp.a $(BUILDDIR)/support/libsupport.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(SHIPFLAGS) $(OBJS) $(RPATH_ORIGIN) -o $@ $(LDFLAGS) $(JLIBLDFLAGS) $(RELEASE_LIBS) $(SONAME)) $(CXXLDFLAGS)
	$(INSTALL_NAME_CMD)libjulia.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@
$(BUILDDIR)/libjulia.a: julia.expmap $(OBJS) $(BUILDDIR)/flisp/libflisp.a $(BUILDDIR)/support/libsupport.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(OBJS))
libjulia-release: $(build_shlibdir)/libjulia.$(SHLIB_EXT)

clean:
	-rm -fr $(build_shlibdir)/libjulia* $(build_shlibdir)/libccalltest*
	-rm -f $(BUILDDIR)/julia_flisp.boot $(BUILDDIR)/julia_flisp.boot.inc
	-rm -f $(BUILDDIR)/*.dbg.obj $(BUILDDIR)/*.o $(BUILDDIR)/*.$(SHLIB_EXT) $(BUILDDIR)/*.a
	-rm -f $(BUILDDIR)/julia_version.h

clean-flisp:
	-$(MAKE) -C $(SRCDIR)/flisp clean BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

clean-support:
	-$(MAKE) -C $(SRCDIR)/support clean BUILDDIR='$(abspath $(BUILDDIR)/support)'


cleanall: clean clean-flisp clean-support

.PHONY: default all debug release clean cleanall clean-* libccalltest
