SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make

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
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -fvisibility=hidden -fno-common \
		 -Wpointer-arith -Wundef
override CFLAGS += -Wold-style-definition -Wstrict-prototypes -Wc++-compat
endif

FLAGS += -DJL_BUILD_ARCH='"$(ARCH)"'
ifeq ($(OS),WINNT)
FLAGS += -DJL_BUILD_UNAME='"NT"'
else
FLAGS += -DJL_BUILD_UNAME='"$(OS)"'
endif

ifeq ($(OS),FreeBSD)
FLAGS += -I$(LOCALBASE)/include
endif

SRCS := \
	jltypes gf typemap ast builtins module interpreter symbol \
	dlload sys init task array dump staticdata toplevel jl_uv datatype \
	simplevector APInt-C runtime_intrinsics runtime_ccall precompile \
	threadgroup threading stackwalk gc gc-debug gc-pages method \
	jlapi signal-handling safepoint jloptions timing subtype rtutils \
	crc32c processor

ifeq ($(USEMSVC), 1)
SRCS += getopt
endif

LLVMLINK :=

ifeq ($(JULIACODEGEN),LLVM)
SRCS += codegen jitlayers disasm debuginfo llvm-simdloop llvm-ptls llvm-muladd \
	llvm-late-gc-lowering llvm-lower-handlers llvm-gc-invariant-verifier \
	llvm-propagate-addrspaces llvm-alloc-opt cgmemmgr
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --includedir)
LLVM_LIBS := all
ifeq ($(USE_POLLY),1)
LLVMLINK += -lPolly -lPollyISL
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --src-root)/tools/polly/include
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --obj-root)/tools/polly/include
FLAGS += -DUSE_POLLY
ifeq ($(USE_POLLY_OPENMP),1)
FLAGS += -fopenmp
endif
ifeq ($(USE_POLLY_ACC),1)
LLVMLINK += -lPollyPPCG -lGPURuntime
FLAGS += -DUSE_POLLY_ACC
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --src-root)/tools/polly/tools # Required to find GPURuntime/GPUJIT.h
endif
endif
else
SRCS += anticodegen
LLVM_LIBS := support
endif

# headers are used for dependency tracking, while public headers will be part of the dist
HEADERS := $(BUILDDIR)/julia_version.h $(wildcard $(SRCDIR)/support/*.h) $(addprefix $(SRCDIR)/,julia.h julia_assert.h julia_threads.h tls.h locks.h atomics.h julia_internal.h options.h timing.h)
PUBLIC_HEADERS := $(BUILDDIR)/julia_version.h $(wildcard $(SRCDIR)/support/*.h) $(addprefix $(SRCDIR)/,julia.h julia_assert.h julia_threads.h tls.h locks.h atomics.h)
ifeq ($(USE_SYSTEM_LIBUV),0)
ifeq ($(OS),WINNT)
HEADERS += $(build_includedir)/tree.h
PUBLIC_HEADERS += $(build_includedir)/tree.h
endif
HEADERS += $(LIBUV_INC)/uv.h
PUBLIC_HEADERS += $(LIBUV_INC)/uv*
endif
PUBLIC_HEADER_TARGETS := $(addprefix $(build_includedir)/julia/,$(notdir $(PUBLIC_HEADERS)))

# In LLVM < 3.4, --ldflags includes both options and libraries, so use it both before and after --libs
# In LLVM >= 3.4, --ldflags has only options, and --system-libs has the libraries.
ifneq ($(USE_LLVM_SHLIB),1)
LLVMLINK += $(shell $(LLVM_CONFIG_HOST) --ldflags) $(shell $(LLVM_CONFIG_HOST) --libs $(LLVM_LIBS)) $(shell $(LLVM_CONFIG_HOST) --ldflags) $(shell $(LLVM_CONFIG_HOST) --system-libs 2> /dev/null)
else
LLVMLINK += $(shell $(LLVM_CONFIG_HOST) --ldflags) -lLLVM
FLAGS += -DLLVM_SHLIB
endif # USE_LLVM_SHLIB == 1

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
	mkdir -p $(BUILDDIR)

LLVM_CONFIG_ABSOLUTE := $(shell which $(LLVM_CONFIG))

# source file rules
$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp $(SRCDIR)/llvm-version.h $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) $(CXX_DISABLE_ASSERTION) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.cpp $(SRCDIR)/llvm-version.h $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@)

# public header rules
$(eval $(call dir_target,$(build_includedir)/julia))
define public_header_target
TARGET = $(build_includedir)/julia/$(notdir $(1))
$$(TARGET): $(1) | $(build_includedir)/julia
	$(INSTALL_F) $$^ $(build_includedir)/julia/
endef
$(foreach HEADER,$(PUBLIC_HEADERS),$(eval $(call public_header_target,$(HEADER))))

libccalltest: $(build_shlibdir)/libccalltest.$(SHLIB_EXT)

ifeq ($(OS), Linux)
JULIA_SPLITDEBUG := 1
else
JULIA_SPLITDEBUG := 0
endif
$(build_shlibdir)/libccalltest.$(SHLIB_EXT): $(SRCDIR)/ccalltest.c
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(CPPFLAGS) $(DEBUGFLAGS) -O3 $< $(fPIC) -shared -o $@.tmp $(LDFLAGS))
ifeq ($(JULIA_SPLITDEBUG),1)
	@# Create split debug info file for libccalltest stacktraces test
	@# packagers should disable this by setting JULIA_SPLITDEBUG=0 if this is already done by your build system
	$(OBJCOPY) --only-keep-debug $@.tmp $@.debug
	$(OBJCOPY) --strip-debug $@.tmp
	$(OBJCOPY) --add-gnu-debuglink=$@.debug $@.tmp
endif
	@## clang should have made the dSYM split-debug directory,
	@## but we are intentionally not going to give it the correct name
	@## because we want to test the non-default debug configuration
	@#rm -r $@.dSYM && mv $@.tmp.dSYM $@.dSYM
	mv $@.tmp $@

julia_flisp.boot.inc.phony: $(BUILDDIR)/julia_flisp.boot.inc

$(BUILDDIR)/julia_flisp.boot.inc: $(BUILDDIR)/julia_flisp.boot $(FLISP_EXECUTABLE_release)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE_release)) $(call cygpath_w,$(SRCDIR)/bin2hex.scm) < $< > $@)

$(BUILDDIR)/julia_flisp.boot: $(addprefix $(SRCDIR)/,jlfrontend.scm flisp/aliases.scm flisp/profile.scm \
		julia-parser.scm julia-syntax.scm match.scm utils.scm ast.scm macroexpand.scm mk_julia_flisp_boot.scm) \
		$(FLISP_EXECUTABLE_release)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE_release)) \
		$(call cygpath_w,$(SRCDIR)/mk_julia_flisp_boot.scm) $(call cygpath_w,$(dir $<)) $(notdir $<) $(call cygpath_w,$@))

# additional dependency links
$(BUILDDIR)/ast.o $(BUILDDIR)/ast.dbg.obj: $(BUILDDIR)/julia_flisp.boot.inc $(SRCDIR)/flisp/*.h
$(BUILDDIR)/codegen.o $(BUILDDIR)/codegen.dbg.obj: $(addprefix $(SRCDIR)/,\
	intrinsics.cpp jitlayers.h intrinsics.h debuginfo.h codegen_shared.h cgutils.cpp ccall.cpp abi_*.cpp)
$(BUILDDIR)/processor.o $(BUILDDIR)/processor.dbg.obj: $(addprefix $(SRCDIR)/,processor_*.cpp processor.h features_*.h)
$(BUILDDIR)/anticodegen.o $(BUILDDIR)/anticodegen.dbg.obj: $(SRCDIR)/intrinsics.h
$(BUILDDIR)/debuginfo.o $(BUILDDIR)/debuginfo.dbg.obj: $(SRCDIR)/debuginfo.h
$(BUILDDIR)/disasm.o $(BUILDDIR)/disasm.dbg.obj: $(SRCDIR)/debuginfo.h $(SRCDIR)/processor.h
$(BUILDDIR)/jitlayers.o $(BUILDDIR)/jitlayers.dbg.obj: $(SRCDIR)/jitlayers.h
$(BUILDDIR)/builtins.o $(BUILDDIR)/builtins.dbg.obj: $(SRCDIR)/table.c
$(BUILDDIR)/gc.o $(BUILDDIR)/gc.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/gc-debug.o $(BUILDDIR)/gc-debug.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/gc-pages.o $(BUILDDIR)/gc-pages.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/signal-handling.o $(BUILDDIR)/signal-handling.dbg.obj: $(addprefix $(SRCDIR)/,signals-*.c)
$(BUILDDIR)/dump.o $(BUILDDIR)/dump.dbg.obj: $(addprefix $(SRCDIR)/,common_symbols1.inc common_symbols2.inc)
$(addprefix $(BUILDDIR)/,threading.o threading.dbg.obj gc.o gc.dbg.obj init.c init.dbg.obj task.o task.dbg.obj): $(addprefix $(SRCDIR)/,threading.h threadgroup.h)
$(addprefix $(BUILDDIR)/,APInt-C.o APInt-C.dbg.obj runtime_intrinsics.o runtime_intrinsics.dbg.obj): $(SRCDIR)/APInt-C.h

# archive library file rules
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
	@echo "#ifndef JL_VERSION_H" >> $@.$(JULIA_BUILD_MODE).tmp
	@echo "#define JL_VERSION_H" >> $@.$(JULIA_BUILD_MODE).tmp
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

# If we're on windows, don't do versioned shared libraries.  If we're on OSX,
# put the version number before the .dylib.  Otherwise, put it after.
ifeq ($(OS), WINNT)
JL_MAJOR_MINOR_SHLIB_EXT := $(SHLIB_EXT)
else
ifeq ($(OS), Darwin)
JL_MAJOR_MINOR_SHLIB_EXT := $(SOMAJOR).$(SOMINOR).$(SHLIB_EXT)
JL_MAJOR_SHLIB_EXT := $(SOMAJOR).$(SHLIB_EXT)
else
JL_MAJOR_MINOR_SHLIB_EXT := $(SHLIB_EXT).$(SOMAJOR).$(SOMINOR)
JL_MAJOR_SHLIB_EXT := $(SHLIB_EXT).$(SOMAJOR)
endif
endif

ifeq ($(SHLIB_EXT), so)
  SONAME       := -Wl,-soname=libjulia.$(JL_MAJOR_SHLIB_EXT)
  SONAME_DEBUG := -Wl,-soname=libjulia-debug.$(JL_MAJOR_SHLIB_EXT)
else
  SONAME       :=
  SONAME_DEBUG :=
endif

$(build_shlibdir)/libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(SRCDIR)/julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(DEBUGFLAGS) $(DOBJS) $(RPATH_LIB) -o $@ $(LDFLAGS) $(JLIBLDFLAGS) $(DEBUG_LIBS) $(SONAME_DEBUG))
	$(INSTALL_NAME_CMD)libjulia-debug.$(SHLIB_EXT) $@
ifneq ($(OS), WINNT)
	@ln -sf libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT) $(build_shlibdir)/libjulia-debug.$(JL_MAJOR_SHLIB_EXT)
	@ln -sf libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT) $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT)
endif
	$(DSYMUTIL) $@

$(BUILDDIR)/libjulia-debug.a: $(SRCDIR)/julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(DOBJS))

libjulia-debug: $(build_shlibdir)/libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT) $(PUBLIC_HEADER_TARGETS)

$(build_shlibdir)/libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT): $(SRCDIR)/julia.expmap $(OBJS) $(BUILDDIR)/flisp/libflisp.a $(BUILDDIR)/support/libsupport.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(SHIPFLAGS) $(OBJS) $(RPATH_LIB) -o $@ $(LDFLAGS) $(JLIBLDFLAGS) $(RELEASE_LIBS) $(SONAME))
	$(INSTALL_NAME_CMD)libjulia.$(SHLIB_EXT) $@
ifneq ($(OS), WINNT)
	@ln -sf libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT) $(build_shlibdir)/libjulia.$(JL_MAJOR_SHLIB_EXT)
	@ln -sf libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT) $(build_shlibdir)/libjulia.$(SHLIB_EXT)
endif
	$(DSYMUTIL) $@

$(BUILDDIR)/libjulia.a: julia.expmap $(OBJS) $(BUILDDIR)/flisp/libflisp.a $(BUILDDIR)/support/libsupport.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(OBJS))
libjulia-release: $(build_shlibdir)/libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT) $(PUBLIC_HEADER_TARGETS)

clean:
	-rm -fr $(build_shlibdir)/libjulia* $(build_shlibdir)/libccalltest*
	-rm -f $(BUILDDIR)/julia_flisp.boot $(BUILDDIR)/julia_flisp.boot.inc
	-rm -f $(BUILDDIR)/*.dbg.obj $(BUILDDIR)/*.o $(BUILDDIR)/*.dwo $(BUILDDIR)/*.$(SHLIB_EXT) $(BUILDDIR)/*.a
	-rm -f $(BUILDDIR)/julia_version.h

clean-flisp:
	-$(MAKE) -C $(SRCDIR)/flisp clean BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

clean-support:
	-$(MAKE) -C $(SRCDIR)/support clean BUILDDIR='$(abspath $(BUILDDIR)/support)'


cleanall: clean clean-flisp clean-support

.PHONY: default all debug release clean cleanall clean-* libccalltest julia_flisp.boot.inc.phony
