SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make

JCFLAGS += $(CFLAGS)
JCXXFLAGS += $(CXXFLAGS)
JCPPFLAGS += $(CPPFLAGS)
JLDFLAGS += $(LDFLAGS)

# -I BUILDDIR comes before -I SRCDIR so that the user can override <options.h> on a per-build-directory basis
#  for gcc/clang, suggested content is:
#  #include_next <options.h>
#  #define ARGUMENT_TO_OVERRIDE 1
FLAGS := \
	-D_GNU_SOURCE -I$(BUILDDIR) -I$(SRCDIR) \
	-I$(SRCDIR)/flisp -I$(SRCDIR)/support \
	-I$(build_includedir) -DLIBRARY_EXPORTS \
	-I$(JULIAHOME)/deps/valgrind
ifeq ($(DISABLE_LIBUV), 0)
FLAGS += -I$(LIBUV_INC)
endif
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -fvisibility=hidden -fno-common \
		 -Wpointer-arith -Wundef
ifeq ($(USEGCC),1) # GCC bug #25509 (void)__attribute__((warn_unused_result))
FLAGS += -Wno-unused-result
endif
JCFLAGS += -Wold-style-definition -Wstrict-prototypes -Wc++-compat
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

RUNTIME_C_SRCS := \
	jltypes gf typemap ast builtins module interpreter symbol \
	dlload sys init task array dump staticdata toplevel datatype \
	simplevector runtime_intrinsics precompile \
	threading partr stackwalk gc gc-debug gc-pages gc-stacks method \
	jlapi signal-handling safepoint jloptions timing subtype \
	crc32c
RUNTIME_SRCS := APInt-C runtime_ccall processor rtutils $(RUNTIME_C_SRCS)
SRCS := $(RUNTIME_SRCS)

ifeq ($(USEMSVC), 1)
SRCS += getopt
endif

ifeq ($(DISABLE_LIBUV),0)
SRCS += jl_uv
else
SRCS += jl_antiuv
endif

ifneq ($(JULIA_THREADS), 0)
SRCS += threadgroup
endif

LLVMLINK :=

ifeq ($(JULIACODEGEN),LLVM)
SRCS += codegen jitlayers disasm debuginfo llvm-simdloop llvm-ptls llvm-muladd \
	llvm-final-gc-lowering llvm-pass-helpers llvm-late-gc-lowering \
	llvm-lower-handlers llvm-gc-invariant-verifier llvm-propagate-addrspaces \
	llvm-multiversioning llvm-alloc-opt cgmemmgr llvm-api
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
UV_HEADERS :=
HEADERS := $(BUILDDIR)/julia_version.h $(wildcard $(SRCDIR)/support/*.h) $(addprefix $(SRCDIR)/,julia.h julia_assert.h julia_threads.h tls.h locks.h atomics.h julia_internal.h options.h timing.h)
PUBLIC_HEADERS := $(BUILDDIR)/julia_version.h $(wildcard $(SRCDIR)/support/*.h) $(addprefix $(SRCDIR)/,julia.h julia_assert.h julia_threads.h tls.h locks.h atomics.h julia_gcext.h)
ifeq ($(USE_SYSTEM_LIBUV),0)
ifeq ($(DISABLE_LIBUV),0)
UV_HEADERS += uv.h
UV_HEADERS += uv/*.h
endif
endif
PUBLIC_HEADER_TARGETS := $(addprefix $(build_includedir)/julia/,$(notdir $(PUBLIC_HEADERS)) $(UV_HEADERS))

ifeq ($(JULIACODEGEN),LLVM)
ifneq ($(USE_SYSTEM_LLVM),0)
LLVMLINK += $(shell $(LLVM_CONFIG_HOST) --ldflags --libs --system-libs)
# HACK: llvm-config doesn't correctly point to shared libs on all platforms
#       https://github.com/JuliaLang/julia/issues/29981
else
ifneq ($(USE_LLVM_SHLIB),1)
LLVMLINK += $(shell $(LLVM_CONFIG_HOST) --ldflags) $(shell $(LLVM_CONFIG_HOST) --libs $(LLVM_LIBS)) $(shell $(LLVM_CONFIG_HOST) --ldflags) $(shell $(LLVM_CONFIG_HOST) --system-libs 2> /dev/null)
else
LLVMLINK += $(shell $(LLVM_CONFIG_HOST) --ldflags) -lLLVM
endif
endif
ifeq ($(USE_LLVM_SHLIB),1)
FLAGS += -DLLVM_SHLIB
endif # USE_LLVM_SHLIB == 1
endif

COMMON_LIBS := -L$(build_shlibdir) -L$(build_libdir) $(LIBUV) $(LIBUTF8PROC) $(NO_WHOLE_ARCHIVE) $(LLVMLINK) $(OSLIBS) $(LIBUNWIND)
DEBUG_LIBS := $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp-debug.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport-debug.a $(COMMON_LIBS)
RELEASE_LIBS := $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport.a $(COMMON_LIBS)

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

# if not absolute, then relative to the directory of the julia executable
SHIPFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys.$(SHLIB_EXT)\""
DEBUGFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys-debug.$(SHLIB_EXT)\""

ifeq ($(USE_CROSS_FLISP), 1)
FLISPDIR := $(BUILDDIR)/flisp/host
else
FLISPDIR := $(BUILDDIR)/flisp
endif
FLISP_EXECUTABLE_debug := $(FLISPDIR)/flisp-debug$(EXE)
FLISP_EXECUTABLE_release := $(FLISPDIR)/flisp$(EXE)
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
	@$(call PRINT_CC, $(CC) $(JCPPFLAGS) $(JCFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(JCPPFLAGS) $(JCFLAGS) $(DEBUGFLAGS) -c $< -o $@)
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp $(SRCDIR)/llvm-version.h $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(JCPPFLAGS) $(JCXXFLAGS) $(SHIPFLAGS) $(CXX_DISABLE_ASSERTION) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.cpp $(SRCDIR)/llvm-version.h $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(JCPPFLAGS) $(JCXXFLAGS) $(DEBUGFLAGS) -c $< -o $@)

# public header rules
$(eval $(call dir_target,$(build_includedir)/julia))
define public_header_target
$$(build_includedir)/julia/$$(notdir $(1)): $(1) | $$(build_includedir)/julia
	$$(INSTALL_F) $$^ $$(build_includedir)/julia/
endef
$(foreach HEADER,$(PUBLIC_HEADERS) $(LIBUV_INC)/uv.h,$(eval $(call public_header_target,$(HEADER))))

$(eval $(call dir_target,$(build_includedir)/julia/uv))
$(build_includedir)/julia/uv/*.h: $(LIBUV_INC)/uv/*.h | $(build_includedir)/julia/uv
	$(INSTALL_F) $^ $(build_includedir)/julia/uv

libccalltest: $(build_shlibdir)/libccalltest.$(SHLIB_EXT)
libllvmcalltest: $(build_shlibdir)/libllvmcalltest.$(SHLIB_EXT)

ifeq ($(OS), Linux)
JULIA_SPLITDEBUG := 1
else
JULIA_SPLITDEBUG := 0
endif
$(build_shlibdir)/libccalltest.$(SHLIB_EXT): $(SRCDIR)/ccalltest.c
	@$(call PRINT_CC, $(CC) $(JCFLAGS) $(JCPPFLAGS) $(DEBUGFLAGS) -O3 $< $(fPIC) -shared -o $@.tmp $(JLDFLAGS))
	$(INSTALL_NAME_CMD)libccalltest.$(SHLIB_EXT) $@.tmp
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
	$(INSTALL_NAME_CMD)libccalltest.$(SHLIB_EXT) $@

$(build_shlibdir)/libllvmcalltest.$(SHLIB_EXT): $(SRCDIR)/llvmcalltest.cpp $(LLVM_CONFIG_ABSOLUTE)
	@$(call PRINT_CC, $(CXX) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(JCXXFLAGS) $(JCPPFLAGS) $(DEBUGFLAGS) -O3 $< $(fPIC) -shared -o $@ $(JLDFLAGS) -L$(build_shlibdir) -L$(build_libdir) $(NO_WHOLE_ARCHIVE) $(LLVMLINK))

julia_flisp.boot.inc.phony: $(BUILDDIR)/julia_flisp.boot.inc

$(BUILDDIR)/julia_flisp.boot.inc: $(BUILDDIR)/julia_flisp.boot $(FLISP_EXECUTABLE_release)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE_release)) --boot $(SRCDIR)/flisp/flisp.boot $(call cygpath_w,$(SRCDIR)/bin2hex.scm) < $< > $@)

$(BUILDDIR)/julia_flisp.boot: $(addprefix $(SRCDIR)/,jlfrontend.scm flisp/aliases.scm flisp/profile.scm \
		julia-parser.scm julia-syntax.scm match.scm utils.scm ast.scm macroexpand.scm mk_julia_flisp_boot.scm) \
		$(FLISP_EXECUTABLE_release)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE_release)) --boot $(SRCDIR)/flisp/flisp.boot \
		$(call cygpath_w,$(SRCDIR)/mk_julia_flisp_boot.scm) $(call cygpath_w,$(dir $<)) $(notdir $<) $(call cygpath_w,$@))

# additional dependency links
$(BUILDDIR)/ast.o $(BUILDDIR)/ast.dbg.obj: $(BUILDDIR)/julia_flisp.boot.inc $(SRCDIR)/flisp/*.h
$(BUILDDIR)/codegen.o $(BUILDDIR)/codegen.dbg.obj: $(addprefix $(SRCDIR)/,\
	intrinsics.cpp jitlayers.h intrinsics.h codegen_shared.h cgutils.cpp ccall.cpp abi_*.cpp processor.h)
$(BUILDDIR)/processor.o $(BUILDDIR)/processor.dbg.obj: $(addprefix $(SRCDIR)/,processor_*.cpp processor.h features_*.h)
$(BUILDDIR)/interpreter.o $(BUILDDIR)/interpreter.dbg.obj: $(SRCDIR)/interpreter-stacktrace.c
$(BUILDDIR)/anticodegen.o $(BUILDDIR)/anticodegen.dbg.obj: $(SRCDIR)/intrinsics.h
$(BUILDDIR)/debuginfo.o $(BUILDDIR)/debuginfo.dbg.obj: \
	$(addprefix $(SRCDIR)/,debuginfo.h processor.h)
$(BUILDDIR)/disasm.o $(BUILDDIR)/disasm.dbg.obj: $(SRCDIR)/debuginfo.h $(SRCDIR)/processor.h
$(BUILDDIR)/jitlayers.o $(BUILDDIR)/jitlayers.dbg.obj: $(SRCDIR)/jitlayers.h
$(BUILDDIR)/builtins.o $(BUILDDIR)/builtins.dbg.obj: $(SRCDIR)/table.c
$(BUILDDIR)/staticdata.o $(BUILDDIR)/staticdata.dbg.obj: $(SRCDIR)/processor.h
$(BUILDDIR)/llvm-final-gc-lowering.o $(BUILDDIR)/llvm-final-gc-lowering.dbg.obj: $(SRCDIR)/llvm-pass-helpers.h
$(BUILDDIR)/llvm-late-gc-lowering.o $(BUILDDIR)/llvm-late-gc-lowering.dbg.obj: $(SRCDIR)/llvm-pass-helpers.h
$(BUILDDIR)/llvm-pass-helpers.o $(BUILDDIR)/llvm-pass-helpers.dbg.obj: $(SRCDIR)/llvm-pass-helpers.h
$(BUILDDIR)/gc.o $(BUILDDIR)/gc.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/gc-debug.o $(BUILDDIR)/gc-debug.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/gc-pages.o $(BUILDDIR)/gc-pages.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/signal-handling.o $(BUILDDIR)/signal-handling.dbg.obj: $(addprefix $(SRCDIR)/,signals-*.c)
$(BUILDDIR)/dump.o $(BUILDDIR)/dump.dbg.obj: $(addprefix $(SRCDIR)/,common_symbols1.inc common_symbols2.inc)
$(addprefix $(BUILDDIR)/,threading.o threading.dbg.obj gc.o gc.dbg.obj init.c init.dbg.obj task.o task.dbg.obj): $(addprefix $(SRCDIR)/,threading.h)
$(addprefix $(BUILDDIR)/,APInt-C.o APInt-C.dbg.obj runtime_intrinsics.o runtime_intrinsics.dbg.obj): $(SRCDIR)/APInt-C.h

# archive library file rules
$(BUILDDIR)/support/libsupport.a: $(addprefix $(SRCDIR)/support/,*.h *.c *.S) $(SRCDIR)/support/*.c
	$(MAKE) -C $(SRCDIR)/support BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(BUILDDIR)/support/libsupport-debug.a: $(addprefix $(SRCDIR)/support/,*.h *.c *.S) $(SRCDIR)/support/*.c
	$(MAKE) -C $(SRCDIR)/support debug BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(FLISP_EXECUTABLE_release): $(BUILDDIR)/flisp/libflisp.a
	$(MAKE) -C $(BUILDDIR)/flisp $(subst $(abspath $(BUILDDIR)/flisp)/,,$(abspath $(FLISP_EXECUTABLE_release)))

$(FLISP_EXECUTABLE_debug): $(BUILDDIR)/flisp/libflisp-debug.a
	$(MAKE) -C $(BUILDDIR)/flisp $(subst $(abspath $(BUILDDIR)/flisp)/,,$(abspath $(FLISP_EXECUTABLE_debug)))

$(BUILDDIR)/flisp/libflisp.a: $(addprefix $(SRCDIR)/flisp/,*.h *.c) $(BUILDDIR)/support/libsupport.a
	$(MAKE) -C $(SRCDIR)/flisp BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

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
	@$(call PRINT_LINK, $(CXXLD) $(JCXXFLAGS) $(CXXLDFLAGS) $(DEBUGFLAGS) $(DOBJS) $(RPATH_LIB) -o $@ $(JLDFLAGS) $(JLIBLDFLAGS) $(DEBUG_LIBS) $(SONAME_DEBUG))
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
	@$(call PRINT_LINK, $(CXXLD) $(JCXXFLAGS) $(CXXLDFLAGS) $(SHIPFLAGS) $(OBJS) $(RPATH_LIB) -o $@ $(JLDFLAGS) $(JLIBLDFLAGS) $(RELEASE_LIBS) $(SONAME))
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
	-rm -fr $(build_shlibdir)/libjulia* $(build_shlibdir)/libccalltest* $(build_shlibdir)/libllvmcalltest*
	-rm -f $(BUILDDIR)/julia_flisp.boot $(BUILDDIR)/julia_flisp.boot.inc
	-rm -f $(BUILDDIR)/*.dbg.obj $(BUILDDIR)/*.o $(BUILDDIR)/*.dwo $(BUILDDIR)/*.$(SHLIB_EXT) $(BUILDDIR)/*.a
	-rm -f $(BUILDDIR)/julia_version.h

clean-flisp:
	-$(MAKE) -C $(SRCDIR)/flisp clean BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

clean-support:
	-$(MAKE) -C $(SRCDIR)/support clean BUILDDIR='$(abspath $(BUILDDIR)/support)'

cleanall: clean clean-flisp clean-support clean-analyzegc

$(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT): $(SRCDIR)/clangsa/GCChecker.cpp
	@$(call PRINT_CC, $(CXX) -g -shared -o $@ -DCLANG_PLUGIN -I$(build_includedir) $(shell $(LLVM_CONFIG_HOST) --cxxflags) $(CPPFLAGS) $(CFLAGS) $< $(shell $(LLVM_CONFIG_HOST) --ldflags) $(LDFLAGS) -L$(build_libdir) -lclangAnalysis -lclangStaticAnalyzerCore -lclangASTMatchers -lclangAST -lclangLex -lclangBasic)

# Throw an error if a proper version of `clang` is not available.
# Note that for a default install, you will need to have run the following
# before attempting this static analysis, so that all necessary headers
# and dependencies are properly installed:
#   make -c deps install-llvm install-libuv install-utf8proc install-unwind
analyzegc-deps-check: $(BUILDDIR)/julia_version.h $(BUILDDIR)/julia_flisp.boot.inc
ifeq ($(USE_BINARYBUILDER_LLVM),0)
ifneq ($(BUILD_LLVM_CLANG),1)
	$(error Clang must be available to use the clang analyzer. Either build it (BUILD_LLVM_CLANG) or use BinaryBuilder)
endif
endif


define CLANG_ANALYZE
clang-sa-$(1): $$(build_shlibdir)/libGCCheckerPlugin.$$(SHLIB_EXT) | analyzegc-deps-check
	@$$(call PRINT_ANALYZE, $$(build_depsbindir)/clang --analyze -Xanalyzer -analyzer-werror -Xanalyzer -analyzer-output=text -Xclang -load -Xclang $$(build_shlibdir)/libGCCheckerPlugin.$$(SHLIB_EXT) $$(CPPFLAGS) $$(CFLAGS) $$(DEBUGFLAGS) -Xclang -analyzer-checker=core$$(COMMA)julia.GCChecker --analyzer-no-default-checks -fcolor-diagnostics -Werror -x c $$(SRCDIR)/$(1).c)
.PHONY: clang-sa-$(1)

# Add this as a target of `analyzegc`
analyzegc: clang-sa-$(1)
endef

# Build a Makefile target for each file we want to analyze
$(foreach S,$(RUNTIME_C_SRCS),$(eval $(call CLANG_ANALYZE,$(S))))

clean-analyzegc:
	rm -f $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT)

.PHONY: default all debug release clean cleanall clean-* libccalltest libllvmcalltest julia_flisp.boot.inc.phony analyzegc
