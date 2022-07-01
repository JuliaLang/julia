SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
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
	-I$(LIBUV_INC) -I$(build_includedir) \
	-I$(JULIAHOME)/deps/valgrind
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -fvisibility=hidden -fno-common \
		 -Wno-comment -Wpointer-arith -Wundef
ifeq ($(USEGCC),1) # GCC bug #25509 (void)__attribute__((warn_unused_result))
FLAGS += -Wno-unused-result
endif
JCFLAGS += -Wold-style-definition -Wstrict-prototypes -Wc++-compat

ifeq ($(USECLANG),1)
FLAGS += -Wno-return-type-c-linkage
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
	jltypes gf typemap smallintset ast builtins module interpreter symbol \
	dlload sys init task array dump staticdata toplevel jl_uv datatype \
	simplevector runtime_intrinsics precompile jloptions \
	threading partr stackwalk gc gc-debug gc-pages gc-stacks gc-alloc-profiler method \
	jlapi signal-handling safepoint timing subtype rtutils \
	crc32c APInt-C processor ircode opaque_closure codegen-stubs coverage runtime_ccall

RT_LLVMLINK :=
CG_LLVMLINK :=

ifeq ($(JULIACODEGEN),LLVM)
CODEGEN_SRCS := codegen jitlayers aotcompile debuginfo disasm llvm-simdloop llvm-muladd \
	llvm-final-gc-lowering llvm-pass-helpers llvm-late-gc-lowering llvm-ptls \
	llvm-lower-handlers llvm-gc-invariant-verifier llvm-propagate-addrspaces \
	llvm-multiversioning llvm-alloc-opt llvm-alloc-helpers cgmemmgr llvm-remove-addrspaces \
	llvm-remove-ni llvm-julia-licm llvm-demote-float16 llvm-cpufeatures
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --includedir)
CG_LLVM_LIBS := all
ifeq ($(USE_POLLY),1)
CG_LLVMLINK += -lPolly -lPollyISL
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --src-root)/tools/polly/include
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --obj-root)/tools/polly/include
FLAGS += -DUSE_POLLY
ifeq ($(USE_POLLY_OPENMP),1)
FLAGS += -fopenmp
endif
ifeq ($(USE_POLLY_ACC),1)
CG_LLVMLINK += -lPollyPPCG -lGPURuntime
FLAGS += -DUSE_POLLY_ACC
FLAGS += -I$(shell $(LLVM_CONFIG_HOST) --src-root)/tools/polly/tools # Required to find GPURuntime/GPUJIT.h
endif
endif
else
# JULIACODEGEN != LLVM
endif

RT_LLVM_LIBS := support

ifeq ($(OS),WINNT)
SRCS += win32_ucontext
endif

ifeq ($(WITH_DTRACE),1)
DTRACE_HEADERS := uprobes.h.gen
ifneq ($(OS),Darwin)
SRCS += uprobes
endif
else
DTRACE_HEADERS :=
endif

# headers are used for dependency tracking, while public headers will be part of the dist
UV_HEADERS :=
ifeq ($(USE_SYSTEM_LIBUV),0)
UV_HEADERS += uv.h
UV_HEADERS += uv/*.h
endif
PUBLIC_HEADERS := $(BUILDDIR)/julia_version.h $(wildcard $(SRCDIR)/support/*.h) $(addprefix $(SRCDIR)/,julia.h julia_assert.h julia_threads.h julia_fasttls.h julia_locks.h julia_atomics.h jloptions.h)
ifeq ($(OS),WINNT)
PUBLIC_HEADERS += $(addprefix $(SRCDIR)/,win32_ucontext.h)
endif
HEADERS := $(PUBLIC_HEADERS) $(addprefix $(SRCDIR)/,julia_internal.h options.h timing.h passes.h) $(addprefix $(BUILDDIR)/,$(DTRACE_HEADERS) jl_internal_funcs.inc)
PUBLIC_HEADERS += $(addprefix $(SRCDIR)/,julia_gcext.h)
PUBLIC_HEADER_TARGETS := $(addprefix $(build_includedir)/julia/,$(notdir $(PUBLIC_HEADERS)) $(UV_HEADERS))

LLVM_LDFLAGS := $(shell $(LLVM_CONFIG_HOST) --ldflags)
LLVM_CXXFLAGS := $(shell $(LLVM_CONFIG_HOST) --cxxflags)

# llvm-config --cxxflags does not return -DNDEBUG
ifeq ($(shell $(LLVM_CONFIG_HOST) --assertion-mode),OFF)
LLVM_CXXFLAGS += -DNDEBUG
endif

ifeq ($(JULIACODEGEN),LLVM)
ifneq ($(USE_SYSTEM_LLVM),0)
CG_LLVMLINK += $(LLVM_LDFLAGS) $(shell $(LLVM_CONFIG_HOST) --libs --system-libs)
# HACK: llvm-config doesn't correctly point to shared libs on all platforms
#       https://github.com/JuliaLang/julia/issues/29981
else
ifneq ($(USE_LLVM_SHLIB),1)
CG_LLVMLINK += $(LLVM_LDFLAGS) $(shell $(LLVM_CONFIG_HOST) --libs $(CG_LLVM_LIBS) --link-static) $($(LLVM_LDFLAGS) $(shell $(LLVM_CONFIG_HOST) --system-libs 2> /dev/null)
else
ifeq ($(OS), Darwin)
CG_LLVMLINK += $(LLVM_LDFLAGS) -lLLVM
else
CG_LLVMLINK += $(LLVM_LDFLAGS) -lLLVM-14jl
endif
endif
endif
ifeq ($(USE_LLVM_SHLIB),1)
FLAGS += -DLLVM_SHLIB
endif # USE_LLVM_SHLIB == 1
endif

RT_LLVM_LINK_ARGS := $(shell $(LLVM_CONFIG_HOST) --libs $(RT_LLVM_LIBS) --system-libs --link-static)
RT_LLVMLINK += $(LLVM_LDFLAGS) $(RT_LLVM_LINK_ARGS)
ifeq ($(OS), WINNT)
RT_LLVMLINK += -luuid -lole32
endif

CLANG_LDFLAGS := $(LLVM_LDFLAGS)
ifeq ($(OS), Darwin)
CLANG_LDFLAGS += -Wl,-undefined,dynamic_lookup
OSLIBS += $(SRCDIR)/mach_dyld_atfork.tbd
endif

COMMON_LIBPATHS := -L$(build_libdir) -L$(build_shlibdir)
RT_LIBS := $(LIBUV) $(LIBUTF8PROC) $(NO_WHOLE_ARCHIVE) $(LIBUNWIND) $(RT_LLVMLINK) $(OSLIBS)
CG_LIBS := $(NO_WHOLE_ARCHIVE) $(LIBUV) $(LIBUNWIND) $(CG_LLVMLINK) $(OSLIBS)
RT_DEBUG_LIBS := $(COMMON_LIBPATHS) $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp-debug.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport-debug.a -ljulia-debug $(RT_LIBS)
CG_DEBUG_LIBS := $(COMMON_LIBPATHS) $(WHOLE_ARCHIVE) $(WHOLE_ARCHIVE) $(CG_LIBS) -ljulia-debug -ljulia-internal-debug
RT_RELEASE_LIBS := $(COMMON_LIBPATHS) $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport.a -ljulia $(RT_LIBS)
CG_RELEASE_LIBS := $(COMMON_LIBPATHS) $(WHOLE_ARCHIVE) $(WHOLE_ARCHIVE) $(CG_LIBS) -ljulia -ljulia-internal

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)

CODEGEN_OBJS := $(CODEGEN_SRCS:%=$(BUILDDIR)/%.o)
CODEGEN_DOBJS := $(CODEGEN_SRCS:%=$(BUILDDIR)/%.dbg.obj)

DEBUGFLAGS += $(FLAGS) -DLIBRARY_EXPORTS
SHIPFLAGS += $(FLAGS) -DLIBRARY_EXPORTS

# if not absolute, then relative to the directory of the julia executable
SHIPFLAGS  += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys.$(SHLIB_EXT)\""
DEBUGFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys-debug.$(SHLIB_EXT)\""

# Add SONAME defines so we can embed proper `dlopen()` calls.
SHIPFLAGS  += "-DJL_LIBJULIA_SONAME=\"libjulia.$(JL_MAJOR_SHLIB_EXT)\""       "-DJL_LIBJULIA_INTERNAL_SONAME=\"libjulia-internal.$(JL_MAJOR_SHLIB_EXT)\""
DEBUGFLAGS += "-DJL_LIBJULIA_SONAME=\"libjulia-debug.$(JL_MAJOR_SHLIB_EXT)\"" "-DJL_LIBJULIA_INTERNAL_SONAME=\"libjulia-internal-debug.$(JL_MAJOR_SHLIB_EXT)\""

ifeq ($(USE_CROSS_FLISP), 1)
FLISPDIR := $(BUILDDIR)/flisp/host
FLISP_EXECUTABLE_debug := $(FLISPDIR)/flisp-debug$(BUILD_EXE)
FLISP_EXECUTABLE_release := $(FLISPDIR)/flisp$(BUILD_EXE)
else
FLISPDIR := $(BUILDDIR)/flisp
FLISP_EXECUTABLE_debug := $(FLISPDIR)/flisp-debug$(EXE)
FLISP_EXECUTABLE_release := $(FLISPDIR)/flisp$(EXE)
endif
ifeq ($(OS),WINNT)
FLISP_EXECUTABLE := $(FLISP_EXECUTABLE_release)
else
FLISP_EXECUTABLE := $(FLISP_EXECUTABLE_$(JULIA_BUILD_MODE))
endif

default: $(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: debug release

release debug: %: libjulia-internal-% libjulia-codegen-%

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

LLVM_CONFIG_ABSOLUTE := $(shell which $(LLVM_CONFIG))

# Generate the DTrace header file, while also renaming the macros from
# JULIA_ to JL_PROBE to clearly delinate them.
$(BUILDDIR)/%.h.gen : $(SRCDIR)/%.d
	@$(call PRINT_DTRACE, $(DTRACE) -h -s $< -o $@)
	sed 's/JULIA_/JL_PROBE_/' $@ > $@.tmp
	mv $@.tmp $@

$(BUILDDIR)/jl_internal_funcs.inc: $(SRCDIR)/jl_exported_funcs.inc
	# Generate `.inc` file that contains a list of `#define` macros to rename functions defined in `libjulia-internal`
	# to have a `ijl_` prefix instead of `jl_`, to denote that they are coming from `libjulia-internal`.  This avoids
	# potential confusion with debugging tools, when inspecting a process that has both `libjulia` and `libjulia-internal`
	# loaded at the same time.
	grep 'XX(.\+)' $< | sed -E 's/.*XX\((.+)\).*/#define \1 i\1/g' >$@

# source file rules
$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(JCPPFLAGS) $(JCFLAGS) $(JL_CFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(JCPPFLAGS) $(JCFLAGS) $(JL_CFLAGS) $(DEBUGFLAGS) -c $< -o $@)
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp $(SRCDIR)/llvm-version.h $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(LLVM_CXXFLAGS) $(JCPPFLAGS) $(JCXXFLAGS) $(JL_CXXFLAGS) $(SHIPFLAGS) $(CXX_DISABLE_ASSERTION) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.cpp $(SRCDIR)/llvm-version.h $(HEADERS) $(LLVM_CONFIG_ABSOLUTE) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(LLVM_CXXFLAGS) $(JCPPFLAGS) $(JCXXFLAGS) $(JL_CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@)
$(BUILDDIR)/%.o : $(SRCDIR)/%.d
	@$(call PRINT_DTRACE, $(DTRACE) -G -s $< -o $@)
$(BUILDDIR)/%.dbg.obj : $(SRCDIR)/%.d
	@$(call PRINT_DTRACE, $(DTRACE) -G -s $< -o $@)

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
	@$(call PRINT_CC, $(CC) $(JCFLAGS) $(JL_CFLAGS) $(JCPPFLAGS) $(FLAGS) -O3 $< $(fPIC) -shared -o $@.tmp $(LDFLAGS))
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
	@#rm -rf $@.dSYM && mv $@.tmp.dSYM $@.dSYM
	mv $@.tmp $@
	$(INSTALL_NAME_CMD)libccalltest.$(SHLIB_EXT) $@

$(build_shlibdir)/libllvmcalltest.$(SHLIB_EXT): $(SRCDIR)/llvmcalltest.cpp $(LLVM_CONFIG_ABSOLUTE)
	@$(call PRINT_CC, $(CXX) $(LLVM_CXXFLAGS) $(FLAGS) $(CPPFLAGS) $(CXXFLAGS) -O3 $< $(fPIC) -shared -o $@ $(LDFLAGS) $(COMMON_LIBPATHS) $(NO_WHOLE_ARCHIVE) $(CG_LLVMLINK)) -lpthread

julia_flisp.boot.inc.phony: $(BUILDDIR)/julia_flisp.boot.inc

$(BUILDDIR)/julia_flisp.boot.inc: $(BUILDDIR)/julia_flisp.boot $(FLISP_EXECUTABLE_release)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE_release)) $(call cygpath_w,$(SRCDIR)/bin2hex.scm) < $< > $@)

$(BUILDDIR)/julia_flisp.boot: $(addprefix $(SRCDIR)/,jlfrontend.scm flisp/aliases.scm flisp/profile.scm \
		julia-parser.scm julia-syntax.scm match.scm utils.scm ast.scm macroexpand.scm mk_julia_flisp_boot.scm) \
		$(FLISP_EXECUTABLE_release)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE_release)) \
		$(call cygpath_w,$(SRCDIR)/mk_julia_flisp_boot.scm) $(call cygpath_w,$(dir $<)) $(notdir $<) $(call cygpath_w,$@))

# additional dependency links
$(BUILDDIR)/codegen-stubs.o $(BUILDDIR)/codegen-stubs.dbg.obj: $(SRCDIR)/intrinsics.h
$(BUILDDIR)/aotcompile.o $(BUILDDIR)/aotcompile.dbg.obj: $(SRCDIR)/jitlayers.h $(SRCDIR)/codegen_shared.h $(SRCDIR)/debug-registry.h
$(BUILDDIR)/ast.o $(BUILDDIR)/ast.dbg.obj: $(BUILDDIR)/julia_flisp.boot.inc $(SRCDIR)/flisp/*.h
$(BUILDDIR)/builtins.o $(BUILDDIR)/builtins.dbg.obj: $(SRCDIR)/iddict.c $(SRCDIR)/builtin_proto.h
$(BUILDDIR)/codegen.o $(BUILDDIR)/codegen.dbg.obj: $(addprefix $(SRCDIR)/,\
	intrinsics.cpp jitlayers.h debug-registry.h intrinsics.h codegen_shared.h cgutils.cpp ccall.cpp abi_*.cpp processor.h builtin_proto.h)
$(BUILDDIR)/debuginfo.o $(BUILDDIR)/debuginfo.dbg.obj: $(addprefix $(SRCDIR)/,debuginfo.h processor.h jitlayers.h debug-registry.h)
$(BUILDDIR)/disasm.o $(BUILDDIR)/disasm.dbg.obj: $(SRCDIR)/debuginfo.h $(SRCDIR)/processor.h
$(BUILDDIR)/dump.o $(BUILDDIR)/dump.dbg.obj: $(addprefix $(SRCDIR)/,common_symbols1.inc common_symbols2.inc builtin_proto.h serialize.h)
$(BUILDDIR)/gc-debug.o $(BUILDDIR)/gc-debug.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/gc-pages.o $(BUILDDIR)/gc-pages.dbg.obj: $(SRCDIR)/gc.h
$(BUILDDIR)/gc.o $(BUILDDIR)/gc.dbg.obj: $(SRCDIR)/gc.h $(SRCDIR)/gc-alloc-profiler.h
$(BUILDDIR)/init.o $(BUILDDIR)/init.dbg.obj: $(SRCDIR)/builtin_proto.h
$(BUILDDIR)/interpreter.o $(BUILDDIR)/interpreter.dbg.obj: $(SRCDIR)/builtin_proto.h
$(BUILDDIR)/jitlayers.o $(BUILDDIR)/jitlayers.dbg.obj: $(SRCDIR)/jitlayers.h $(SRCDIR)/codegen_shared.h $(SRCDIR)/debug-registry.h
$(BUILDDIR)/jltypes.o $(BUILDDIR)/jltypes.dbg.obj: $(SRCDIR)/builtin_proto.h
$(build_shlibdir)/libllvmcalltest.$(SHLIB_EXT): $(SRCDIR)/codegen_shared.h $(BUILDDIR)/julia_version.h
$(BUILDDIR)/llvm-alloc-helpers.o $(BUILDDIR)/llvm-alloc-helpers.dbg.obj: $(SRCDIR)/codegen_shared.h $(SRCDIR)/llvm-pass-helpers.h $(SRCDIR)/llvm-alloc-helpers.h
$(BUILDDIR)/llvm-alloc-opt.o $(BUILDDIR)/llvm-alloc-opt.dbg.obj: $(SRCDIR)/codegen_shared.h $(SRCDIR)/llvm-pass-helpers.h $(SRCDIR)/llvm-alloc-helpers.h
$(BUILDDIR)/llvm-cpufeatures.o $(BUILDDIR)/llvm-cpufeatures.dbg.obj: $(SRCDIR)/jitlayers.h $(SRCDIR)/debug-registry.h
$(BUILDDIR)/llvm-final-gc-lowering.o $(BUILDDIR)/llvm-final-gc-lowering.dbg.obj: $(SRCDIR)/llvm-pass-helpers.h $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-gc-invariant-verifier.o $(BUILDDIR)/llvm-gc-invariant-verifier.dbg.obj: $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-julia-licm.o $(BUILDDIR)/llvm-julia-licm.dbg.obj: $(SRCDIR)/codegen_shared.h $(SRCDIR)/llvm-alloc-helpers.h $(SRCDIR)/llvm-pass-helpers.h
$(BUILDDIR)/llvm-late-gc-lowering.o $(BUILDDIR)/llvm-late-gc-lowering.dbg.obj: $(SRCDIR)/llvm-pass-helpers.h $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-lower-handlers.o $(BUILDDIR)/llvm-lower-handlers.dbg.obj: $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-multiversioning.o $(BUILDDIR)/llvm-multiversioning.dbg.obj: $(SRCDIR)/codegen_shared.h $(SRCDIR)/processor.h
$(BUILDDIR)/llvm-pass-helpers.o $(BUILDDIR)/llvm-pass-helpers.dbg.obj: $(SRCDIR)/llvm-pass-helpers.h $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-propagate-addrspaces.o $(BUILDDIR)/llvm-propagate-addrspaces.dbg.obj: $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-remove-addrspaces.o $(BUILDDIR)/llvm-remove-addrspaces.dbg.obj: $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/llvm-ptls.o $(BUILDDIR)/llvm-ptls.dbg.obj: $(SRCDIR)/codegen_shared.h
$(BUILDDIR)/processor.o $(BUILDDIR)/processor.dbg.obj: $(addprefix $(SRCDIR)/,processor_*.cpp processor.h features_*.h)
$(BUILDDIR)/signal-handling.o $(BUILDDIR)/signal-handling.dbg.obj: $(addprefix $(SRCDIR)/,signals-*.c)
$(BUILDDIR)/staticdata.o $(BUILDDIR)/staticdata.dbg.obj: $(SRCDIR)/processor.h $(SRCDIR)/builtin_proto.h
$(BUILDDIR)/toplevel.o $(BUILDDIR)/toplevel.dbg.obj: $(SRCDIR)/builtin_proto.h
$(BUILDDIR)/ircode.o $(BUILDDIR)/ircode.dbg.obj: $(SRCDIR)/serialize.h

$(addprefix $(BUILDDIR)/,threading.o threading.dbg.obj gc.o gc.dbg.obj init.c init.dbg.obj task.o task.dbg.obj): $(addprefix $(SRCDIR)/,threading.h)
$(addprefix $(BUILDDIR)/,APInt-C.o APInt-C.dbg.obj runtime_intrinsics.o runtime_intrinsics.dbg.obj): $(SRCDIR)/APInt-C.h

# archive library file rules
$(BUILDDIR)/support/libsupport.a: $(addprefix $(SRCDIR)/support/,*.h *.c *.S *.inc) $(SRCDIR)/support/*.c
	$(MAKE) -C $(SRCDIR)/support BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(BUILDDIR)/support/libsupport-debug.a: $(addprefix $(SRCDIR)/support/,*.h *.c *.S *.inc) $(SRCDIR)/support/*.c
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

CXXLD = $(CXX) -shared

$(build_shlibdir)/libjulia-internal.$(JL_MAJOR_MINOR_SHLIB_EXT): $(SRCDIR)/julia.expmap $(OBJS) $(BUILDDIR)/flisp/libflisp.a $(BUILDDIR)/support/libsupport.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(call IMPLIB_FLAGS,$@) $(JCXXFLAGS) $(JL_CXXFLAGS) $(CXXLDFLAGS) $(SHIPFLAGS) $(OBJS) $(RPATH_LIB) -o $@ \
		$(JLDFLAGS) $(JLIBLDFLAGS) $(RT_RELEASE_LIBS) $(call SONAME_FLAGS,libjulia-internal.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libjulia-internal.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@

$(build_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(SRCDIR)/julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(call IMPLIB_FLAGS,$@) $(JCXXFLAGS) $(JL_CXXFLAGS) $(CXXLDFLAGS) $(DEBUGFLAGS) $(DOBJS) $(RPATH_LIB) -o $@ \
		$(JLDFLAGS) $(JLIBLDFLAGS) $(RT_DEBUG_LIBS) $(call SONAME_FLAGS,libjulia-internal-debug.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libjulia-internal-debug.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@

ifneq ($(OS), WINNT)
$(build_shlibdir)/libjulia-internal.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_SHLIB_EXT): $(build_shlibdir)/libjulia-internal%.$(JL_MAJOR_SHLIB_EXT): \
		$(build_shlibdir)/libjulia-internal%.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
$(build_shlibdir)/libjulia-internal.$(SHLIB_EXT) $(build_shlibdir)/libjulia-internal-debug.$(SHLIB_EXT): $(build_shlibdir)/libjulia-internal%.$(SHLIB_EXT): \
		$(build_shlibdir)/libjulia-internal%.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
libjulia-internal-release: $(build_shlibdir)/libjulia-internal.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-internal.$(SHLIB_EXT)
libjulia-internal-debug: $(build_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-internal-debug.$(SHLIB_EXT)
endif
libjulia-internal-release: $(build_shlibdir)/libjulia-internal.$(JL_MAJOR_MINOR_SHLIB_EXT)
libjulia-internal-debug: $(build_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_MINOR_SHLIB_EXT)
libjulia-internal-debug libjulia-internal-release: $(PUBLIC_HEADER_TARGETS)

$(build_shlibdir)/libjulia-codegen.$(JL_MAJOR_MINOR_SHLIB_EXT): $(SRCDIR)/julia.expmap $(CODEGEN_OBJS) $(BUILDDIR)/support/libsupport.a $(build_shlibdir)/libjulia-internal.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, $(CXXLD) $(call IMPLIB_FLAGS,$@) $(JCXXFLAGS) $(JL_CXXFLAGS) $(CXXLDFLAGS) $(SHIPFLAGS) $(CODEGEN_OBJS) $(RPATH_LIB) -o $@ \
		$(JLDFLAGS) $(JLIBLDFLAGS) $(CG_RELEASE_LIBS) $(call SONAME_FLAGS,libjulia-codegen.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libjulia-codegen.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@

$(build_shlibdir)/libjulia-codegen-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(SRCDIR)/julia.expmap $(CODEGEN_DOBJS) $(BUILDDIR)/support/libsupport-debug.a $(build_shlibdir)/libjulia-internal-debug.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, $(CXXLD) $(call IMPLIB_FLAGS,$@) $(JCXXFLAGS) $(JL_CXXFLAGS) $(CXXLDFLAGS) $(DEBUGFLAGS) $(CODEGEN_DOBJS) $(RPATH_LIB) -o $@ \
		$(JLDFLAGS) $(JLIBLDFLAGS) $(CG_DEBUG_LIBS) $(call SONAME_FLAGS,libjulia-codegen-debug.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libjulia-codegen-debug.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@

ifneq ($(OS), WINNT)
$(build_shlibdir)/libjulia-codegen.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-codegen-debug.$(JL_MAJOR_SHLIB_EXT): $(build_shlibdir)/libjulia-codegen%.$(JL_MAJOR_SHLIB_EXT): \
		$(build_shlibdir)/libjulia-codegen%.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
$(build_shlibdir)/libjulia-codegen.$(SHLIB_EXT) $(build_shlibdir)/libjulia-codegen-debug.$(SHLIB_EXT): $(build_shlibdir)/libjulia-codegen%.$(SHLIB_EXT): \
		$(build_shlibdir)/libjulia-codegen%.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
libjulia-codegen-release: $(build_shlibdir)/libjulia-codegen.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-codegen.$(SHLIB_EXT)
libjulia-codegen-debug: $(build_shlibdir)/libjulia-codegen-debug.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-codegen-debug.$(SHLIB_EXT)
endif
libjulia-codegen-release: $(build_shlibdir)/libjulia-codegen.$(JL_MAJOR_MINOR_SHLIB_EXT)
libjulia-codegen-debug: $(build_shlibdir)/libjulia-codegen-debug.$(JL_MAJOR_MINOR_SHLIB_EXT)
libjulia-codegen-debug libjulia-codegen-release: $(PUBLIC_HEADER_TARGETS)

clean:
	-rm -fr $(build_shlibdir)/libjulia-internal* $(build_shlibdir)/libjulia-codegen* $(build_shlibdir)/libccalltest* $(build_shlibdir)/libllvmcalltest*
	-rm -f $(BUILDDIR)/julia_flisp.boot $(BUILDDIR)/julia_flisp.boot.inc $(BUILDDIR)/jl_internal_funcs.inc
	-rm -f $(BUILDDIR)/*.dbg.obj $(BUILDDIR)/*.o $(BUILDDIR)/*.dwo $(BUILDDIR)/*.$(SHLIB_EXT) $(BUILDDIR)/*.a $(BUILDDIR)/*.h.gen
	-rm -f $(BUILDDIR)/julia_version.h

clean-flisp:
	-$(MAKE) -C $(SRCDIR)/flisp clean BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

clean-support:
	-$(MAKE) -C $(SRCDIR)/support clean BUILDDIR='$(abspath $(BUILDDIR)/support)'

cleanall: clean clean-flisp clean-support clean-analyzegc

$(build_shlibdir)/lib%Plugin.$(SHLIB_EXT): $(SRCDIR)/clangsa/%.cpp $(LLVM_CONFIG_ABSOLUTE)
	@$(call PRINT_CC, $(CXX) -g $(fPIC) -shared -o $@ -DCLANG_PLUGIN -I$(build_includedir) -L$(build_libdir) \
		$(LLVM_CXXFLAGS) $(CLANG_LDFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $(CXXLDFLAGS) $<)

# Throw an error if a proper version of `clang` is not available.
# Note that for a default install, you will need to have run the following
# before attempting this static analysis, so that all necessary headers
# and dependencies are properly installed:
#   make -C src install-analysis-deps
install-analysis-deps:
	$(MAKE) -C $(JULIAHOME)/deps install-llvm install-clang install-llvm-tools install-libuv install-utf8proc install-unwind

analyzegc-deps-check: $(BUILDDIR)/julia_version.h $(BUILDDIR)/julia_flisp.boot.inc $(BUILDDIR)/jl_internal_funcs.inc
ifeq ($(USE_BINARYBUILDER_LLVM),0)
ifneq ($(BUILD_LLVM_CLANG),1)
	$(error Clang must be available to use the clang analyzer. Either build it (BUILD_LLVM_CLANG=1) or use BinaryBuilder)
endif
endif

clangsa: $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT)
clangsa: $(build_shlibdir)/libImplicitAtomicsPlugin.$(SHLIB_EXT)

# optarg is a required_argument for these
SA_EXCEPTIONS-jloptions.c                   := -Xanalyzer -analyzer-config -Xanalyzer silence-checkers="core.NonNullParamChecker;unix.cstring.NullArg"
 # clang doesn't understand that e->vars has the same value in save_env (NULL) and restore_env (assumed non-NULL)
SA_EXCEPTIONS-subtype.c                     := -Xanalyzer -analyzer-config -Xanalyzer silence-checkers="core.uninitialized.Assign;core.UndefinedBinaryOperatorResult"
SA_EXCEPTIONS-codegen.c                     := -Xanalyzer -analyzer-config -Xanalyzer silence-checkers="core"
 # these need to be annotated (and possibly fixed)
SKIP_IMPLICIT_ATOMICS := dump.c module.c staticdata.c codegen.cpp
 # these need to be annotated (and possibly fixed)
SKIP_GC_CHECK := codegen.cpp rtutils.c

# make sure LLVM's invariant information is not discarded with -DNDEBUG
clang-sagc-%: JL_CXXFLAGS += -UNDEBUG
clang-sagc-%: $(SRCDIR)/%.c $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT) .FORCE | analyzegc-deps-check
	@$(call PRINT_ANALYZE, $(build_depsbindir)/clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-werror -Xanalyzer -analyzer-output=text --analyzer-no-default-checks \
		-Xclang -load -Xclang $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT) -Xclang -analyzer-checker=core$(COMMA)julia.GCChecker \
		$(SA_EXCEPTIONS-$(notdir $<)) \
		$(CLANGSA_FLAGS) $(JCPPFLAGS) $(JCFLAGS) $(JL_CFLAGS) $(DEBUGFLAGS) -fcolor-diagnostics -x c $<)
clang-sagc-%: $(SRCDIR)/%.cpp $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT) .FORCE | analyzegc-deps-check
	@$(call PRINT_ANALYZE, $(build_depsbindir)/clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-werror -Xanalyzer -analyzer-output=text --analyzer-no-default-checks \
		-Xclang -load -Xclang $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT) -Xclang -analyzer-checker=core$(COMMA)julia.GCChecker \
		$(SA_EXCEPTIONS-$(notdir $<)) \
		$(CLANGSA_FLAGS) $(CLANGSA_CXXFLAGS) $(LLVM_CXXFLAGS) $(JCPPFLAGS) $(JCXXFLAGS) $(JL_CXXFLAGS) $(DEBUGFLAGS) -fcolor-diagnostics -x c++ $<)

clang-sa-%: JL_CXXFLAGS += -UNDEBUG
clang-sa-%: $(SRCDIR)/%.c .FORCE | analyzegc-deps-check
	@$(call PRINT_ANALYZE, $(build_depsbindir)/clang --analyze -Xanalyzer -analyzer-werror -Xanalyzer -analyzer-output=text \
		-Xanalyzer -analyzer-disable-checker=deadcode.DeadStores \
		$(SA_EXCEPTIONS-$(notdir $<)) \
		$(CLANGSA_FLAGS) $(JCPPFLAGS) $(JCFLAGS) $(JL_CFLAGS) $(DEBUGFLAGS) -fcolor-diagnostics -Werror -x c $<)
clang-sa-%: $(SRCDIR)/%.cpp .FORCE | analyzegc-deps-check
	@$(call PRINT_ANALYZE, $(build_depsbindir)/clang --analyze -Xanalyzer -analyzer-werror -Xanalyzer -analyzer-output=text \
		-Xanalyzer -analyzer-disable-checker=deadcode.DeadStores \
		$(SA_EXCEPTIONS-$(notdir $<)) \
		$(CLANGSA_FLAGS) $(CLANGSA_CXXFLAGS) $(LLVM_CXXFLAGS) $(JCPPFLAGS) $(JCXXFLAGS) $(JL_CXXFLAGS) $(DEBUGFLAGS) -fcolor-diagnostics -Werror -x c++ $<)

clang-tidy-%: $(SRCDIR)/%.c $(build_shlibdir)/libImplicitAtomicsPlugin.$(SHLIB_EXT) .FORCE | analyzegc-deps-check
	@$(call PRINT_ANALYZE, $(build_depsbindir)/clang-tidy $< -header-filter='.*' --quiet \
		-load $(build_shlibdir)/libImplicitAtomicsPlugin.$(SHLIB_EXT) --checks='-clang-analyzer-*$(COMMA)-clang-diagnostic-*$(COMMA)concurrency-implicit-atomics' --warnings-as-errors='*' \
		-- $(CLANGSA_FLAGS) $(JCPPFLAGS) $(JCFLAGS) $(JL_CFLAGS) $(DEBUGFLAGS) -fcolor-diagnostics -fno-caret-diagnostics -x c)
clang-tidy-%: $(SRCDIR)/%.cpp $(build_shlibdir)/libImplicitAtomicsPlugin.$(SHLIB_EXT) .FORCE | analyzegc-deps-check
	@$(call PRINT_ANALYZE, $(build_depsbindir)/clang-tidy $< -header-filter='.*' --quiet \
		-load $(build_shlibdir)/libImplicitAtomicsPlugin.$(SHLIB_EXT) --checks='-clang-analyzer-*$(COMMA)-clang-diagnostic-*$(COMMA)concurrency-implicit-atomics' --warnings-as-errors='*' \
		-- $(CLANGSA_FLAGS) $(CLANGSA_CXXFLAGS) $(LLVM_CXXFLAGS) $(JCPPFLAGS) $(JCXXFLAGS) $(JL_CXXFLAGS) $(DEBUGFLAGS) -fcolor-diagnostics --system-header-prefix=llvm -Wno-deprecated-declarations -fno-caret-diagnostics -x c++)


# Add C files as a target of `analyzesrc` and `analyzegc` and `tidysrc`
tidysrc: $(addprefix clang-tidy-,$(filter-out $(basename $(SKIP_IMPLICIT_ATOMICS)),$(CODEGEN_SRCS) $(SRCS)))
analyzesrc: $(addprefix clang-sa-,$(CODEGEN_SRCS) $(SRCS))
analyzegc: $(addprefix clang-sagc-,$(filter-out $(basename $(SKIP_GC_CHECK)),$(CODEGEN_SRCS) $(SRCS)))
analyzegc: analyzesrc tidysrc # TODO: remove me (depended on by CI currently)
analyze: analyzesrc analyzegc tidysrc

clean-analyzegc:
	rm -f $(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT)
	rm -f $(build_shlibdir)/libImplicitAtomicsPlugin.$(SHLIB_EXT)

.FORCE:
.PHONY: default all debug release clean cleanall clean-* libccalltest libllvmcalltest julia_flisp.boot.inc.phony analyzegc analyzesrc .FORCE
