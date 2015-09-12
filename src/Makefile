JULIAHOME = $(abspath ..)
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

BUILDDIR ?= .

SRCS = \
	jltypes gf ast builtins module codegen disasm debuginfo interpreter \
	alloc dlload sys init task array dump toplevel jl_uv jlapi signal-handling \
	llvm-simdloop simplevector

HEADERS = julia.h julia_internal.h julia_version.h options.h $(wildcard support/*.h) $(LIBUV_INC)/uv.h

FLAGS = \
	-D_GNU_SOURCE -Iflisp -Isupport \
	-I$(call exec,$(LLVM_CONFIG) --includedir) \
	-I$(LIBUV_INC) -I$(build_includedir) -DLIBRARY_EXPORTS \
	-I$(BUILDDIR) -I$(JULIAHOME)/deps/valgrind
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -fvisibility=hidden -fno-common
endif

# In LLVM < 3.4, --ldflags includes both options and libraries, so use it both before and after --libs
# In LLVM >= 3.4, --ldflags has only options, and --system-libs has the libraries.
LLVMLINK = $(call exec,$(LLVM_CONFIG) --ldflags) $(call exec,$(LLVM_CONFIG) --libs) $(call exec,$(LLVM_CONFIG) --ldflags) $(call exec,$(LLVM_CONFIG) --system-libs 2> /dev/null)
ifeq ($(USE_LLVM_SHLIB),1)
ifeq ($(LLVM_USE_CMAKE),1)
LLVMLINK = $(call exec,$(LLVM_CONFIG) --ldflags) -lLLVM
else
LLVMLINK = $(call exec,$(LLVM_CONFIG) --ldflags) -lLLVM-$(call exec,$(LLVM_CONFIG) --version)
endif
FLAGS += -DLLVM_SHLIB
endif

COMMON_LIBS = -L$(build_shlibdir) -L$(build_libdir) $(LIBUV) $(LIBUTF8PROC) $(NO_WHOLE_ARCHIVE) $(LLVMLINK) $(OSLIBS)
DEBUG_LIBS = $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp-debug.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport-debug.a $(COMMON_LIBS)
RELEASE_LIBS = $(WHOLE_ARCHIVE) $(BUILDDIR)/flisp/libflisp.a $(WHOLE_ARCHIVE) $(BUILDDIR)/support/libsupport.a $(COMMON_LIBS)

OBJS = $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS = $(SRCS:%=$(BUILDDIR)/%.dbg.obj)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

# if not absolute, then relative to the directory of the julia executable
SHIPFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys.$(SHLIB_EXT)\""
DEBUGFLAGS += "-DJL_SYSTEM_IMAGE_PATH=\"$(build_private_libdir_rel)/sys-debug.$(SHLIB_EXT)\""

ifeq ($(JULIAGC),MARKSWEEP)
SRCS += gc
endif

ifeq ($(USE_COPY_STACKS),1)
JCFLAGS += -DCOPY_STACKS
endif

FLISP_EXECUTABLE_debug = $(BUILDDIR)/flisp/flisp-debug
FLISP_EXECUTABLE_release = $(BUILDDIR)/flisp/flisp
ifeq ($(OS),WINNT)
FLISP_EXECUTABLE = $(FLISP_EXECUTABLE_release)
else
FLISP_EXECUTABLE = $(FLISP_EXECUTABLE_$(JULIA_BUILD_MODE))
endif

default: $(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: debug release

release debug: %: libjulia-%

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BUILDDIR)/%.o: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)
$(BUILDDIR)/%.o: %.cpp $(HEADERS) $(shell which $(LLVM_CONFIG)) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(call exec,$(LLVM_CONFIG) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: %.cpp $(HEADERS) $(shell which $(LLVM_CONFIG)) | $(BUILDDIR)
	@$(call PRINT_CC, $(CXX) $(call exec,$(LLVM_CONFIG) --cxxflags) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@)

libccalltest: $(build_shlibdir)/libccalltest.$(SHLIB_EXT)
$(build_shlibdir)/libccalltest.$(SHLIB_EXT): ccalltest.c
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(CPPFLAGS) $(DEBUGFLAGS) -O3 $< $(fPIC) -shared -o $@ $(LDFLAGS) -DCC="$(CC)")
$(BUILDDIR)/ccalltest: ccalltest.c $(BUILDDIR)/libccalltest.$(SHLIB_EXT)
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -o $@ $(LDFLAGS) -DCC="$(CC)")



$(BUILDDIR)/julia_flisp.boot.inc: $(BUILDDIR)/julia_flisp.boot $(FLISP_EXECUTABLE)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE)) ./bin2hex.scm < $< > $@)

export julia_flisp_boot=$(BUILDDIR)/julia_flisp.boot
$(julia_flisp_boot): julia-parser.scm julia-syntax.scm \
	match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm $(FLISP_EXECUTABLE)
	@$(call PRINT_FLISP, $(call spawn,$(FLISP_EXECUTABLE)) ./mk_julia_flisp_boot.scm)

$(BUILDDIR)/ast.o $(BUILDDIR)/ast.dbg.obj: $(BUILDDIR)/julia_flisp.boot.inc flisp/*.h
$(BUILDDIR)/codegen.o $(BUILDDIR)/codegen.dbg.obj: intrinsics.cpp cgutils.cpp ccall.cpp abi_*.cpp
$(BUILDDIR)/builtins.o $(BUILDDIR)/builtins.dbg.obj: table.c
$(BUILDDIR)/gc.o $(BUILDDIR)/gc.dbg.obj: gc-debug.c
$(BUILDDIR)/signal-handling.o $(BUILDDIR)/signal-handling.dbg.obj: signals-*.c

$(BUILDDIR)/support/libsupport.a: support/*.h support/*.c
	$(MAKE) -C support BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(BUILDDIR)/support/libsupport-debug.a: support/*.h support/*.c
	$(MAKE) -C support debug BUILDDIR='$(abspath $(BUILDDIR)/support)'

$(FLISP_EXECUTABLE_release): $(BUILDDIR)/flisp/libflisp.a
$(BUILDDIR)/flisp/libflisp.a: flisp/*.h flisp/*.c $(BUILDDIR)/support/libsupport.a
	$(MAKE) -C flisp BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

$(FLISP_EXECUTABLE_debug): $(BUILDDIR)/flisp/libflisp-debug.a
$(BUILDDIR)/flisp/libflisp-debug.a: flisp/*.h flisp/*.c $(BUILDDIR)/support/libsupport-debug.a
	$(MAKE) -C flisp debug BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

julia_version.h: ../VERSION
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

$(build_shlibdir)/libjulia-debug.$(SHLIB_EXT): julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a $(LIBUV)
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(DEBUGFLAGS) $(DOBJS) $(RPATH_ORIGIN) -o $@ $(LDFLAGS) $(JLIBLDFLAGS) $(DEBUG_LIBS))
	$(INSTALL_NAME_CMD)libjulia-debug.$(SHLIB_EXT) $@
	$(DSYMUTIL) $@
$(BUILDDIR)/libjulia-debug.a: julia.expmap $(DOBJS) $(BUILDDIR)/flisp/libflisp-debug.a $(BUILDDIR)/support/libsupport-debug.a
	rm -f $@
	@$(call PRINT_LINK, ar -rcs $@ $(DOBJS))
libjulia-debug: $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT)

ifeq ($(SHLIB_EXT), so)
  SONAME = -Wl,-soname=libjulia.so
else
  SONAME =
endif

$(build_shlibdir)/libjulia.$(SHLIB_EXT): julia.expmap $(OBJS) $(BUILDDIR)/flisp/libflisp.a $(BUILDDIR)/support/libsupport.a $(LIBUV)
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
	-rm -f $(BUILDDIR)/*.dbg.obj $(BUILDDIR)/*.o *~ $(BUILDDIR)/*.$(SHLIB_EXT) $(BUILDDIR)/*.a *#
	-rm -f $(BUILDDIR)/julia_version.h

clean-flisp:
	-$(MAKE) -C flisp clean BUILDDIR='$(abspath $(BUILDDIR)/flisp)'

clean-support:
	-$(MAKE) -C support clean BUILDDIR='$(abspath $(BUILDDIR)/support)'


cleanall: clean clean-flisp clean-support

.PHONY: debug release clean cleanall clean-*
