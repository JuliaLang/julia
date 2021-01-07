SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR ?= .
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make


HEADERS := $(addprefix $(JULIAHOME)/cli/,jl_exports.h loader.h) $(addprefix $(JULIAHOME)/src/support/,platform.h dirpath.h) $(addprefix $(JULIAHOME)/src/,jl_exported_data.inc jl_exported_funcs.inc)

LOADER_CFLAGS = $(JCFLAGS) -I$(BUILDROOT)/src -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir) -ffreestanding
LOADER_LDFLAGS = $(JLDFLAGS) -ffreestanding -L$(build_shlibdir) -L$(build_libdir)

ifeq ($(OS),WINNT)
LOADER_CFLAGS += -municode -mconsole -nostdlib -fno-stack-check -fno-stack-protector -mno-stack-arg-probe
endif

ifeq ($(OS),WINNT)
LOADER_LDFLAGS += -municode -mconsole -nostdlib --disable-auto-import \
                  --disable-runtime-pseudo-reloc -lntdll -lkernel32 -lpsapi
else ifeq ($(OS),Linux)
LOADER_LDFLAGS += -Wl,--no-as-needed -ldl -lpthread -rdynamic -lc -Wl,--as-needed
else ifeq ($(OS),FreeBSD)
LOADER_LDFLAGS += -Wl,--no-as-needed -ldl -lpthread -rdynamic -lc -Wl,--as-needed
else ifeq ($(OS),Darwin)
LOADER_LDFLAGS += -lSystem
endif

# Build list of dependent libraries that must be opened
SHIPFLAGS  += -DDEP_LIBS="\"$(LOADER_BUILD_DEP_LIBS)\""
DEBUGFLAGS += -DDEP_LIBS="\"$(LOADER_DEBUG_BUILD_DEP_LIBS)\""

EXE_OBJS := $(BUILDDIR)/loader_exe.o
EXE_DOBJS := $(BUILDDIR)/loader_exe.dbg.obj
LIB_OBJS := $(BUILDDIR)/loader_lib.o
LIB_DOBJS := $(BUILDDIR)/loader_lib.dbg.obj

# If this is an architecture that supports dynamic linking, link in a trampoline definition
ifneq (,$(wildcard $(SRCDIR)/trampolines/trampolines_$(ARCH).S))
LIB_OBJS += $(BUILDDIR)/loader_trampolines.o
LIB_DOBJS += $(BUILDDIR)/loader_trampolines.o
endif

default: release
all: release debug
release debug :  % : julia-% libjulia-%

$(BUILDDIR)/loader_lib.o : $(SRCDIR)/loader_lib.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) -DLIBRARY_EXPORTS $(SHIPFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_lib.dbg.obj : $(SRCDIR)/loader_lib.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) -DLIBRARY_EXPORTS $(DEBUGFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_exe.o : $(SRCDIR)/loader_exe.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_exe.dbg.obj : $(SRCDIR)/loader_exe.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) $(DEBUGFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_trampolines.o : $(SRCDIR)/trampolines/trampolines_$(ARCH).S
	@$(call PRINT_CC, $(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) $< -c -o $@)

# Debugging target to help us see what kind of code is being generated for our trampolines
dump-trampolines: $(SRCDIR)/trampolines/trampolines_$(ARCH).S
	$(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) $< -S | sed -E 's/ ((%%)|;) /\n/g' | sed -E 's/.global/\n.global/g'

DIRS = $(build_bindir) $(build_libdir)
$(DIRS):
	@mkdir -p $@

ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
$(BUILDDIR)/julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc $(JULIAHOME)/VERSION
	JLVER=`cat $(JULIAHOME)/VERSION` && \
	JLVERi=`echo $$JLVER | perl -nle \
		'/^(\d+)\.?(\d*)\.?(\d*)/ && \
		print int $$1,",",int $$2,",0,",int $$3'` && \
	$(CROSS_COMPILE)windres $< -O coff -o $@ -DJLVER=$$JLVERi -DJLVER_STR=\\\"$$JLVER\\\"
EXE_OBJS += $(BUILDDIR)/julia_res.o
EXE_DOBJS += $(BUILDDIR)/julia_res.o
endif
endif

# Embed an Info.plist in the julia executable
# Create an intermediate target Info.plist for Darwin code signing.
ifeq ($(DARWIN_FRAMEWORK),1)
$(BUILDDIR)/Info.plist: $(JULIAHOME)/VERSION
	/usr/libexec/PlistBuddy -x -c "Clear dict" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleName string julia" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleIdentifier string $(darwin_codesign_id_julia_ui)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleInfoDictionaryVersion string 6.0" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleVersion string $(JULIA_COMMIT)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleShortVersionString string $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)" $@
.INTERMEDIATE: $(BUILDDIR)/Info.plist # cleanup this file after we are done using it
JLDFLAGS += -Wl,-sectcreate,__TEXT,__info_plist,Info.plist
$(build_bindir)/julia$(EXE): $(BUILDDIR)/Info.plist
$(build_bindir)/julia-debug$(EXE): $(BUILDDIR)/Info.plist
endif

julia-release: $(build_bindir)/julia$(EXE)
julia-debug: $(build_bindir)/julia-debug$(EXE)
libjulia-release: $(build_shlibdir)/libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT)
libjulia-debug: $(build_shlibdir)/libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT)

ifeq ($(OS),WINNT)
# On Windows we need to strip out exported functions from the generated import library.
# On i686, there's an extra underscore at the beginning
ifeq ($(ARCH),i686)
ABI_UNDERSCORE := _\#\#
else
ABI_UNDERSCORE :=
endif
EXPORTED_FUNCS := $(shell echo -e "#include \"jl_exported_funcs.inc\"\n#define XX(x) $(ABI_UNDERSCORE)x\nJL_EXPORTED_FUNCS(XX)" | $(CPP) -I$(JULIAHOME)/src - | tail -n 1)
STRIP_EXPORTED_FUNCS := $(patsubst %,--strip-symbol=%,$(EXPORTED_FUNCS))

# Note that if the objcopy command starts getting too long, we can use `@file` to read
# command-line options from `file` instead.
define strip_implib
	$(OBJCOPY) $(1).a $(STRIP_EXPORTED_FUNCS) $(1).a.stripped
	mv $(1).a.stripped $(1).a
endef
endif

$(build_shlibdir)/libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_OBJS) | $(build_shlibdir) $(build_libdir)
	@$(call PRINT_LINK, $(CC) $(call IMPLIB_FLAGS,$@) $(LOADER_CFLAGS) -DLIBRARY_EXPORTS -shared $(SHIPFLAGS) $(LIB_OBJS) -o $@ $(JLIBLDFLAGS) $(LOADER_LDFLAGS) $(RPATH_LIB)) $(call SONAME_FLAGS,libjulia.$(JL_MAJOR_SHLIB_EXT))
	$(INSTALL_NAME_CMD)libjulia.$(SHLIB_EXT) $@
ifneq ($(OS), WINNT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulia.$(JL_MAJOR_SHLIB_EXT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulia.$(SHLIB_EXT)
else
	$(call strip_implib,$(build_libdir)/libjulia.$(SHLIB_EXT))
endif

$(build_shlibdir)/libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_DOBJS) | $(build_shlibdir) $(build_libdir)
	@$(call PRINT_LINK, $(CC) $(call IMPLIB_FLAGS,$@) $(LOADER_CFLAGS) -DLIBRARY_EXPORTS -shared $(DEBUGFLAGS) $(LIB_DOBJS) -o $@ $(JLIBLDFLAGS) $(LOADER_LDFLAGS) $(RPATH_LIB)) $(call SONAME_FLAGS,libjulia-debug.$(JL_MAJOR_SHLIB_EXT))
	$(INSTALL_NAME_CMD)libjulia-debug.$(SHLIB_EXT) $@
ifneq ($(OS), WINNT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulia-debug.$(JL_MAJOR_SHLIB_EXT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT)
else
	$(call strip_implib,$(build_libdir)/libjulia-debug.$(SHLIB_EXT))
endif

$(build_bindir)/julia$(EXE): $(EXE_OBJS) $(build_shlibdir)/libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT) | $(build_bindir)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) $(SHIPFLAGS) $(EXE_OBJS) -o $@ $(LOADER_LDFLAGS) $(RPATH) -ljulia)

$(build_bindir)/julia-debug$(EXE): $(EXE_DOBJS) $(build_shlibdir)/libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT) | $(build_bindir)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) $(DEBUGFLAGS) $(EXE_DOBJS) -o $@ $(LOADER_LDFLAGS) $(RPATH) -ljulia-debug)

clean: | $(CLEAN_TARGETS)
	rm -f $(BUILDDIR)/*.o $(BUILDDIR)/*.dbg.obj
	rm -f $(build_bindir)/julia*

.PHONY: clean release debug julia-release julia-debug
