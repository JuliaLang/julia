SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR ?= .
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make


HEADERS := $(addprefix $(SRCDIR)/,jl_exports.h loader.h) $(addprefix $(JULIAHOME)/src/,julia_fasttls.h support/platform.h support/dirpath.h jl_exported_data.inc jl_exported_funcs.inc)

LOADER_CFLAGS = $(JCFLAGS) -I$(BUILDROOT)/src -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir) -ffreestanding
LOADER_LDFLAGS = $(JLDFLAGS) -ffreestanding -L$(build_shlibdir) -L$(build_libdir)

ifeq ($(OS),WINNT)
LOADER_CFLAGS += -municode -mconsole -nostdlib -fno-stack-check -fno-stack-protector -mno-stack-arg-probe
else ifeq ($(OS),Linux)
LOADER_CFLAGS += -DGLIBCXX_LEAST_VERSION_SYMBOL=\"$(shell echo "$(CSL_NEXT_GLIBCXX_VERSION)" | cut -d'|' -f1 | sed 's/\\//g')\"
endif

ifeq ($(OS),WINNT)
LOADER_LDFLAGS += -municode -mconsole -nostdlib -lntdll -lkernel32 -lpsapi
else ifeq ($(OS),Linux)
# textoff and notext are aliases to the same option which suppress the TEXTREL warning for i686
LOADER_LDFLAGS += -Wl,--no-as-needed -ldl -lpthread -rdynamic -lc -Wl,--as-needed -Wl,-z,notext
else ifeq ($(OS),FreeBSD)
LOADER_LDFLAGS += -Wl,--no-as-needed -ldl -lpthread -rdynamic -lc -Wl,--as-needed
else ifeq ($(OS),OpenBSD)
LOADER_LDFLAGS += -Wl,--no-as-needed -lpthread -rdynamic -lc -Wl,--as-needed
endif

# Build list of dependent libraries that must be opened
SHIPFLAGS  += -DDEP_LIBS=$(call shell_escape,$(call c_escape,$(LOADER_BUILD_DEP_LIBS)))
DEBUGFLAGS += -DDEP_LIBS=$(call shell_escape,$(call c_escape,$(LOADER_DEBUG_BUILD_DEP_LIBS)))
ifneq (,$(findstring MINGW,$(shell uname)))
# In MSYS2, do not perform path conversion for `DEP_LIBS`.
# https://www.msys2.org/wiki/Porting/#filesystem-namespaces
# We define this environment variable for only these two object files,
# as they're the only ones that require it at the time of writing.
$(BUILDDIR)/loader_lib.o: export MSYS2_ARG_CONV_EXCL = -DDEP_LIBS=
$(BUILDDIR)/loader_lib.dbg.obj: export MSYS2_ARG_CONV_EXCL = -DDEP_LIBS=
endif # MSYS2

EXE_OBJS := $(BUILDDIR)/loader_exe.o
EXE_DOBJS := $(BUILDDIR)/loader_exe.dbg.obj
LIB_OBJS := $(BUILDDIR)/loader_lib.o
LIB_DOBJS := $(BUILDDIR)/loader_lib.dbg.obj

# If this is an architecture that supports dynamic linking, link in a trampoline definition
ifneq (,$(wildcard $(SRCDIR)/trampolines/trampolines_$(ARCH).S))
LIB_OBJS += $(BUILDDIR)/loader_trampolines.o
LIB_DOBJS += $(BUILDDIR)/loader_trampolines.dbg.obj
endif

default: release
all: release debug
release debug :  % : julia-% libjulia-%

$(BUILDDIR)/loader_lib.o : $(SRCDIR)/loader_lib.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) -DJL_LIBRARY_EXPORTS $(SHIPFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_lib.dbg.obj : $(SRCDIR)/loader_lib.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) -DJL_LIBRARY_EXPORTS $(DEBUGFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_exe.o : $(SRCDIR)/loader_exe.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_exe.dbg.obj : $(SRCDIR)/loader_exe.c $(HEADERS) $(JULIAHOME)/VERSION
	@$(call PRINT_CC, $(CC) $(DEBUGFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/loader_trampolines.o : $(SRCDIR)/trampolines/trampolines_$(ARCH).S $(HEADERS) $(SRCDIR)/trampolines/common.h
	@$(call PRINT_CC, $(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) $< -c -o $@)
$(BUILDDIR)/loader_trampolines.dbg.obj : $(SRCDIR)/trampolines/trampolines_$(ARCH).S $(HEADERS) $(SRCDIR)/trampolines/common.h
	@$(call PRINT_CC, $(CC) $(DEBUGFLAGS) $(LOADER_CFLAGS) $< -c -o $@)

# Debugging target to help us see what kind of code is being generated for our trampolines
dump-trampolines: $(SRCDIR)/trampolines/trampolines_$(ARCH).S
	$(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) $< -S | sed -E 's/ ((%%)|;) /\n/g' | sed -E 's/.global/\n.global/g'

DIRS = $(build_bindir) $(build_libdir)
$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

ifeq ($(OS),WINNT)
$(BUILDDIR)/julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc $(JULIAHOME)/VERSION
	JLVER=`cat $(JULIAHOME)/VERSION` && \
	JLVERi=`echo $$JLVER | perl -nle \
		'/^(\d+)\.?(\d*)\.?(\d*)/ && \
		print int $$1,",",int $$2,",",int $$3,",0"'` && \
	$(CROSS_COMPILE)windres $< -O coff -o $@ -DJLVER=$$JLVERi -DJLVER_STR=\\\"$$JLVER\\\"
EXE_OBJS += $(BUILDDIR)/julia_res.o
EXE_DOBJS += $(BUILDDIR)/julia_res.o
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
libjulia-release: $(build_shlibdir)/libjulia.$(SHLIB_EXT)
libjulia-debug: $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT)

ifneq (,$(filter $(OS), Linux FreeBSD OpenBSD))
VERSIONSCRIPT := -Wl,--version-script=$(BUILDDIR)/julia.expmap
endif

ifeq ($(OS),WINNT)
# On Windows we need to strip out exported functions from the generated import library.
STRIP_EXPORTED_FUNCS := $(shell $(CPP_STDOUT) -I$(JULIAHOME)/src $(SRCDIR)/list_strip_symbols.h)
endif

$(build_shlibdir)/libjulia.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_OBJS) $(SRCDIR)/list_strip_symbols.h $(BUILDDIR)/julia.expmap | $(build_shlibdir) $(build_libdir)
	@$(call PRINT_LINK, $(CC) $(call IMPLIB_FLAGS,$@.tmp) $(LOADER_CFLAGS) -shared $(SHIPFLAGS) $(LIB_OBJS) $(RPATH_LIB) -o $@ \
		$(JLIBLDFLAGS) $(LOADER_LDFLAGS) $(VERSIONSCRIPT) $(call SONAME_FLAGS,libjulia.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libjulia.$(JL_MAJOR_SHLIB_EXT) $@
	@$(DSYMUTIL) $@
ifeq ($(OS), WINNT)
	@# Note that if the objcopy command starts getting too long, we can use `@file` to read
	@# command-line options from `file` instead.
	@$(call PRINT_ANALYZE, $(OBJCOPY) $(build_libdir)/$(notdir $@).tmp.a $(STRIP_EXPORTED_FUNCS) $(build_libdir)/$(notdir $@).a && rm $(build_libdir)/$(notdir $@).tmp.a)
endif

$(build_shlibdir)/libjulia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_DOBJS) $(SRCDIR)/list_strip_symbols.h $(BUILDDIR)/julia.expmap | $(build_shlibdir) $(build_libdir)
	@$(call PRINT_LINK, $(CC) $(call IMPLIB_FLAGS,$@.tmp) $(LOADER_CFLAGS) -shared $(DEBUGFLAGS) $(LIB_DOBJS) $(RPATH_LIB) -o $@ \
		$(JLIBLDFLAGS) $(LOADER_LDFLAGS) $(VERSIONSCRIPT) $(call SONAME_FLAGS,libjulia-debug.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libjulia-debug.$(JL_MAJOR_SHLIB_EXT) $@
	@$(DSYMUTIL) $@
ifeq ($(OS), WINNT)
	@$(call PRINT_ANALYZE, $(OBJCOPY) $(build_libdir)/$(notdir $@).tmp.a $(STRIP_EXPORTED_FUNCS) $(build_libdir)/$(notdir $@).a && rm $(build_libdir)/$(notdir $@).tmp.a)
endif

ifneq ($(OS), WINNT)
$(build_shlibdir)/libjulia.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libjulia-debug.$(JL_MAJOR_SHLIB_EXT): $(build_shlibdir)/libjulia%.$(JL_MAJOR_SHLIB_EXT): \
		$(build_shlibdir)/libjulia%.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
$(build_shlibdir)/libjulia.$(SHLIB_EXT) $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT): $(build_shlibdir)/libjulia%.$(SHLIB_EXT): \
		$(build_shlibdir)/libjulia%.$(JL_MAJOR_MINOR_SHLIB_EXT) $(build_shlibdir)/libjulia%.$(JL_MAJOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
endif

$(build_bindir)/julia$(EXE): $(EXE_OBJS) $(build_shlibdir)/libjulia.$(SHLIB_EXT) | $(build_bindir)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) $(SHIPFLAGS) $(EXE_OBJS) -o $@ $(LOADER_LDFLAGS) $(RPATH) -ljulia)

$(build_bindir)/julia-debug$(EXE): $(EXE_DOBJS) $(build_shlibdir)/libjulia-debug.$(SHLIB_EXT) | $(build_bindir)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) $(DEBUGFLAGS) $(EXE_DOBJS) -o $@ $(LOADER_LDFLAGS) $(RPATH) -ljulia-debug)

$(BUILDDIR)/julia.expmap: $(SRCDIR)/julia.expmap.in $(JULIAHOME)/VERSION
	sed <'$<' >'$@' -e 's/@JULIA_SHLIB_SYMBOL_VERSION@/JL_LIBJULIA_$(SOMAJOR)/'

clean: | $(CLEAN_TARGETS)
	rm -f $(BUILDDIR)/*.o $(BUILDDIR)/*.dbg.obj
	rm -f $(build_bindir)/julia*
	rm -f $(BUILDDIR)/julia.expmap

.PHONY: clean release debug julia-release julia-debug
