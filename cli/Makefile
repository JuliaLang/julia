SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR ?= .
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make


HEADERS := $(addprefix $(JULIAHOME)/src/,julia.h julia_assert.h julia_threads.h julia_internal.h options.h) \
	$(BUILDDIR)/../src/julia_version.h $(wildcard $(JULIAHOME)/src/support/*.h) $(LIBUV_INC)/uv.h

LOADER_CFLAGS = $(JCFLAGS) -I$(BUILDROOT)/src -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir) -ffreestanding
LOADER_LDFLAGS = $(JLDFLAGS) -ffreestanding

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

SRCS := loader_exe loader_lib
OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)
LIB_OBJS := $(BUILDDIR)/loader_lib.o
LIB_DOBJS := $(BUILDDIR)/loader_lib.dbg.obj

default: release
all: release debug
release debug :  % : julia-%

$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(SHIPFLAGS) $(LOADER_CFLAGS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(DEBUGFLAGS) $(LOADER_CFLAGS) -c $< -o $@)

ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
$(BUILDDIR)/julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc $(JULIAHOME)/VERSION
	JLVER=`cat $(JULIAHOME)/VERSION` && \
	JLVERi=`echo $$JLVER | perl -nle \
		'/^(\d+)\.?(\d*)\.?(\d*)/ && \
		print int $$1,",",int $$2,",0,",int $$3'` && \
	$(CROSS_COMPILE)windres $< -O coff -o $@ -DJLVER=$$JLVERi -DJLVER_STR=\\\"$$JLVER\\\"
OBJS += julia_res.o
DOBJS += julia_res.o
endif
endif

julia-release: $(build_bindir)/julia$(EXE) $(build_shlibdir)/libjulialoader.$(JL_MAJOR_MINOR_SHLIB_EXT)
julia-debug: $(build_bindir)/julia-debug$(EXE) $(build_shlibdir)/libjulialoader-debug.$(JL_MAJOR_MINOR_SHLIB_EXT)

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

$(build_shlibdir)/libjulialoader.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_OBJS)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) -shared $(SHIPFLAGS) $^ -o $@ $(LOADER_LDFLAGS) $(RPATH_LIB))
ifneq ($(OS), WINNT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulialoader.$(JL_MAJOR_SHLIB_EXT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulialoader.$(SHLIB_EXT)
endif


$(build_shlibdir)/libjulialoader-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_DOBJS)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) -shared $(DEBUGFLAGS) $^ -o $@ $(LOADER_LDFLAGS) $(RPATH_LIB))
ifneq ($(OS), WINNT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulialoader-debug.$(JL_MAJOR_SHLIB_EXT)
	@ln -sf $(notdir $@) $(build_shlibdir)/libjulialoader-debug.$(SHLIB_EXT)
endif

$(build_bindir)/julia$(EXE): $(OBJS)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) $(SHIPFLAGS) $^ -o $@ $(LOADER_LDFLAGS) $(RPATH))

$(build_bindir)/julia-debug$(EXE): $(DOBJS)
	@$(call PRINT_LINK, $(CC) $(LOADER_CFLAGS) $(DEBUGFLAGS) $^ -o $@ $(LOADER_LDFLAGS) $(RPATH))


clean: | $(CLEAN_TARGETS)
	rm -f *.o *.dbg.obj
	rm -f $(build_bindir)/julia*

.PHONY: clean release debug julia-release julia-debug
