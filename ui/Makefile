SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR ?= .
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

SRCS := repl

HEADERS := $(addprefix $(JULIAHOME)/src/,julia.h julia_assert.h julia_threads.h julia_internal.h options.h) \
	$(BUILDDIR)/../src/julia_version.h $(wildcard $(JULIAHOME)/src/support/*.h) $(LIBUV_INC)/uv.h

FLAGS := -I$(BUILDROOT)/src -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir)
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer -Wc++-compat
endif

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS += $(LDFLAGS) $(NO_WHOLE_ARCHIVE) $(OSLIBS) $(RPATH)

ifeq ($(USE_SYSTEM_LIBM),0)
ifneq ($(UNTRUSTED_SYSTEM_LIBM),0)
JLDFLAGS += $(WHOLE_ARCHIVE) $(build_libdir)/libopenlibm.a $(NO_WHOLE_ARCHIVE)
endif
endif

ifeq ($(OS),WINNT)
JLDFLAGS += -municode
endif

ifeq ($(DARWIN_FRAMEWORK),1)
# Embed an Info.plist in the julia executable
JLDFLAGS += -Wl,-sectcreate,__TEXT,__info_plist,Info.plist
endif

default: release
all: release debug
release debug :  % : julia-%

$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)

ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
$(BUILDDIR)/julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc
	JLVER=`cat $(JULIAHOME)/VERSION` && \
	JLVERi=`echo $$JLVER | perl -nle \
		'/^(\d+)\.?(\d*)\.?(\d*)/ && \
		print int $$1,",",int $$2,",0,",int $$3'` && \
	$(CROSS_COMPILE)windres $< -O coff -o $@ -DJLVER=$$JLVERi -DJLVER_STR=\\\"$$JLVER\\\"
$(build_bindir)/julia$(EXE): julia_res.o
$(build_bindir)/julia-debug$(EXE): julia_res.o
endif
endif

julia-release: $(build_bindir)/julia$(EXE)
julia-debug: $(build_bindir)/julia-debug$(EXE)

# Create an intermediate target Info.plist for Darwin code signing.
$(BUILDDIR)/Info.plist:
ifeq ($(OS),Darwin)
	/usr/libexec/PlistBuddy -x -c "Clear dict" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleName string julia" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleIdentifier string $(darwin_codesign_id_julia_ui)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleInfoDictionaryVersion string 6.0" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleVersion string $(JULIA_COMMIT)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleShortVersionString string $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)" $@
else
	touch $@
endif
.INTERMEDIATE: $(BUILDDIR)/Info.plist

ifneq ($(USEMSVC), 1)
CXXLD := $(CXX)
else
CXXLD := $(LD)
endif

$(build_bindir)/julia$(EXE): $(OBJS) $(BUILDDIR)/Info.plist
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(LINK_FLAGS) $(SHIPFLAGS) $(OBJS) -o $@ -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia $(JLDFLAGS) $(CXXLDFLAGS))
$(build_bindir)/julia-debug$(EXE): $(DOBJS) $(BUILDDIR)/Info.plist
	@$(call PRINT_LINK, $(CXXLD) $(CXXFLAGS) $(CXXLDFLAGS) $(LINK_FLAGS) $(DEBUGFLAGS) $(DOBJS) -o $@ -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia-debug $(JLDFLAGS) $(CXXLDFLAGS))

clean: | $(CLEAN_TARGETS)
	rm -f *.o *.dbg.obj
	rm -f $(build_bindir)/julia*

.PHONY: clean release debug julia-release julia-debug
