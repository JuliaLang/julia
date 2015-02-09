JULIAHOME = $(abspath ..)
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

SRCS = repl
ifeq ($(USEMSVC), 1)
SRCS += getopt
endif

FLAGS = -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir)
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fno-omit-frame-pointer
endif

OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.dbg.obj)
DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
ifeq ($(USE_LLVM_SHLIB),1)
LLVMLINK = $(call exec,$(LLVM_CONFIG) --ldflags) -lLLVM-$(call exec,$(LLVM_CONFIG) --version)
else
LLVMLINK =
endif
JLDFLAGS += $(LDFLAGS) $(NO_WHOLE_ARCHIVE) $(OSLIBS) $(LLVMLINK) $(RPATH)

ifeq ($(USE_SYSTEM_LIBM),0)
ifneq ($(UNTRUSTED_SYSTEM_LIBM),0)
JLDFLAGS += $(WHOLE_ARCHIVE) $(build_libdir)/libopenlibm.a $(NO_WHOLE_ARCHIVE)
endif
endif

ifeq ($(OS),WINNT)
JLDFLAGS += -municode
endif

release debug:
	$(MAKE) julia-$@

%.o: %.c
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@)
%.dbg.obj: %.c
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)

ifeq ($(OS),WINNT)
$(build_bindir)/julia.bat: $(JULIAHOME)/contrib/windows/*.bat
	cp $(JULIAHOME)/contrib/windows/*.bat $(build_bindir)
julia-release julia-debug: $(build_bindir)/julia.bat
ifneq ($(USEMSVC), 1)
julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc
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

ifneq ($(USEMSVC), 1)
CXXLD = $(CXX)
else
CXXLD = $(LD)
endif

$(build_bindir)/julia$(EXE): $(OBJS)
	@$(call PRINT_LINK, $(CXXLD) $(CXXLDFLAGS) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia $(JLDFLAGS) $(CXXLDFLAGS))
$(build_bindir)/julia-debug$(EXE): $(DOBJS)
	@$(call PRINT_LINK, $(CXXLD) $(CXXLDFLAGS) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia-debug $(JLDFLAGS) $(CXXLDFLAGS))

clean: | $(CLEAN_TARGETS)
	rm -f *.o *.dbg.obj
	rm -f $(build_bindir)/julia*

.PHONY: clean release debug julia-release julia-debug

