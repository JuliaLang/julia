JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir)

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS += $(LDFLAGS) $(NO_WHOLE_ARCHIVE) $(call exec,$(LLVM_CONFIG) --ldflags) $(OSLIBS) $(RPATH)

ifeq ($(USE_SYSTEM_LIBM),0)
ifneq ($(UNTRUSTED_SYSTEM_LIBM),0)
JLDFLAGS += $(WHOLE_ARCHIVE) $(build_libdir)/libopenlibm.a $(NO_WHOLE_ARCHIVE)
endif
endif

julia-release: julia-readline julia-basic
julia-debug: julia-debug-readline julia-debug-basic

release debug:
	$(MAKE) julia-$@

%.o: %.c repl.h
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@)
%.do: %.c repl.h
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)

ifeq ($(OS),WINNT)
$(build_bindir)/julia.bat: $(JULIAHOME)/contrib/windows/*.bat
	cp $(JULIAHOME)/contrib/windows/*.bat $(build_bindir)
julia-release julia-debug: $(build_bindir)/julia.bat
julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc
	JLVER=`cat $(JULIAHOME)/VERSION` && \
	JLVERi=`echo $$JLVER | perl -nle \
		'/^(\d+)\.?(\d*)\.?(\d*)/ && \
		print int $$1,",",int $$2,",0,",int $$3'` && \
	$(CROSS_COMPILE)windres $< -O coff -o $@ -DJLVER=$$JLVERi -DJLVER_STR=\\\"$$JLVER\\\"
$(build_bindir)/julia-readline$(EXE): julia_res.o
$(build_bindir)/julia-debug-readline$(EXE): julia_res.o
$(build_bindir)/julia-basic$(EXE): julia_res.o
$(build_bindir)/julia-debug-basic$(EXE): julia_res.o
JLDFLAGS += julia_res.o
endif 

julia-basic: $(build_bindir)/julia-basic$(EXE)
julia-debug-basic: $(build_bindir)/julia-debug-basic$(EXE)
julia-readline: $(build_bindir)/julia-readline$(EXE)
julia-debug-readline: $(build_bindir)/julia-debug-readline$(EXE)

$(build_bindir)/julia-basic$(EXE): repl.o repl-basic.o
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia $(JLDFLAGS))
$(build_bindir)/julia-debug-basic$(EXE): repl.do repl-basic.do
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia-debug $(JLDFLAGS))

$(build_bindir)/julia-readline$(EXE): repl.o repl-readline.o
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ $(READLINE) -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia $(JLDFLAGS)) || (echo "*** Please ensure that the ncurses-devel package is installed on your OS, and try again. ***" && false)
$(build_bindir)/julia-debug-readline$(EXE): repl.do repl-readline.do
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ $(READLINE) -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -ljulia-debug $(JLDFLAGS))

clean: | $(CLEAN_TARGETS)
	rm -f *.o *.do
	rm -f $(build_bindir)/julia-*-basic $(build_bindir)/julia-*-readline $(build_bindir)/julia

.PHONY: clean release debug julia-release julia-debug

