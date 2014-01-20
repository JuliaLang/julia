JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(BUILD)/include

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS += $(LDFLAGS) $(NO_WHOLE_ARCHIVE) $(call exec,$(LLVM_CONFIG) --ldflags) $(OSLIBS) $(RPATH) -Wl,--export-dynamic

ifeq ($(USE_SYSTEM_LIBM),0)
ifneq ($(UNTRUSTED_SYSTEM_LIBM),0)
ifeq ($(OS),WINNT)
JLDFLAGS += $(WHOLE_ARCHIVE) $(BUILD)/lib/libopenlibm.a $(NO_WHOLE_ARCHIVE)
else
JLDFLAGS += $(WHOLE_ARCHIVE) $(BUILD)/$(JL_LIBDIR)/libopenlibm.a $(NO_WHOLE_ARCHIVE)
endif
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
$(BUILD)/bin/julia.bat: ${JULIAHOME}/contrib/windows/*.bat
	cp ${JULIAHOME}/contrib/windows/*.bat ${BUILD}/bin
julia-release julia-debug: $(BUILD)/bin/julia.bat
julia_res.o: $(JULIAHOME)/contrib/windows/julia.rc
	JLVER=`cat ${JULIAHOME}/VERSION` && \
	JLVERi=`echo $$JLVER | perl -nle \
		'/^(\d+)\.?(\d*)\.?(\d*)/ && \
		print int $$1,",",int $$2,",0,",int $$3'` && \
	$(CROSS_COMPILE)windres $< -O coff -o $@ -DJLVER=$$JLVERi -DJLVER_STR=\\\"$$JLVER\\\"
$(BUILD)/bin/julia-readline$(EXE): julia_res.o
$(BUILD)/bin/julia-debug-readline$(EXE): julia_res.o
$(BUILD)/bin/julia-basic$(EXE): julia_res.o
$(BUILD)/bin/julia-debug-basic$(EXE): julia_res.o
JLDFLAGS += julia_res.o
endif 

julia-basic: $(BUILD)/bin/julia-basic$(EXE)
julia-debug-basic: $(BUILD)/bin/julia-debug-basic$(EXE)
julia-readline: $(BUILD)/bin/julia-readline$(EXE)
julia-debug-readline: $(BUILD)/bin/julia-debug-readline$(EXE)

$(BUILD)/bin/julia-basic$(EXE): repl.o repl-basic.o
	@$(call PRINT_LINK, $(CXX) $(CXXFLAGS) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -ljulia $(JLDFLAGS))
$(BUILD)/bin/julia-debug-basic$(EXE): repl.do repl-basic.do
	@$(call PRINT_LINK, $(CXX) $(CXXFLAGS) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -ljulia-debug $(JLDFLAGS))

$(BUILD)/bin/julia-readline$(EXE): repl.o repl-readline.o
	@$(call PRINT_LINK, $(CC) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ $(READLINE) -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -ljulia $(JLDFLAGS)) || (echo "*** Please ensure that the ncurses-devel package is installed on your OS, and try again. ***" && false)
$(BUILD)/bin/julia-debug-readline$(EXE): repl.do repl-readline.do
	@$(call PRINT_LINK, $(CC) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ $(READLINE) -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -ljulia-debug $(JLDFLAGS))

clean: | $(CLEAN_TARGETS)
	rm -f *.o *.do
	rm -f $(BUILD)/bin/julia-*-basic $(BUILD)/bin/julia-*-readline $(BUILD)/bin/julia

.PHONY: clean release debug julia-release julia-debug

