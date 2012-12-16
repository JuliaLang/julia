JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(BUILD)/include $(CFLAGS) 

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS +=  $(shell $(LLVM_CONFIG) --ldflags) $(OSLIBS) -lpthread $(LDFLAGS) $(RPATH)

julia-release julia-debug: %: %-basic %-readline

release debug:
	$(MAKE) julia-$@

%.o: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release-basic: $(BUILD)/bin/julia-release-basic
julia-debug-basic: $(BUILD)/bin/julia-debug-basic
julia-release-readline: $(BUILD)/bin/julia-release-readline
julia-debug-readline: $(BUILD)/bin/julia-debug-readline

$(BUILD)/bin/julia-release-basic: repl.o repl-basic.o
	$(QUIET_LINK) $(CXX) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) $(JLDFLAGS) -ljulia-release
$(BUILD)/bin/julia-debug-basic: repl.do repl-basic.do
	$(QUIET_LINK) $(CXX) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) $(JLDFLAGS) -ljulia-debug

$(BUILD)/bin/julia-release-readline: repl.o repl-readline.o
	$(QUIET_LINK) $(CXX) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ $(READLINE) -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) $(JLDFLAGS) -ljulia-release
$(BUILD)/bin/julia-debug-readline: repl.do repl-readline.do
	$(QUIET_LINK) $(CXX) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ $(READLINE) -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) $(JLDFLAGS) -ljulia-debug

clean: | $(CLEAN_TARGETS)
	rm -f *.o *.do
	rm -f $(BUILD)/bin/julia-*-basic $(BUILD)/bin/julia-*-readline $(BUILD)/bin/julia
