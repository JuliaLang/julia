JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(USR)/include $(CFLAGS) 

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

julia-release julia-debug: %: %-basic  %-readline

JLDFLAGS =  $(shell $(LLVM_CONFIG) --ldflags) $(OSLIBS) -lpthread $(LDFLAGS) $(RPATH) 

julia-release julia-debug: %: %-basic %-readline
	@$(MAKE) -C webserver $@

release debug:
	$(MAKE) julia-$@

%.o: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release-basic: $(JULIAHOME)/julia-release-basic
julia-debug-basic: $(JULIAHOME)/julia-debug-basic
julia-release-readline: $(JULIAHOME)/julia-release-readline
julia-debug-readline: $(JULIAHOME)/julia-debug-readline

$(JULIAHOME)/julia-release-basic: repl.o repl-basic.o
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $^ -o $@ -L$(USRLIB) -ljulia-release $(JLDFLAGS)
$(JULIAHOME)/julia-debug-basic: repl.do repl-basic.do
	$(QUIET_LINK) $(CXX) $(JLDFLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(USRLIB) -ljulia-debug

$(JULIAHOME)/julia-release-readline: repl.o repl-readline.o
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $^ -o $@ $(READLINE) -L$(USRLIB) -ljulia-release $(JLDFLAGS)
$(JULIAHOME)/julia-debug-readline: repl.do repl-readline.do
	$(QUIET_LINK) $(CXX) $(JLDFLAGS) $(DEBUGFLAGS) $^ -o $@ $(READLINE) -L$(USRLIB) -ljulia-debug

clean:
	$(MAKE) -C webserver $@
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-basic $(JULIAHOME)/julia-*-readline $(JULIAHOME)/julia
