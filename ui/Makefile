JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(USR)/include $(CFLAGS) 

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS +=  $(shell $(LLVM_CONFIG) --ldflags) $(OSLIBS) -lpthread $(LDFLAGS) $(RPATH)

julia-release julia-debug: %: %-basic %-readline | $(USR)/lib/julia/website
	@$(MAKE) -C webserver $@

$(eval $(call symlink_target,website,$(USR)/lib/julia))
	
release debug: | $(USR)/lib/julia/website
	$(MAKE) julia-$@

%.o: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release-basic: $(USRBIN)/julia-release-basic
julia-debug-basic: $(USRBIN)/julia-debug-basic
julia-release-readline: $(USRBIN)/julia-release-readline
julia-debug-readline: $(USRBIN)/julia-debug-readline

$(USRBIN)/julia-release-basic: repl.o repl-basic.o
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $^ -o $@ -L$(USRLIB) $(JLDFLAGS) -ljulia-release
$(USRBIN)/julia-debug-basic: repl.do repl-basic.do
	$(QUIET_LINK) $(CXX) $(DEBUGFLAGS) $^ -o $@ -L$(USRLIB) $(JLDFLAGS) -ljulia-debug

$(USRBIN)/julia-release-readline: repl.o repl-readline.o
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $^ -o $@ $(READLINE) -L$(USRLIB) $(JLDFLAGS) -ljulia-release
$(USRBIN)/julia-debug-readline: repl.do repl-readline.do
	$(QUIET_LINK) $(CXX) $(DEBUGFLAGS) $^ -o $@ $(READLINE) -L$(USRLIB) $(JLDFLAGS) -ljulia-debug

clean: | $(CLEAN_TARGETS)
	$(MAKE) -C webserver $@
	rm -f *.o *.do
	rm -f $(USRBIN)/julia-*-basic $(USRBIN)/julia-*-readline $(USRBIN)/julia
