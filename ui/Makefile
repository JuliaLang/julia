JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	    -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(EXTROOT)/include $(CFLAGS)

DEBUGFLAGS = $(FLAGS)
SHIPFLAGS = $(FLAGS)

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

$(JULIAHOME)/julia-release-basic: repl.o repl-basic.o $(JULIAHOME)/src/libjulia-release.a
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(SHIPFLAGS) $^ -o $@ $(JULIAHOME)/src/libjulia-release.a $(LIBS)
$(JULIAHOME)/julia-debug-basic: repl.do repl-basic.do $(JULIAHOME)/src/libjulia-debug.a
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(DEBUGFLAGS) $^ -o $@ $(JULIAHOME)/src/libjulia-debug.a $(LIBS)

$(JULIAHOME)/julia-release-readline: repl.o repl-readline.o $(JULIAHOME)/src/libjulia-release.a
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(SHIPFLAGS) $^ -o $@ $(JULIAHOME)/src/libjulia-release.a $(READLINE) $(LIBS)
$(JULIAHOME)/julia-debug-readline: repl.do repl-readline.do $(JULIAHOME)/src/libjulia-debug.a
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(DEBUGFLAGS) $^ -o $@ $(JULIAHOME)/src/libjulia-debug.a $(READLINE) $(LIBS)

clean:
	$(MAKE) -C webserver $@
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-basic
	rm -f $(JULIAHOME)/julia-*-readline
