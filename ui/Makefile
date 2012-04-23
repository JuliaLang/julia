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

$(JULIAHOME)/julia-release-basic: repl.o repl-basic.o
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(SHIPFLAGS) $^ -o $@ -L$(JULIALIB) -ljulia-release
$(JULIAHOME)/julia-debug-basic: repl.do repl-basic.do
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(JULIALIB) -ljulia-debug

$(JULIAHOME)/julia-release-readline: repl.o repl-readline.o
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(SHIPFLAGS) $^ -o $@ -L$(JULIALIB) -ljulia-release $(READLINE)
$(JULIAHOME)/julia-debug-readline: repl.do repl-readline.do
	$(QUIET_LINK) $(CXX) $(LDFLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(JULIALIB) -ljulia-debug $(READLINE)

clean:
	$(MAKE) -C webserver $@
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-basic
	rm -f $(JULIAHOME)/julia-*-readline
