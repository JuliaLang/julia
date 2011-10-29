JULIAHOME = $(shell pwd)/..
include $(JULIAHOME)/Make.inc

LLTDIR = $(JULIAHOME)/src/support
FLISPDIR = $(JULIAHOME)/src/flisp
READLINE = -lncurses $(EXTROOT)/lib/libreadline.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(LLTDIR) -I$(EXTROOT)/include $(CFLAGS)
DEBUGFLAGS += $(FLAGS) -static-libgcc -static-libstdc++
SHIPFLAGS += $(FLAGS) -static-libgcc -static-libstdc++

LIBJULIA_RELEASE = $(JULIAHOME)/src/libjulia-release.a
LIBJULIA_DEBUG = $(JULIAHOME)/src/libjulia-debug.a

julia-debug julia-release: %: %-basic %-readline

debug release:
	$(MAKE) julia-$@

%.o: %.c repl.h
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release-basic: $(JULIAHOME)/julia-release-basic
julia-debug-basic: $(JULIAHOME)/julia-debug-basic
julia-release-readline: $(JULIAHOME)/julia-release-readline
julia-debug-readline: $(JULIAHOME)/julia-debug-readline

$(JULIAHOME)/julia-release-basic: repl.o repl-basic.o
	$(CXX) $(SHIPFLAGS) $^ -o $@ $(LIBJULIA_RELEASE) $(LIBS)
$(JULIAHOME)/julia-debug-basic: repl.do repl-basic.do
	$(CXX) $(DEBUGFLAGS) $^ -o $@ $(LIBJULIA_DEBUG) $(LIBS)

$(JULIAHOME)/julia-release-readline: repl.o repl-readline.o
	$(CXX) $(SHIPFLAGS) $^ -o $@ $(LIBJULIA_RELEASE) $(READLINE) $(LIBS)
$(JULIAHOME)/julia-debug-readline: repl.do repl-readline.do
	$(CXX) $(DEBUGFLAGS) $^ -o $@ $(LIBJULIA_DEBUG) $(READLINE) $(LIBS)

clean:
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-basic
	rm -f $(JULIAHOME)/julia-*-readline
	rm -f $(JULIAHOME)/julia-*-web
