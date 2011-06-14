JULIAHOME = $(shell pwd)/..
include $(JULIAHOME)/Make.inc

LIBS = -L$(EXTROOT)/lib -lutil -ldl -lpthread $(OSLIBS)

LLTDIR = $(JULIAHOME)/src/support
FLISPDIR = $(JULIAHOME)/src/flisp
MONGOOSE = $(JULIAHOME)/external/mongoose
READLINE = -lncurses -lreadline

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-I$(JULIAHOME)/src -I$(LLTDIR) -I$(EXTROOT)/include $(CFLAGS)
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

LIBJULIA_RELEASE = $(JULIAHOME)/libjulia-release.$(SHLIB_EXT)
LIBJULIA_DEBUG = $(JULIAHOME)/libjulia-debug.$(SHLIB_EXT)

julia-debug julia-release: %: %-basic %-readline %-cloud

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
julia-release-cloud: $(JULIAHOME)/julia-release-cloud
julia-debug-cloud: $(JULIAHOME)/julia-debug-cloud

$(JULIAHOME)/julia-release-basic: repl.o repl-basic.o
	$(CC) $(SHIPFLAGS) $^ -o $@ $(LIBJULIA_RELEASE) $(LIBS)
$(JULIAHOME)/julia-debug-basic: repl.do repl-basic.do
	$(CC) $(DEBUGFLAGS) $^ -o $@ $(LIBJULIA_DEBUG) $(LIBS)

$(JULIAHOME)/julia-release-readline: repl.o repl-readline.o
	$(CC) $(SHIPFLAGS) $^ -o $@ $(LIBJULIA_RELEASE) $(READLINE) $(LIBS)
$(JULIAHOME)/julia-debug-readline: repl.do repl-readline.do
	$(CC) $(DEBUGFLAGS) $^ -o $@ $(LIBJULIA_DEBUG) $(READLINE) $(LIBS)

$(JULIAHOME)/julia-release-cloud: repl.c repl-cloud.c
	$(CC) $(SHIPFLAGS) -I$(MONGOOSE) -DCLOUD_REPL repl.c repl-cloud.c $(MONGOOSE)/mongoose.c -o $@ $(LIBJULIA_RELEASE) -lssl -lcrypto $(LIBS)
$(JULIAHOME)/julia-debug-cloud: repl.c repl-cloud.c
	$(CC) $(DEBUGFLAGS) -I$(MONGOOSE) -DCLOUD_REPL repl.c repl-cloud.c $(MONGOOSE)/mongoose.c -o $@ $(LIBJULIA_DEBUG) -lssl -lcrypto $(LIBS)

clean:
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-basic
	rm -f $(JULIAHOME)/julia-*-readline
	rm -f $(JULIAHOME)/julia-*-cloud
