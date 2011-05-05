JULIAHOME = $(shell pwd)/..
include $(JULIAHOME)/Make.inc

LIBS = -L$(EXTROOT)/lib -lutil -ldl -lm $(OSLIBS) -lpthread 

LLTDIR = $(JULIAHOME)/src/support
FLISPDIR = $(JULIAHOME)/src/flisp
MONGOOSE = $(JULIAHOME)/external/mongoose
ifeq ($(OS), Darwin)
READLINE = $(JULIAHOME)/lib/libreadline.$(SHLIB_EXT)
endif
ifeq ($(OS), Linux)
READLINE = -lncurses -lreadline
endif

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-I$(JULIAHOME)/src -I$(LLTDIR) -I$(FLISPDIR) \
	-I$(EXTROOT)/include $(CONFIG) $(CFLAGS)
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

LIBJULIA_RELEASE = $(JULIAHOME)/libjulia-release.$(SHLIB_EXT)
LIBJULIA_DEBUG = $(JULIAHOME)/libjulia-debug.$(SHLIB_EXT)

julia-debug julia-release: %: %-basic %-readline %-cloud

%.o: %.c repl.h
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release-basic: repl.o repl-basic.o
	$(CC) $(SHIPFLAGS) $^ -o $@ $(LIBJULIA_RELEASE) $(LIBS)
julia-debug-basic: repl.do repl-basic.do
	$(CC) $(DEBUGFLAGS) $^ -o $@ $(LIBJULIA_DEBUG) $(LIBS)

julia-release-readline: repl.o repl-readline.o
	$(CC) $(SHIPFLAGS) $^ -o $@ $(LIBJULIA_RELEASE) $(READLINE) $(LIBS)
julia-debug-readline: repl.do repl-readline.do
	$(CC) $(DEBUGFLAGS) $^ -o $@ $(LIBJULIA_DEBUG) $(READLINE) $(LIBS)

julia-release-cloud:
	$(CC) $(SHIPFLAGS) -I$(MONGOOSE) -DCLOUD_REPL repl.c repl-cloud.c $(MONGOOSE)/mongoose.c -o $@ $(LIBJULIA_RELEASE) -lssl -lcrypto $(LIBS)
julia-debug-cloud:
	$(CC) $(DEBUGFLAGS) -I$(MONGOOSE) -DCLOUD_REPL repl.c repl-cloud.c $(MONGOOSE)/mongoose.c -o $@ $(LIBJULIA_DEBUG) -lssl -lcrypto $(LIBS)

clean:
	rm -f *.o *.do julia-*-basic julia-*-readline julia-*-cloud
