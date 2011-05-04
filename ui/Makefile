JULIAHOME = $(shell pwd)/..
include $(JULIAHOME)/Make.inc

LLTDIR = $(JULIAHOME)/src/support
FLISPDIR = $(JULIAHOME)/src/flisp
MONGOOSE = $(JULIAHOME)/external/mongoose
READLINE = $(JULIAHOME)/lib/libreadline.$(SHLIB_EXT)

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-I$(JULIAHOME)/src -I$(LLTDIR) -I$(FLISPDIR) \
	-I$(EXTROOT)/include $(CONFIG) $(CFLAGS)
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

LIBJULIA_RELEASE = $(JULIAHOME)/libjulia-release.dylib
LIBJULIA_DEBUG = $(JULIAHOME)/libjulia-debug.dylib

all: julia-basic julia-readline julia-cloud

%.o: %.c repl.h
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-basic: repl.o repl-basic.o
	$(CC) $(SHIPFLAGS) repl.o repl-basic.o -o $@ $(LIBJULIA_RELEASE)

julia-readline: repl.o repl-readline.o
	$(CC) $(SHIPFLAGS) repl.o repl-readline.o -o $@ $(LIBJULIA_RELEASE) $(READLINE)

julia-cloud:
	$(CC) $(SHIPFLAGS) -I$(MONGOOSE) -DCLOUD_REPL repl.c repl-cloud.c $(MONGOOSE)/mongoose.c -o $@ $(LIBJULIA_RELEASE) -lssl -lcrypto

clean:
	rm -f *.o *.do julia-basic julia-readline julia-cloud
