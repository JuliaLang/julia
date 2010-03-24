CC = gcc
CXX = g++

NAME = julia
SRCS = jltypes gf ast repl builtins jlfrontend jlfrontend_ module codegen
OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.do)
EXENAME = $(NAME)
LLTDIR = lib
LLT = $(LLTDIR)/libllt.a

include ./Make.inc.$(shell uname)

FLAGS = -falign-functions -Wall -Wno-strict-aliasing -I$(LLTDIR) $(HFILEDIRS:%=-I%) $(LIBDIRS:%=-L%) $(CFLAGS) -D___LIBRARY $(CONFIG)
LIBFILES = $(LLT) $(GAMBITLIB)
LIBS = $(LIBFILES) -lutil -ldl -lm -lgc $(shell llvm-config --ldflags --libs core engine jit interpreter bitreader) -lreadline

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O2 -DNDEBUG $(FLAGS)

default: debug

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@
%.o: %.cpp
	$(CXX) $(SHIPFLAGS) $(shell llvm-config --cppflags) -c $< -o $@
%.do: %.cpp
	$(CXX) $(DEBUGFLAGS) $(shell llvm-config --cppflags) -c $< -o $@

jlfrontend.c: jlfrontend.scm julia-parser.scm julia-syntax.scm
	$(GAMBITGSC) -c $<

jlfrontend_.c: jlfrontend.c
	$(GAMBITGSC) -link -o $@ $<

julia-defs.s.bc: julia-defs.s
	llvm-as -f julia-defs.s

$(LLT):
	cd $(LLTDIR) && $(MAKE)

debug: $(DOBJS) $(LIBFILES) julia-defs.s.bc
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -o $(EXENAME) $(LIBS)

release: $(OBJS) $(LIBFILES) julia-defs.s.bc
	$(CXX) $(SHIPFLAGS) $(OBJS) -o $(EXENAME) $(LIBS)

clean:
	rm -f *.o
	rm -f *.do
	rm -f $(EXENAME)
	rm -f jlfrontend.c
	rm -f jlfrontend_.c
	rm -f *~ *#
	cd $(LLTDIR) && $(MAKE) clean
