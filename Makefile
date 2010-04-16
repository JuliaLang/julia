NAME = julia
SRCS = jltypes gf ast repl builtins jlfrontend jlfrontend_ module codegen
OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.do)
EXENAME = $(NAME)
LLTDIR = lib
LLT = $(LLTDIR)/libllt.a
LLT_H = $(LLTDIR)/llt.h

include ./Make.inc.$(shell uname)

FLAGS = -falign-functions -Wall -Wno-strict-aliasing -I$(LLTDIR) $(HFILEDIRS:%=-I%) $(LIBDIRS:%=-L%) $(CFLAGS) -D___LIBRARY $(CONFIG)
LIBFILES = $(LLT) $(GAMBITLIB)
LIBS = $(LIBFILES) -lutil -ldl -lm -lgc $(shell llvm-config --ldflags --libs core engine jit interpreter bitreader) -lreadline

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O2 -DNDEBUG $(FLAGS)

default: debug

%.o: %.c julia.h
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c julia.h
	$(CC) $(DEBUGFLAGS) -c $< -o $@
%.o: %.cpp julia.h
	$(CXX) $(SHIPFLAGS) $(shell llvm-config --cppflags) -c $< -o $@
%.do: %.cpp julia.h
	$(CXX) $(DEBUGFLAGS) $(shell llvm-config --cppflags) -c $< -o $@

ast.o ast.do: jlfrontend.h
codegen.o codegen.do: intrinsics.cpp julia-defs.s.bc.inc

jlfrontend.c: jlfrontend.scm \
	julia-parser.scm julia-syntax.scm match.scm utils.scm
	$(GAMBITGSC) -c $<
jlfrontend_.c: jlfrontend.c
	$(GAMBITGSC) -link -o $@ $<
jlfrontend.o jlfrontend_.o: %.o: %.c
	$(CC) $(SHIPFLAGS) -w -c $< -o $@
jlfrontend.do jlfrontend_.do: %.do: %.c
	$(CC) $(DEBUGFLAGS) -w -c $< -o $@

julia-defs.s.bc: julia-defs$(NBITS).s
	llvm-as -f $< -o $@

julia-defs.s.bc.inc: julia-defs.s.bc bin2hex.scm
	$(GAMBITGSI) ./bin2hex.scm < $< > $@

$(LLT): $(LLT_H)
	cd $(LLTDIR) && $(MAKE)

julia-debug: $(DOBJS) $(LIBFILES) julia-defs.s.bc
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -o $@ $(LIBS)
	ln -sf $@ julia

julia-efence: $(DOBJS) $(LIBFILES) julia-defs.s.bc
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -o $@ $(EFENCE) $(LIBS)
	ln -sf $@ julia

julia-release: $(OBJS) $(LIBFILES) julia-defs.s.bc
	$(CXX) $(SHIPFLAGS) $(OBJS) -o $@ $(LIBS)
	ln -sf $@ julia

debug release efence: %: julia-%

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.bc
	rm -f *.bc.inc
	rm -f jlfrontend.c
	rm -f jlfrontend_.c
	rm -f $(EXENAME)
	rm -f *~ *#
	cd $(LLTDIR) && $(MAKE) clean

cleanall: clean
	rm -rf $(EXENAME)-{debug,release,efence}

.PHONY: debug release efence clean
