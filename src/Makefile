JULIAHOME = $(shell pwd)/..
include ../Make.inc

SRCS = jltypes gf ast repl builtins module codegen interpreter alloc dlload \
	sys init task array dump $(GCSRCS)
OBJS = $(SRCS:%=%.o)
DOBJS = $(SRCS:%=%.do)
EXENAME = $(NAME)
LLTDIR = ../supportlib
FLISPDIR = ../flisp
LLT = $(LLTDIR)/libllt.a
FLISP = $(FLISPDIR)/libflisp.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing \
	-I$(FLISPDIR) -I$(LLTDIR) $(HFILEDIRS:%=-I%) $(LIBDIRS:%=-L%) \
	$(CONFIG) -I$(shell $(LLVMROOT)/bin/llvm-config --includedir) \
	-fvisibility=hidden
DEBUGFLAGS = -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

LIBFILES = $(FLISP) $(LLT)
LIBS = $(LIBFILES) -L$(EXTROOT)/lib -lutil -ldl -lm $(OSLIBS) \
	$(shell $(LLVMROOT)/bin/llvm-config --ldflags --libs engine) -lpthread 

ifeq ($(USE_READLINE), 1)
FLAGS += -DUSE_READLINE
SRCS += repl-readline
LIBS += -lreadline
else
SRCS += repl-basic
endif

%.o: %.c julia.h
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c julia.h
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@
%.o: %.cpp julia.h
	$(CXX) $(CXXFLAGS) $(SHIPFLAGS) $(shell $(LLVMROOT)/bin/llvm-config --cppflags) -c $< -o $@
%.do: %.cpp julia.h
	$(CXX) $(CXXFLAGS) $(DEBUGFLAGS) $(shell $(LLVMROOT)/bin/llvm-config --cppflags) -c $< -o $@

ast.o ast.do: julia_flisp.boot.inc boot.j.inc
julia_flisp.boot.inc: julia_flisp.boot $(FLISP)
	$(FLISPDIR)/flisp ./bin2hex.scm < $< > $@
julia_flisp.boot: julia-parser.scm julia-syntax.scm \
	match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm $(FLISP)
	$(FLISPDIR)/flisp ./mk_julia_flisp_boot.scm

boot.j.inc: boot.j preparse.scm julia_flisp.boot
	$(FLISPDIR)/flisp ./preparse.scm < $< | $(FLISPDIR)/flisp ./bin2hex.scm > $@

codegen.o codegen.do: intrinsics.cpp
builtins.o builtins.do: table.c

$(LLT): $(LLTDIR)/*.h $(LLTDIR)/*.c
	cd $(LLTDIR) && $(MAKE)

$(FLISP): $(FLISPDIR)/*.h $(FLISPDIR)/*.c $(LLT)
	cd $(FLISPDIR) && $(MAKE)

../julia-debug: $(DOBJS) $(LIBFILES)
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -o $@ $(LIBS)

../julia-release: $(OBJS) $(LIBFILES)
	$(CXX) $(SHIPFLAGS) $(OBJS) -o $@ $(LIBS)

julia-debug-link:
	ln -sf julia-debug ../julia

julia-release-link:
	ln -sf julia-release ../julia

debug release: %: ../julia-% julia-%-link

SLOCCOUNT = sloccount \
	--addlang makefile \
	--personcost 100000 \
	--effort 3.6 1.2 \
	--schedule 2.5 0.32

sloccount:
	for x in *.j; do cp $$x $${x%.j}.hs; done
	$(SLOCCOUNT) . | perl -ple 's/haskell/_julia_/g'
	rm *.hs

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.ji
	rm -f julia_flisp.boot
	rm -f julia_flisp.boot.inc
	rm -f boot.j.inc
	rm -f ../$(EXENAME)
	rm -f *~ *#

cleanother:
	rm -rf ../$(EXENAME)-{debug,release}
	$(MAKE) -C $(LLTDIR) clean
	$(MAKE) -C $(FLISPDIR) clean

cleanall: clean cleanother

.PHONY: debug release sloccount clean cleanall cleanother
