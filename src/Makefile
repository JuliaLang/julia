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

ifeq ($(REPL_READLINE), 1)
SRCS += repl-readline
LIBS += -lreadline
endif

ifeq ($(REPL_BASIC), 1)
SRCS += repl-basic
endif

ifeq ($(REPL_CLOUD), 1)
MONGOOSE = $(JULIAHOME)/ext/mongoose
FLAGS += -DCLOUD_REPL -I$(MONGOOSE)
SRCS += mongoose repl-cloud
LIBS += -lssl -lcrypto

mongoose.o:
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $(MONGOOSE)/mongoose.c -o $@

mongoose.do:
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $(MONGOOSE)/mongoose.c -o $@
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

julia-debug: $(DOBJS) $(LIBFILES)
	$(CXX) $(DEBUGFLAGS) $(DOBJS) -o $@ $(LIBS)

julia-release: $(OBJS) $(LIBFILES)
	$(CXX) $(SHIPFLAGS) $(OBJS) -o $@ $(LIBS)

debug release: %: julia-%

clean:
	rm -f julia-{debug,release}
	rm -f julia_flisp.boot
	rm -f julia_flisp.boot.inc
	rm -f boot.j.inc
	rm -f *.do
	rm -f *.o
	rm -f *.ji
	rm -f *~ *#

cleanother:
	$(MAKE) -C $(LLTDIR) clean
	$(MAKE) -C $(FLISPDIR) clean

cleanall: clean cleanother

.PHONY: debug release clean cleanall cleanother
