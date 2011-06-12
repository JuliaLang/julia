JULIAHOME = $(shell pwd)/../..
include $(JULIAHOME)/Make.inc

NAME = flisp
EXENAME = $(NAME)
LIBTARGET = lib$(NAME)

SRCS = flisp.c builtins.c string.c equalhash.c table.c iostream.c \
	julia_extensions.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
LLTDIR = ../support
LLT = $(LLTDIR)/libllt.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing -I$(LLTDIR) $(CFLAGS) \
	-DUSE_COMPUTED_GOTO $(HFILEDIRS:%=-I%) $(LIBDIRS:%=-L%) -fvisibility=hidden
LIBFILES = $(LLT)
LIBS = $(LIBFILES) -lm $(GCLIBS)

DEBUGFLAGS = -g -ggdb3 -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

default: release

release: $(EXENAME)

debug: $(EXENAME)-debug

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@

flisp.o:  flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flisp.do: flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flmain.o: flmain.c flisp.h
flmain.do: flmain.c flisp.h

$(LLT): $(LLTDIR)/*.h $(LLTDIR)/*.c
	cd $(LLTDIR) && make

$(LIBTARGET)-debug.a: $(DOBJS)
	rm -rf $@
	ar rs $@ $(DOBJS)
	ln -sf $@ $(LIBTARGET).a

$(LIBTARGET).a: $(OBJS)
	rm -rf $@
	ar rs $@ $(OBJS)

$(EXENAME)-debug: $(DOBJS) $(LIBFILES) $(LIBTARGET)-debug.a flmain.do
	$(CC) $(DEBUGFLAGS) $(DOBJS) flmain.do -o $(EXENAME)-debug $(LIBS) $(LIBTARGET).a $(OSLIBS)
	./$(EXENAME)-debug unittest.lsp

$(EXENAME): $(OBJS) $(LIBFILES) $(LIBTARGET).a flmain.o
	$(CC) $(SHIPFLAGS) $(OBJS) flmain.o -o $(EXENAME) $(LIBS) $(LIBTARGET).a $(OSLIBS)
	./$(EXENAME) unittest.lsp

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.a
	rm -f $(EXENAME)
	rm -f $(EXENAME)-debug
