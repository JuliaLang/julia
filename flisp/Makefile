include ../Make.inc.$(shell uname)

CC = gcc

NAME = flisp
SRCS = $(NAME).c builtins.c string.c equalhash.c table.c iostream.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
EXENAME = $(NAME)
LIBTARGET = lib$(NAME)
LLTDIR = ../lib
LLT = $(LLTDIR)/libllt.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing -I$(LLTDIR) $(CFLAGS) $(CONFIG) -DUSE_COMPUTED_GOTO
LIBFILES = $(LLT)
LIBS = $(LIBFILES) -lm -lgc

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O2 -DNDEBUG $(FLAGS)

default: release test

test:
	./flisp unittest.lsp

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@

flisp.o:  flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flisp.do: flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flmain.o: flmain.c flisp.h
flmain.do: flmain.c flisp.h

$(LLT):
	cd $(LLTDIR) && make

$(LIBTARGET)-debug.a: $(DOBJS)
	rm -rf $@
	ar rs $@ $(DOBJS)
	ln -sf $@ $(LIBTARGET).a

$(LIBTARGET).a: $(OBJS)
	rm -rf $@
	ar rs $@ $(OBJS)

debug: $(DOBJS) $(LIBFILES) $(LIBTARGET)-debug.a flmain.do
	$(CC) $(DEBUGFLAGS) $(DOBJS) flmain.do -o $(EXENAME) $(LIBS) $(LIBTARGET).a
	make test

release: $(OBJS) $(LIBFILES) $(LIBTARGET).a flmain.o
	$(CC) $(SHIPFLAGS) $(OBJS) flmain.o -o $(EXENAME) $(LIBS) $(LIBTARGET).a

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.a
	rm -f $(EXENAME)
