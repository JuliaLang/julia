NBITS = $(shell (test -e ../nbits || $(CC) ../nbits.c -o ../nbits) && ../nbits)
include ../Make.inc.$(shell uname)

SRCS = hashing.c timefuncs.c dblprint.c ptrhash.c operators.c socket.c \
	utf8.c ios.c cplxprint.c dirpath.c htable.c bitvector.c \
	int2str.c dump.c random.c bswap.c lltinit.c arraylist.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
TARGET = libllt.a

FLAGS = -Wall -Wno-strict-aliasing $(CFLAGS) $(CONFIG) -I $(HFILEDIRS:%=-I%)
LIBS =

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

default: release

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@

debug: $(DOBJS)
	rm -rf $(TARGET)
	ar rs $(TARGET) $(DOBJS)

release: $(OBJS)
	rm -rf $(TARGET)
	ar rs $(TARGET) $(OBJS)

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.a
	rm -f *~ *#
	rm -f core*
	rm -f $(TARGET)
