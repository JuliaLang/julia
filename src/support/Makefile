JULIAHOME = $(shell pwd)/../..
include $(JULIAHOME)/Make.inc

SRCS = hashing.c timefuncs.c dblprint.c ptrhash.c operators.c socket.c \
	utf8.c ios.c cplxprint.c dirpath.c htable.c bitvector.c \
	int2str.c dump.c bswap.c lltinit.c arraylist.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
TARGET = libllt.a

FLAGS = -Wall -Wno-strict-aliasing $(CFLAGS) $(HFILEDIRS:%=-I%) -fvisibility=hidden
LIBS =

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

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
