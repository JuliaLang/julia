CC = gcc

SRCS = hashing.c timefuncs.c dblprint.c ptrhash.c \
	utf8.c ios.c cplxprint.c dirpath.c htable.c \
	int2str.c dump.c random.c bswap.c lltinit.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
TARGET = libllt.a

FLAGS = -Wall -Wno-strict-aliasing $(CFLAGS)
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
	rm -f *~
	rm -f core*
	rm -f $(TARGET)
