!INCLUDE <..\..\Windows.inc>

.SUFFIXES: .c

NAME = support

HEADERS = \
	arraylist.h \
	bitvector.h \
	dirpath.h \
	dtypes.h \
	hashing.h \
	htable.h \
	ios.h \
	libsupport.h \
	MurmurHash3.h \
	ptrhash.h \
	strtod.h \
	timefuncs.h \
	utf8.h \
	utils.h \
	platform.h

OBJECTS = \
	hashing.obj \
	timefuncs.obj \
	strtod.obj \
	ptrhash.obj \
	operators.obj \
	utf8.obj \
	ios.obj \
	htable.obj \
	bitvector.obj \
	int2str.obj \
	libsupportinit.obj \
	arraylist.obj \
	asprintf.obj \
	wcwidth.obj

INCLUDE = $(INCLUDE);$(MAKEDIR)\..\..\deps\libuv\include
CFLAGS = $(CFLAGS) -D_CRT_SECURE_NO_WARNINGS -DLIBRARY_EXPORTS

default: lib$(NAME).lib

lib$(NAME).lib: $(OBJECTS)
	$(AR) /OUT:$@ $(OBJECTS)

.c.obj:
	$(CC) $(CFLAGS) $<

# vim: noexpandtab:ts=4:sw=4:

