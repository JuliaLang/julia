!INCLUDE <..\..\Windows.inc>

.SUFFIXES: .c

NAME = flisp

SOURCES = \
	builtins.c \
	string.c \
	equalhash.c \
	table.c \
	iostream.c \
	julia_extensions.c \
	dirname.c \
	basename.c

HEADERS = \
	flisp.h \
	opcodes.h \
	libgen.h

OBJECTS = \
	flisp.obj \
	builtins.obj \
	string.obj \
	equalhash.obj \
	table.obj \
	iostream.obj \
	julia_extensions.obj \
	dirname.obj \
	basename.obj

LIBUV = $(MAKEDIR)\..\..\deps\libuv\libuv.lib
LIBMOJIBAKE = $(MAKEDIR)\..\..\deps\libmojibake\libmojibake.lib
LIBSUPPORT = $(MAKEDIR)\..\support\libsupport.lib

INCLUDE = $(INCLUDE);$(MAKEDIR)\..\..\deps\libuv\include;$(MAKEDIR)\..\..\deps\libmojibake;$(MAKEDIR)\..\support

CFLAGS = $(CFLAGS) /Qstd=c99 -D_CRT_SECURE_NO_WARNINGS -DLIBRARY_EXPORTS
LFLAGS = $(LFLAGS) kernel32.lib ws2_32.lib psapi.lib advapi32.lib iphlpapi.lib

default: $(NAME).exe

$(NAME).exe: lib$(NAME).lib flmain.obj $(LIBSUPPORT) $(LIBUV) $(LIBMOJIBAKE)
	$(LINK) $(LFLAGS) /OUT:$(NAME).exe /PDB:$(NAME).pdb /MAP $**

$(LIBSUPPORT):
	PUSHD $(MAKEDIR)\..\support && $(MAKE) /NOLOGO /F Windows.mk && POPD

$(LIBUV):
	PUSHD $(MAKEDIR)\..\..\deps\libuv && $(MAKE) /NOLOGO /F Windows.mk  && POPD

$(LIBMOJIBAKE):
	PUSHD $(MAKEDIR)\..\..\deps\libmojibake && cl -nologo /c utf8proc.c && $(AR) /OUT:libmojibake.lib utf8proc.obj && POPD

lib$(NAME).lib: $(OBJECTS)
	$(AR) /OUT:lib$(NAME).lib $**

flisp.obj: flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
	$(CC) $(CFLAGS) flisp.c

flmain.obj: flmain.c flisp.h
	$(CC) $(CFLAGS) flmain.c

.c.obj:
	$(CC) $(CFLAGS) $<

# vim: noexpandtab:ts=4:sw=4:

