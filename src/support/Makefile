JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

OBJS = hashing.o timefuncs.o ptrhash.o operators.o \
	utf8.o ios.o htable.o bitvector.o \
	int2str.o libsupportinit.o arraylist.o strtod.o

ifeq ($(OS),WINNT)
OBJS += asprintf.o wcwidth.o
ifeq ($(ARCH),i686)
OBJS += _setjmp.win32.o _longjmp.win32.o
else ifeq ($(ARCH),i386)
OBJS += _setjmp.win32.o _longjmp.win32.o
else ifeq ($(ARCH),x86_64)
OBJS += _setjmp.win64.o _longjmp.win64.o
endif
endif

DOBJS = $(OBJS:%.o=%.do)

FLAGS = $(CFLAGS) $(HFILEDIRS:%=-I%) -I$(LIBUV_INC) -DLIBRARY_EXPORTS
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fvisibility=hidden
endif

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

default: release

HEADERS = $(wildcard *.h) $(LIBUV_INC)/uv.h

%.o: %.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(SHIPFLAGS) -DNDEBUG -c $< -o $@)
%.do: %.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)
ifneq ($(USEMSVC), 1)
%.o: %.S
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(SHIPFLAGS) -c $< -o $@)
%.do: %.S
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)
else
%.o: %.S
	@$(call PRINT_CC, $(CPP) -P $(CPPFLAGS) $(SHIPFLAGS) $<)
	@$(call PRINT_CC, $(AS) $(CPPFLAGS) $(SHIPFLAGS) -Fo$@ -c $*.i)
%.do: %.S
	@$(call PRINT_CC, $(CPP) -P $(CPPFLAGS) $(DEBUGFLAGS) $<)
	@$(call PRINT_CC, $(AS) $(CPPFLAGS) $(DEBUGFLAGS) -Fo$@ -c $*.i)
endif

release: libsupport.a
debug: libsupport-debug.a

libsupport.a: $(OBJS)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $^)

libsupport-debug.a: $(DOBJS)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $^)

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.a
	rm -f *~ *#
	rm -f core*
	rm -f libsupport.a
	rm -f libsupport-debug.a
