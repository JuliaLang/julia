JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

NAME = flisp
EXENAME = $(NAME)
LIBTARGET = lib$(NAME)

SRCS = flisp.c builtins.c string.c equalhash.c table.c iostream.c \
        julia_extensions.c
ifeq ($(USEMSVC), 1)
SRCS += basename.c dirname.c
endif

OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
LLTDIR = ../support
LLT = $(LLTDIR)/libsupport.a $(LIBUV) $(LIBUTF8PROC)

FLAGS = -I$(LLTDIR) $(CFLAGS) $(HFILEDIRS:%=-I%) \
        -I$(LIBUV_INC) -I$(JULIAHOME)/usr/include $(LIBDIRS:%=-L%) \
        -DLIBRARY_EXPORTS
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -DUSE_COMPUTED_GOTO -fvisibility=hidden 
endif
LIBFILES = $(LLT)
LIBS = $(LIBFILES)
ifneq ($(OS),WINNT)
LIBS += -lpthread
endif

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

default: release

release: $(EXENAME)

debug: $(EXENAME)-debug

HEADERS = $(wildcard *.h) $(LIBUV_INC)/uv.h

%.o: %.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(SHIPFLAGS) -DNDEBUG -c $< -o $@)
%.do: %.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)

flisp.o:   flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flisp.do:  flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flmain.o:  flmain.c flisp.h
flmain.do: flmain.c flisp.h

$(LLT): $(LLTDIR)/*.h $(LLTDIR)/*.c
	cd $(LLTDIR) && $(MAKE)

$(LIBTARGET)-debug.a: $(DOBJS)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $(DOBJS))
	ln -sf $@ $(LIBTARGET).a

$(LIBTARGET).a: $(OBJS)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $(OBJS))

ifneq ($(USEMSVC), 1)
CCLD = $(CC)
else
CCLD = $(LD)
endif

$(EXENAME)-debug: $(DOBJS) $(LIBFILES) $(LIBTARGET)-debug.a flmain.do
	@$(call PRINT_LINK, $(CCLD) $(DEBUGFLAGS) $(DOBJS) flmain.do -o $(EXENAME)-debug $(LIBTARGET).a $(LIBS) $(OSLIBS))
ifndef CROSS_COMPILE
ifneq ($(USEMSVC), 1)
	$(call spawn,./$(EXENAME)-debug) unittest.lsp
endif
endif

$(EXENAME): $(OBJS) $(LIBFILES) $(LIBTARGET).a flmain.o
	@$(call PRINT_LINK, $(CCLD) $(SHIPFLAGS) $(OBJS) flmain.o $(LDFLAGS) -o $(EXENAME) $(LIBTARGET).a $(LIBS) $(OSLIBS))
ifneq ($(USEMSVC), 1)
	$(call spawn,./$(EXENAME)) unittest.lsp
endif

clean:
	rm -f *.o
	rm -f *.do
	rm -f *.a
	rm -f $(EXENAME)
	rm -f $(EXENAME)-debug
