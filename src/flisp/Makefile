JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

BUILDDIR ?= .

NAME = flisp
EXENAME = $(NAME)
LIBTARGET = lib$(NAME)

SRCS = flisp.c builtins.c string.c equalhash.c table.c iostream.c \
        julia_extensions.c
ifeq ($(USEMSVC), 1)
SRCS += basename.c dirname.c
endif

HEADERS = $(wildcard *.h) $(LIBUV_INC)/uv.h

OBJS = $(SRCS:%.c=$(BUILDDIR)/%.o)
DOBJS = $(SRCS:%.c=$(BUILDDIR)/%.dbg.obj)
LLTDIR = ../support
LLT = $(BUILDDIR)/$(LLTDIR)/libsupport.a $(LIBUV) $(LIBMOJIBAKE)

FLAGS = -I$(LLTDIR) $(CFLAGS) $(HFILEDIRS:%=-I%) \
        -I$(LIBUV_INC) -I$(build_includedir) $(LIBDIRS:%=-L%) \
        -DLIBRARY_EXPORTS -DMOJIBAKE_EXPORTS
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

release: $(BUILDDIR)/$(EXENAME)

debug: $(BUILDDIR)/$(EXENAME)-debug

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BUILDDIR)/%.o: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(SHIPFLAGS) -DNDEBUG -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)

$(BUILDDIR)/flisp.o:   flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
$(BUILDDIR)/flisp.dbg.obj:  flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
$(BUILDDIR)/flmain.o:  flmain.c flisp.h
$(BUILDDIR)/flmain.dbg.obj: flmain.c flisp.h

$(LLT): $(LLTDIR)/*.h $(LLTDIR)/*.c
	$(MAKE) -C $(LLTDIR) BUILDDIR='$(abspath $(BUILDDIR)/$(LLTDIR))'

$(BUILDDIR)/$(LIBTARGET)-debug.a: $(DOBJS) | $(BUILDDIR)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $(DOBJS))
	ln -sf $@ $(LIBTARGET).a

$(BUILDDIR)/$(LIBTARGET).a: $(OBJS) | $(BUILDDIR)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $(OBJS))

ifneq ($(USEMSVC), 1)
CCLD = $(CC)
else
CCLD = $(LD)
endif

$(BUILDDIR)/$(EXENAME)-debug: $(DOBJS) $(LIBFILES) $(BUILDDIR)/$(LIBTARGET)-debug.a $(BUILDDIR)/flmain.dbg.obj
	@$(call PRINT_LINK, $(CCLD) $(DEBUGFLAGS) $(DOBJS) $(BUILDDIR)/flmain.dbg.obj -o $@ $(BUILDDIR)/$(LIBTARGET).a $(LIBS) $(OSLIBS))

$(BUILDDIR)/$(EXENAME): $(OBJS) $(LIBFILES) $(BUILDDIR)/$(LIBTARGET).a $(BUILDDIR)/flmain.o | $(BUILDDIR)/flisp.boot
	@$(call PRINT_LINK, $(CCLD) $(SHIPFLAGS) $(OBJS) $(BUILDDIR)/flmain.o -o $@ $(BUILDDIR)/$(LIBTARGET).a $(LIBS) $(OSLIBS))

$(BUILDDIR)/flisp.boot: flisp.boot
	cp $< $@

test:
ifneq ($(USEMSVC), 1)
	$(call spawn,./$(EXENAME)) unittest.lsp
endif

clean:
	rm -f $(BUILDDIR)/*.o
	rm -f $(BUILDDIR)/*.dbg.obj
	rm -f $(BUILDDIR)/*.a
	rm -f $(BUILDDIR)/$(EXENAME)
	rm -f $(BUILDDIR)/$(EXENAME)-debug
