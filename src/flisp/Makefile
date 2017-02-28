JULIAHOME := $(abspath ../..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

NAME := flisp
EXENAME := $(NAME)
LIBTARGET := lib$(NAME)

SRCS := flisp.c builtins.c string.c equalhash.c table.c iostream.c \
        julia_extensions.c

HEADERS := $(wildcard *.h) $(LIBUV_INC)/uv.h

OBJS := $(SRCS:%.c=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%.c=$(BUILDDIR)/%.dbg.obj)
LLTDIR := ../support
LLT_release := $(BUILDDIR)/$(LLTDIR)/libsupport.a
LLT_debug := $(BUILDDIR)/$(LLTDIR)/libsupport-debug.a
LIBFILES_release := $(LLT_release) $(LIBUV) $(LIBUTF8PROC)
LIBFILES_debug := $(LLT_debug) $(LIBUV) $(LIBUTF8PROC)
LIBS :=
ifneq ($(OS),WINNT)
LIBS += -lpthread
endif

FLAGS := -I$(LLTDIR) $(CFLAGS) $(HFILEDIRS:%=-I%) \
        -I$(LIBUV_INC) -I$(UTF8PROC_INC) -I$(build_includedir) $(LIBDIRS:%=-L%) \
        -DLIBRARY_EXPORTS -DUTF8PROC_EXPORTS
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -DUSE_COMPUTED_GOTO -fvisibility=hidden -Wpointer-arith -Wundef
FLAGS += -Wold-style-definition -Wstrict-prototypes -Wc++-compat
endif

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

default: release

release: $(BUILDDIR)/$(EXENAME)

debug: $(BUILDDIR)/$(EXENAME)-debug

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)

$(BUILDDIR)/flisp.o:   flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
$(BUILDDIR)/flisp.dbg.obj:  flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
$(BUILDDIR)/flmain.o:  flmain.c flisp.h
$(BUILDDIR)/flmain.dbg.obj: flmain.c flisp.h

$(LLT_release): $(LLTDIR)/*.h $(LLTDIR)/*.c
	$(MAKE) -C $(LLTDIR) BUILDDIR='$(abspath $(BUILDDIR)/$(LLTDIR))'
$(LLT_debug): $(LLTDIR)/*.h $(LLTDIR)/*.c
	$(MAKE) debug -C $(LLTDIR) BUILDDIR='$(abspath $(BUILDDIR)/$(LLTDIR))'

$(BUILDDIR)/$(LIBTARGET)-debug.a: $(DOBJS) | $(BUILDDIR)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $(DOBJS))

$(BUILDDIR)/$(LIBTARGET).a: $(OBJS) | $(BUILDDIR)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $(OBJS))

ifneq ($(USEMSVC), 1)
CCLD := $(CC)
else
CCLD := $(LD)
endif

$(BUILDDIR)/$(EXENAME)-debug: $(DOBJS) $(LIBFILES_debug) $(BUILDDIR)/$(LIBTARGET)-debug.a $(BUILDDIR)/flmain.dbg.obj | $(BUILDDIR)/flisp.boot
	@$(call PRINT_LINK, $(CCLD) $(DEBUGFLAGS) $(LDFLAGS) $(DOBJS) $(BUILDDIR)/flmain.dbg.obj -o $@ $(BUILDDIR)/$(LIBTARGET)-debug.a $(LIBFILES_debug) $(LIBS) $(OSLIBS))

$(BUILDDIR)/$(EXENAME): $(OBJS) $(LIBFILES_release) $(BUILDDIR)/$(LIBTARGET).a $(BUILDDIR)/flmain.o | $(BUILDDIR)/flisp.boot
	@$(call PRINT_LINK, $(CCLD) $(SHIPFLAGS) $(LDFLAGS) $(OBJS) $(BUILDDIR)/flmain.o -o $@ $(BUILDDIR)/$(LIBTARGET).a $(LIBFILES_release) $(LIBS) $(OSLIBS))

ifneq ($(BUILDDIR),.)
$(BUILDDIR)/flisp.boot: flisp.boot | $(BUILDDIR)
	cp $< $@
endif

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
