JULIAHOME := $(abspath ../..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

SRCS := hashing timefuncs ptrhash operators utf8 ios htable bitvector \
	int2str libsupportinit arraylist strtod crc32c
ifeq ($(OS),WINNT)
SRCS += asprintf wsasocketpair strptime
ifeq ($(ARCH),i686)
SRCS += _setjmp.win32 _longjmp.win32
else ifeq ($(ARCH),i386)
SRCS += _setjmp.win32 _longjmp.win32
else ifeq ($(ARCH),x86_64)
SRCS += _setjmp.win64 _longjmp.win64
endif
endif
ifeq ($(USEMSVC), 1)
SRCS += dirname
endif

HEADERS := $(wildcard *.h) $(LIBUV_INC)/uv.h

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)

FLAGS := $(HFILEDIRS:%=-I%) -I$(LIBUV_INC) -I$(UTF8PROC_INC) -DLIBRARY_EXPORTS -DUTF8PROC_EXPORTS
ifneq ($(USEMSVC), 1)
FLAGS += -Wall -Wno-strict-aliasing -fvisibility=hidden -Wpointer-arith -Wundef
override CFLAGS += -Wold-style-definition -Wstrict-prototypes -Wc++-compat
endif

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

default: release

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: %.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)
ifneq ($(USEMSVC), 1)
$(BUILDDIR)/%.o: %.S | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(SHIPFLAGS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: %.S | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)
else
$(BUILDDIR)/%.o: %.S | $(BUILDDIR)
	@$(call PRINT_CC, $(CPP) -P $(CPPFLAGS) $(SHIPFLAGS) $<)
	@$(call PRINT_CC, $(AS) $(CPPFLAGS) $(SHIPFLAGS) -Fo $@ -c $*.i)
$(BUILDDIR)/%.dbg.obj: %.S | $(BUILDDIR)
	@$(call PRINT_CC, $(CPP) -P $(CPPFLAGS) $(DEBUGFLAGS) $<)
	@$(call PRINT_CC, $(AS) $(CPPFLAGS) $(DEBUGFLAGS) -Fo $@ -c $*.i)
endif

release: $(BUILDDIR)/libsupport.a
debug: $(BUILDDIR)/libsupport-debug.a

$(BUILDDIR)/libsupport.a: $(OBJS)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $^)

$(BUILDDIR)/libsupport-debug.a: $(DOBJS)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $^)

clean:
	rm -f $(BUILDDIR)/*.o
	rm -f $(BUILDDIR)/*.dbg.obj
	rm -f $(BUILDDIR)/*.a
	rm -f $(BUILDDIR)/*~ *#
	rm -f $(BUILDDIR)/core*
	rm -f $(BUILDDIR)/libsupport.a
	rm -f $(BUILDDIR)/libsupport-debug.a
