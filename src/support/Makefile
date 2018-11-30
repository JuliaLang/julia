JULIAHOME := $(abspath ../..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)
override CPPFLAGS += $(JCPPFLAGS)

SRCS := hashing timefuncs ptrhash operators utf8 ios htable bitvector \
	int2str libsupportinit arraylist strtod
ifeq ($(OS),WINNT)
SRCS += asprintf strptime win32_ucontext
ifeq ($(ARCH),i686)
SRCS += _setjmp.win32
else ifeq ($(ARCH),x86_64)
SRCS += _setjmp.win64
endif
endif
ifeq ($(USEMSVC), 1)
SRCS += dirname
endif

FLAGS := $(HFILEDIRS:%=-I%) -I$(UTF8PROC_INC) -DLIBRARY_EXPORTS -DUTF8PROC_EXPORTS

ifneq ($(OS),wasm)
DEP_HEADERS += $(LIBUV_INC)/uv.h
FLAGS += -I$(LIBUV_INC)
endif

HEADERS := $(wildcard *.h) $(DEP_HEADERS)

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)

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
