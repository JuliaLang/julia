SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
JULIASRC := $(abspath $(JULIAHOME)/src)
JULIASUPPORT := $(abspath $(JULIAHOME)/src/support)
BUILDDIR := .
# Get the name of the parent of BUILDDIR since there will be a few automatically generated files there
SRCBUILDDIR := $(abspath $(BUILDDIR)/..)
include $(JULIAHOME)/Make.inc

JCFLAGS += $(CFLAGS)
JCXXFLAGS += $(CXXFLAGS)
JCPPFLAGS += $(CPPFLAGS)
JLDFLAGS += $(LDFLAGS)

SRCS := gc-debug gc-page-profiler gc-pages gc-stacks gc

OBJS := $(SRCS:%=$(BUILDDIR)/%.o)
DOBJS := $(SRCS:%=$(BUILDDIR)/%.dbg.obj)

HEADERS := $(wildcard *.h) $(LIBUV_INC)/uv.h

FLAGS := $(HFILEDIRS:%=-I%) -I$(LIBUV_INC) -I$(UTF8PROC_INC) -I$(JULIASRC) -I$(JULIASUPPORT) -I$(SRCBUILDDIR) -I. -DJL_LIBRARY_EXPORTS_INTERNAL
FLAGS += -Wall -Wno-strict-aliasing -fvisibility=hidden -Wpointer-arith -Wundef -fPIC
JCFLAGS += -Wold-style-definition -Wstrict-prototypes -Wc++-compat

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)

default: release

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(JCPPFLAGS) $(SHIPFLAGS) $(DISABLE_ASSERTIONS) -c $< -o $@)
$(BUILDDIR)/%.dbg.obj: $(SRCDIR)/%.c $(HEADERS) | $(BUILDDIR)
	@$(call PRINT_CC, $(CC) $(JCPPFLAGS) $(DEBUGFLAGS) -c $< -o $@)

release: $(BUILDDIR)/libgc.a
debug: $(BUILDDIR)/libgc-debug.a

$(BUILDDIR)/libgc.a: $(OBJS) | $(BUILDIR)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $^)

$(BUILDDIR)/libgc-debug.a: $(DOBJS) | $(BUILDDIR)
	rm -rf $@
	@$(call PRINT_LINK, $(AR) -rcs $@ $^)

clean:
	rm -f $(BUILDDIR)/*.o
	rm -f $(BUILDDIR)/*.dbg.obj
	rm -f $(BUILDDIR)/*.a
	rm -f $(BUILDDIR)/*~ *#
	rm -f $(BUILDDIR)/core*
	rm -f $(BUILDDIR)/libgc.a
	rm -f $(BUILDDIR)/libgc-debug.a
	rm -f $(BUILDDIR)/host/*
