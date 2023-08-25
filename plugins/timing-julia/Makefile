# This file is a part of Julia. License is MIT: https://julialang.org/license

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
BUILDDIR ?= .

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make

HEADERS := $(addprefix $(JULIAHOME)/src/,julia.h julia_fasttls.h support/platform.h support/dirpath.h)

PLUGIN_CFLAGS = $(JCFLAGS) -I$(BUILDROOT)/src -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir)
PLUGIN_LDFLAGS = $(JLDFLAGS) -L$(build_shlibdir) -L$(build_libdir)

LIB_OBJS := $(BUILDDIR)/timing-julia.o
LIB_DOBJS := $(BUILDDIR)/timing-julia.dbg.obj

$(BUILDDIR)/timing-julia.o : $(SRCDIR)/timing-julia.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(SHIPFLAGS) $(PLUGIN_CFLAGS) -c $< -o $@)
$(BUILDDIR)/timing-julia.dbg.obj : $(SRCDIR)/timing-julia.c $(HEADERS)
	@$(call PRINT_CC, $(CC) $(DEBUGFLAGS) $(PLUGIN_CFLAGS) -c $< -o $@)

libtiming-julia-release: $(build_shlibdir)/libtiming-julia.$(SHLIB_EXT)
libtiming-julia-debug: $(build_shlibdir)/libtiming-julia-debug.$(SHLIB_EXT)

$(build_shlibdir)/libtiming-julia.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_OBJS) | $(build_shlibdir) $(build_libdir)
	@$(call PRINT_LINK, $(CC) $(call IMPLIB_FLAGS,$@.tmp) $(PLUGIN_CFLAGS) -shared $(SHIPFLAGS) $(LIB_OBJS) $(RPATH_LIB) -o $@ \
		$(JLIBLDFLAGS) $(PLUGIN_LDFLAGS) $(call SONAME_FLAGS,libtiming-julia.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libtiming-julia.$(JL_MAJOR_SHLIB_EXT) $@
	@$(DSYMUTIL) $@

$(build_shlibdir)/libtiming-julia-debug.$(JL_MAJOR_MINOR_SHLIB_EXT): $(LIB_DOBJS) | $(build_shlibdir) $(build_libdir)
	@$(call PRINT_LINK, $(CC) $(call IMPLIB_FLAGS,$@.tmp) $(PLUGIN_CFLAGS) -shared $(DEBUGFLAGS) $(LIB_DOBJS) $(RPATH_LIB) -o $@ \
		$(JLIBLDFLAGS) $(PLUGIN_LDFLAGS) $(call SONAME_FLAGS,libtiming-julia-debug.$(JL_MAJOR_SHLIB_EXT)))
	@$(INSTALL_NAME_CMD)libtiming-julia-debug.$(JL_MAJOR_SHLIB_EXT) $@
	@$(DSYMUTIL) $@

ifneq ($(OS), WINNT)
$(build_shlibdir)/libtiming-julia.$(JL_MAJOR_SHLIB_EXT) $(build_shlibdir)/libtiming-julia-debug.$(JL_MAJOR_SHLIB_EXT): $(build_shlibdir)/libtiming-julia%.$(JL_MAJOR_SHLIB_EXT): \
		$(build_shlibdir)/libtiming-julia%.$(JL_MAJOR_MINOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
$(build_shlibdir)/libtiming-julia.$(SHLIB_EXT) $(build_shlibdir)/libtiming-julia-debug.$(SHLIB_EXT): $(build_shlibdir)/libtiming-julia%.$(SHLIB_EXT): \
		$(build_shlibdir)/libtiming-julia%.$(JL_MAJOR_MINOR_SHLIB_EXT) $(build_shlibdir)/libtiming-julia%.$(JL_MAJOR_SHLIB_EXT)
	@$(call PRINT_LINK, ln -sf $(notdir $<) $@)
endif

#=============================================================================

release: libtiming-julia-release
debug:   libtiming-julia-debug

clean:
	-rm -fr $(build_shlibdir)/libtiming-julia*
	-rm -f $(LIB_OBJS) $(LIB_DOBJS)

.PHONY: release debug clean
