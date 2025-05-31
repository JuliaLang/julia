SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/stdlib/stdlib.mk

DEPOTDIR := $(build_prefix)/share/julia

# set some influential environment variables
export JULIA_DEPOT_PATH := $(shell echo $(call cygpath_w,$(DEPOTDIR)))
export JULIA_LOAD_PATH := @stdlib$(PATHSEP)$(shell echo $(call cygpath_w,$(JULIAHOME)/stdlib))
unexport JULIA_PROJECT :=
unexport JULIA_BINDIR :=

export JULIA_FALLBACK_REPL := true

default: release
release: $(BUILDDIR)/stdlib/release.image
debug: $(BUILDDIR)/stdlib/debug.image
all: release debug

$(DEPOTDIR)/compiled:
	mkdir -p $@

print-depot-path:
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e '@show Base.DEPOT_PATH')

$(BUILDDIR)/stdlib/%.image: $(JULIAHOME)/stdlib/Project.toml $(JULIAHOME)/stdlib/Manifest.toml $(INDEPENDENT_STDLIBS_SRCS) $(DEPOTDIR)/compiled
	@$(call PRINT_JULIA, JULIA_CPU_TARGET="$(JULIA_CPU_TARGET)" $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e \
		'Base.Precompilation.precompilepkgs(configs=[``=>Base.CacheFlags(debug_level=2, opt_level=3), ``=>Base.CacheFlags(check_bounds=1, debug_level=2, opt_level=3)])')
	touch $@

$(BUILDDIR)/stdlib/release.image: $(build_private_libdir)/sys.$(SHLIB_EXT)
$(BUILDDIR)/stdlib/debug.image: $(build_private_libdir)/sys-debug.$(SHLIB_EXT)

clean:
	rm -rf $(DEPOTDIR)/compiled
	rm -f $(BUILDDIR)/stdlib/*.image
