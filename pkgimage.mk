SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/stdlib/stdlib.mk

DEPOTDIR := $(build_prefix)/share/julia
CHECKBOUNDS_DIR := $(BUILDROOT)/stdlib/checkbounds-$(JULIA_BUILD_MODE)
CHECKBOUNDS_DEPOT := $(CHECKBOUNDS_DIR)/share/julia

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

STDLIB_PKGIMAGE_CONFIGS := ``=>Base.CacheFlags(debug_level=2, opt_level=3)
ifeq ($(JULIA_CHECK_BOUNDS_PKGIMAGES),1)
STDLIB_PKGIMAGE_CONFIGS := $(STDLIB_PKGIMAGE_CONFIGS), ``=>Base.CacheFlags(check_bounds=1, debug_level=2, opt_level=3)
endif

$(DEPOTDIR)/compiled:
	mkdir -p $@

print-depot-path:
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e '@show Base.DEPOT_PATH')

# The precompile workers inherit the coverage configuration through `Base.CacheFlags`.
$(BUILDDIR)/stdlib/%.image: $(JULIAHOME)/stdlib/Project.toml $(JULIAHOME)/stdlib/Manifest.toml $(INDEPENDENT_STDLIBS_SRCS) $(DEPOTDIR)/compiled
	@$(call PRINT_JULIA, JULIA_CPU_TARGET="sysimage" $(call spawn,$(JULIA_EXECUTABLE)) $(JULIA_COVERAGE_IMAGE_FLAGS) --startup-file=no -e \
		'Base.Precompilation.precompilepkgs(configs=[$(STDLIB_PKGIMAGE_CONFIGS)]; strict=true)')
	touch $@

$(BUILDDIR)/stdlib/release.image: $(build_private_libdir)/sys.$(SHLIB_EXT)
$(BUILDDIR)/stdlib/debug.image: $(build_private_libdir)/sys-debug.$(SHLIB_EXT)

# Keep these caches out of the installed depot so binary-dist and installers
# contain only the ordinary images. The second depot supplies relocatable
# stdlib sources. Force regeneration so even builds that already bundle the
# checked images produce a complete, independent companion.
.PHONY: checkbounds-pkgimages
checkbounds-pkgimages: $(BUILDDIR)/stdlib/checkbounds-$(JULIA_BUILD_MODE).image
	cd $(CHECKBOUNDS_DIR) && $(TAR) -zcf $(abspath $(JULIA_BINARYDIST_FILENAME)).checkbounds.tar.gz share/julia/compiled

$(BUILDDIR)/stdlib/checkbounds-$(JULIA_BUILD_MODE).image: $(BUILDDIR)/stdlib/$(JULIA_BUILD_MODE).image $(JULIAHOME)/pkgimage.mk
	rm -rf $(CHECKBOUNDS_DIR)
	mkdir -p $(CHECKBOUNDS_DEPOT)
	@$(call PRINT_JULIA, JULIA_CPU_TARGET="sysimage" JULIA_DEPOT_PATH="$(call cygpath_w,$(CHECKBOUNDS_DEPOT))$(PATHSEP)$(call cygpath_w,$(DEPOTDIR))" $(call spawn,$(JULIA_EXECUTABLE)) $(JULIA_COVERAGE_IMAGE_FLAGS) --startup-file=no -e \
		'Base.Precompilation.precompilepkgs(configs=[``=>Base.CacheFlags(check_bounds=1, debug_level=2, opt_level=3)]; strict=true, force_stdlibs=true)')
	touch $@

clean:
	rm -rf $(BUILDROOT)/stdlib/checkbounds-release $(BUILDROOT)/stdlib/checkbounds-debug
	rm -rf $(DEPOTDIR)/compiled
	rm -f $(BUILDDIR)/stdlib/*.image
