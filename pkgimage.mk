SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/stdlib/stdlib.mk


# set some influential environment variables
export JULIA_DEPOT_PATH := $(build_prefix)/share/julia
export JULIA_LOAD_PATH := @stdlib:$(JULIAHOME)/stdlib
export JULIA_CPU_TARGET := $(JULIA_CPU_TARGET)
unexport JULIA_PROJECT :=
unexport JULIA_BINDIR :=

export JULIA_FALLBACK_REPL := true

default: release
release: all-release
debug: all-debug
all: release debug

$(JULIA_DEPOT_PATH):
	mkdir -p $@

print-depot-path:
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e '@show Base.DEPOT_PATH')

all-release all-debug:
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e 'Base.Precompilation.precompilepkgs(;configs=[``=>Base.CacheFlags(), `--check-bounds=yes`=>Base.CacheFlags(;check_bounds=1)])')
