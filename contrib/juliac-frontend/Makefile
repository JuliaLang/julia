# This file is a part of Julia. License is MIT: https://julialang.org/license

# Build a drop-in implementation of the libjulia-frontend interface from
# JuliaSyntax/JuliaLowering, compiled with juliac into a shared library that
# exports the jl_frontend_*_impl entry points (see README.md).
#
# Usage:
#   make            # build libjulia-frontend-jl.so and the test driver
#   make check      # run ABI smoke test plus parity tests against flisp

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

BIN := $(SRCDIR)/build
JULIA ?= $(build_bindir)/julia
JULIA_CMD := $(JULIA) --startup-file=no --history-file=no

JULIA_CONFIG := $(JULIA_CMD) -e 'include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "julia-config.jl"))' --
CFLAGS_ADD = $(shell $(JULIA_CONFIG) --cflags)
LDFLAGS_ADD = -lm $(shell $(JULIA_CONFIG) --ldflags --ldlibs) -ljulia-internal

JULIAC_BUILDSCRIPT := $(JULIAHOME)/contrib/juliac/juliac-buildscript.jl

FRONTEND_LIB := $(BIN)/libjulia-frontend-jl.$(SHLIB_EXT)

release: $(FRONTEND_LIB) $(BIN)/frontend_driver$(EXE)

$(BIN):
	mkdir -p $(BIN)

$(BIN)/Manifest.stamp: $(SRCDIR)/JuliaFrontend/Project.toml | $(BIN)
	$(JULIA_CMD) --project=$(SRCDIR)/JuliaFrontend -e 'using Pkg; Pkg.instantiate()'
	touch $@

$(BIN)/julia-frontend-jl-o.a: $(SRCDIR)/frontend_lib.jl $(SRCDIR)/JuliaFrontend/src/*.jl $(JULIAC_BUILDSCRIPT) $(BIN)/Manifest.stamp | $(BIN)
	$(JULIA_CMD) -t 1 -J $(build_private_libdir)/sys.$(SHLIB_EXT) --project=$(SRCDIR)/JuliaFrontend --output-o $@ --output-incremental=no \
		$(JULIAC_BUILDSCRIPT) $(SRCDIR)/frontend_lib.jl --output-lib true

$(FRONTEND_LIB): $(BIN)/julia-frontend-jl-o.a
	$(CC) -shared -o $@ $(call whole_archive,$<) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/frontend_driver$(EXE): $(SRCDIR)/frontend_driver.c | $(BIN)
	$(CC) -o $@ $< $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS) -ldl

check: release
	$(BIN)/frontend_driver$(EXE) $(FRONTEND_LIB) smoke
	$(JULIA_CMD) $(SRCDIR)/test_ops_parity.jl
	$(JULIA_CMD) $(SRCDIR)/test_frontend_parity.jl $(BIN)/frontend_driver$(EXE) $(FRONTEND_LIB)

clean:
	rm -rf $(BIN)

.PHONY: release check clean

print-%:
	@echo '$*=$($*)'
