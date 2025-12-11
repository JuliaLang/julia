# This file is a part of Julia. License is MIT: https://julialang.org/license

# This Makefile template requires the following variables to be set
# in the environment or on the command-line:
#   JULIA: path to julia[.exe] executable
#   BIN:   binary build directory

ifndef JULIA
  $(error "Please pass JULIA=[path of target julia binary], or set as environment variable!")
endif
ifndef BIN
  $(error "Please pass BIN=[path of build directory], or set as environment variable!")
endif

#=============================================================================
# location of test source
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

# get the executable suffix, if any
EXE := $(suffix $(abspath $(JULIA)))

# get compiler and linker flags. (see: `contrib/julia-config.jl`)
JULIA_CONFIG := $(JULIA) -e 'include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "julia-config.jl"))' --
JULIA_LIBDIR := $(shell $(JULIA)  --startup-file=no -e 'println(joinpath(Sys.BINDIR, "..", "lib"))' --)
CPPFLAGS_ADD :=
CFLAGS_ADD = $(shell $(JULIA_CONFIG) --cflags)
LDFLAGS_ADD = -lm $(shell $(JULIA_CONFIG) --ldflags --ldlibs) -ljulia-internal

# get the JuliaC build script
JULIAC_BUILDSCRIPT := $(shell $(JULIA) --startup-file=no -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "juliac", "juliac-buildscript.jl"))')

#=============================================================================

release: $(BIN)/hello$(EXE) $(BIN)/trimmability$(EXE) $(BIN)/basic_jll$(EXE) $(BIN)/capplication$(EXE)

# Complete any needed Pkg operations before trimming (some Pkg code may not be trimmable)
Manifest.toml: $(SRCDIR)/Project.toml
	$(JULIA) --startup-file=no --history-file=no -e 'using Pkg; Pkg.activate("$(SRCDIR)"); Pkg.instantiate()'

$(BIN)/hello-o.a: $(SRCDIR)/hello.jl $(JULIAC_BUILDSCRIPT) Manifest.toml
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-exe true

$(BIN)/trimmability-o.a: $(SRCDIR)/trimmability.jl $(JULIAC_BUILDSCRIPT) Manifest.toml
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-exe true

$(BIN)/basic_jll-o.a: $(SRCDIR)/basic_jll.jl $(JULIAC_BUILDSCRIPT) Manifest.toml
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --project=$(SRCDIR) --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-exe true

$(BIN)/libsimple-o.a: $(SRCDIR)/libsimple.jl $(JULIAC_BUILDSCRIPT)
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-lib true $(BIN)/bindinginfo_libsimple.json

$(BIN)/hello$(EXE): $(BIN)/hello-o.a
	$(CC) -o $@ $(call whole_archive,$<) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/trimmability$(EXE): $(BIN)/trimmability-o.a
	$(CC) -o $@ $(call whole_archive,$<) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/basic_jll$(EXE): $(BIN)/basic_jll-o.a
	$(CC) -o $@ $(call whole_archive,$<) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/capplication$(EXE): $(SRCDIR)/capplication.c $(SRCDIR)/libsimple.h $(BIN)/libsimple-o.a
	$(CC) -I$(BIN) -I$(SRCDIR) -I$(JULIA_LIBDIR) -o $@ $< -Wl,--whole-archive $(BIN)/libsimple-o.a -Wl,--no-whole-archive $(LDFLAGS_ADD) $(LDFLAGS) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS)

check: $(BIN)/hello$(EXE) $(BIN)/trimmability$(EXE) $(BIN)/basic_jll$(EXE) $(BIN)/capplication$(EXE)
	$(JULIA) --startup-file=no --history-file=no --depwarn=error $(SRCDIR)/trimming.jl $<

clean:
	-rm -f $(BIN)/hello$(EXE) $(BIN)/trimmability$(EXE) $(BIN)/basic_jll$(EXE) $(BIN)/hello-o.a $(BIN)/trimmability-o.a $(BIN)/basic_jll-o.a $(BIN)/libsimple-o.a $(BIN)/libsimple.$(SHLIB_EXT) $(BIN)/capplication$(EXE) $(BIN)/bindinginfo_libsimple.json $(BIN)/Manifest.toml

.PHONY: release clean check

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
