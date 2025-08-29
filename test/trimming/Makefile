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
JULIA_LIBDIR := $(shell $(JULIA) -e 'println(joinpath(Sys.BINDIR, "..", "lib"))' --)
CPPFLAGS_ADD :=
CFLAGS_ADD = $(shell $(JULIA_CONFIG) --cflags)
LDFLAGS_ADD = -lm $(shell $(JULIA_CONFIG) --ldflags --ldlibs) -ljulia-internal

# get the JuliaC build script
JULIAC_BUILDSCRIPT := $(shell $(JULIA) -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "juliac", "juliac-buildscript.jl"))')

#=============================================================================

release: $(BIN)/hello$(EXE) $(BIN)/trimmability$(EXE) $(BIN)/basic_jll$(EXE)

$(BIN)/hello-o.a: $(SRCDIR)/hello.jl $(JULIAC_BUILDSCRIPT)
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-exe true

$(BIN)/trimmability-o.a: $(SRCDIR)/trimmability.jl $(JULIAC_BUILDSCRIPT)
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-exe true

$(BIN)/basic_jll-o.a: $(SRCDIR)/basic_jll.jl $(JULIAC_BUILDSCRIPT)
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --project=$(SRCDIR) -e "using Pkg; Pkg.instantiate()"
	$(JULIA) -t 1 -J $(JULIA_LIBDIR)/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --project=$(SRCDIR) --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(JULIAC_BUILDSCRIPT) $< --output-exe true

$(BIN)/hello$(EXE): $(BIN)/hello-o.a
	$(CC) -o $@ $(WHOLE_ARCHIVE) $< $(NO_WHOLE_ARCHIVE) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/trimmability$(EXE): $(BIN)/trimmability-o.a
	$(CC) -o $@ $(WHOLE_ARCHIVE) $< $(NO_WHOLE_ARCHIVE) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/basic_jll$(EXE): $(BIN)/basic_jll-o.a
	$(CC) -o $@ $(WHOLE_ARCHIVE) $< $(NO_WHOLE_ARCHIVE) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

check: $(BIN)/hello$(EXE) $(BIN)/trimmability$(EXE) $(BIN)/basic_jll$(EXE)
	$(JULIA) --depwarn=error $(SRCDIR)/trimming.jl $<

clean:
	-rm -f $(BIN)/hello$(EXE) $(BIN)/trimmability$(EXE) $(BIN)/basic_jll$(EXE) $(BIN)/hello-o.a $(BIN)/trimmability-o.a $(BIN)/basic_jll-o.a

.PHONY: release clean check

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
