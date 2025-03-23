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
BUILDSCRIPT := $(BIN)/../share/julia/juliac-buildscript.jl
include $(JULIAHOME)/Make.inc

# get the executable suffix, if any
EXE := $(suffix $(abspath $(JULIA)))

# get compiler and linker flags. (see: `contrib/julia-config.jl`)
JULIA_CONFIG := $(JULIA) -e 'include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "julia-config.jl"))' --
CPPFLAGS_ADD :=
CFLAGS_ADD = $(shell $(JULIA_CONFIG) --cflags)
LDFLAGS_ADD = -lm $(shell $(JULIA_CONFIG) --ldflags --ldlibs) -ljulia-internal

#=============================================================================

release: hello$(EXE) basic_jll$(EXE)

hello.o: $(SRCDIR)/hello.jl $(BUILDSCRIPT)
	$(JULIA) -t 1 -J $(BIN)/../lib/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(BUILDSCRIPT) $(SRCDIR)/hello.jl --output-exe true

init.o: $(SRCDIR)/init.c
	$(CC) -c -o $@ $< $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS)

basic_jll.o: $(SRCDIR)/basic_jll.jl $(BUILDSCRIPT)
	$(JULIA) -t 1 -J $(BIN)/../lib/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --project=$(SRCDIR) -e "using Pkg; Pkg.instantiate()"
	$(JULIA) -t 1 -J $(BIN)/../lib/julia/sys.$(SHLIB_EXT) --startup-file=no --history-file=no --project=$(SRCDIR) --output-o $@ --output-incremental=no --strip-ir --strip-metadata --experimental --trim $(BUILDSCRIPT) $(SRCDIR)/basic_jll.jl --output-exe true

hello$(EXE): hello.o init.o
	$(CC) -o $@ $(WHOLE_ARCHIVE) hello.o $(NO_WHOLE_ARCHIVE) init.o $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

basic_jll$(EXE): basic_jll.o init.o
	$(CC) -o $@ $(WHOLE_ARCHIVE) basic_jll.o $(NO_WHOLE_ARCHIVE) init.o $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

check: hello$(EXE) basic_jll$(EXE)
	$(JULIA) --depwarn=error $(SRCDIR)/../runtests.jl $(SRCDIR)/trimming

clean:
	-rm -f hello$(EXE) basic_jll$(EXE) init.o hello.o basic_jll.o

.PHONY: release clean check

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
