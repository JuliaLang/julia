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
# this source directory where embedding.c is located
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# get the executable suffix, if any
EXE := $(suffix $(abspath $(JULIA)))

# get compiler and linker flags. (see: `contrib/julia-config.jl`)
JULIA_CONFIG := $(JULIA) -e 'include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "julia-config.jl"))' --
JULIA_LIBDIR := $(shell $(JULIA) -e 'println(joinpath(Sys.BINDIR, "..", "lib"))' --)
CPPFLAGS_ADD :=
CFLAGS_ADD = $(shell $(JULIA_CONFIG) --cflags)
LDFLAGS_ADD = -lm $(shell $(JULIA_CONFIG) --ldflags --ldlibs)

DEBUGFLAGS += -g

#=============================================================================

release: $(BIN)/embedding$(EXE) $(BIN)/libdl-embedding$(EXE)
debug:   $(BIN)/embedding-debug$(EXE) $(BIN)/libdl-embedding$(EXE)

$(BIN)/embedding$(EXE): $(SRCDIR)/embedding.c
	$(CC) $^ -o $@ $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/embedding-debug$(EXE): $(SRCDIR)/embedding.c
	$(CC) $^ -o $@ $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS) $(DEBUGFLAGS)

$(BIN)/libdl-embedding$(EXE): $(SRCDIR)/libdl_embedding.c
	$(CC) $^ -o $@ $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -ldl -DLIBJULIA_PATH=\"$(JULIA_LIBDIR)/libjulia.so\"

$(BIN)/libdl-embedding-debug$(EXE): $(SRCDIR)/libdl_embedding.c
	$(CC) $^ -o $@ $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(DEBUGFLAGS) -ldl -DLIBJULIA_PATH=\"$(JULIA_LIBDIR)/libjulia.so\"

ifneq ($(abspath $(BIN)),$(abspath $(SRCDIR)))
# for demonstration purposes, our demo code is also installed
# in $BIN, although this would likely not be typical
$(BIN)/LocalModule.jl: $(SRCDIR)/LocalModule.jl
	cp $< $@
endif

check: $(BIN)/embedding$(EXE) $(BIN)/libdl-embedding$(EXE) $(BIN)/LocalModule.jl
	$(BIN)/libdl-embedding$(EXE) # run w/o error
	$(JULIA) --depwarn=error $(SRCDIR)/embedding-test.jl $<
	@echo SUCCESS

clean:
	-rm -f $(BIN)/embedding-debug$(EXE) $(BIN)/embedding$(EXE)

.PHONY: release debug clean check

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
