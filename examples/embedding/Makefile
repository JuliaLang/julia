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
# get the executable suffix, if any
EXE := $(suffix $(abspath $(JULIA)))

# get compiler and linker flags. (see: `contrib/julia-config.jl`)
CFLAGS += -lm $(shell $(SPAWN) $(JULIA) -e \
                           'include(joinpath(JULIA_HOME,Base.DATAROOTDIR,"julia","julia-config.jl"))' \
                           -- --cflags --ldflags --ldlibs)

DEBUGFLAGS += -g

#=============================================================================

release: $(BIN)/embedding$(EXE)
debug:   $(BIN)/embedding-debug$(EXE)

$(BIN)/embedding$(EXE): embedding.c
	$(CC) $^ -o $@ $(CFLAGS)

$(BIN)/embedding-debug$(EXE): embedding.c
	$(CC) $^ -o $@ $(CFLAGS) $(DEBUGFLAGS)

clean:
	@rm -f $(BIN)/embedding-debug$(EXE) $(BIN)/embedding$(EXE)

.PHONY: release debug clean

