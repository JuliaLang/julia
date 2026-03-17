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
LDFLAGS_ADD = -lm -lpthread $(shell $(JULIA_CONFIG) --ldflags --ldlibs)

DEBUGFLAGS += -g

# Path conversion for passing paths to Julia on Windows (MSYS2/Cygwin)
ifeq ($(OS),Windows_NT)
cygpath_w = `cygpath -w $(1)`
else
cygpath_w = $(1)
endif

#=============================================================================
# Test list: each entry has a matching .c source and -test.jl script

TESTS := embedding

ifeq ($(OS),Windows_NT)
TESTS += embedding-signals-win
else
TESTS += embedding-signals-unix libdl-embedding
ifeq ($(shell uname -s),Darwin)
TESTS += embedding-signals-macos
endif
endif

TARGETS       := $(addprefix $(BIN)/,$(addsuffix $(EXE),$(TESTS)))
DEBUG_TARGETS := $(addprefix $(BIN)/,$(addsuffix -debug$(EXE),$(TESTS)))

release: $(TARGETS)
debug:   $(DEBUG_TARGETS)

#=============================================================================
# Build rules

$(BIN)/%$(EXE): $(SRCDIR)/%.c
	$(CC) $^ -o $@ $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

$(BIN)/%-debug$(EXE): $(SRCDIR)/%.c
	$(CC) $^ -o $@ $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS) $(DEBUGFLAGS)

$(BIN)/libdl-embedding$(EXE): $(SRCDIR)/libdl-embedding.c
	$(CC) $^ -o $@ $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -ldl -DLIBJULIA_PATH=\"$(JULIA_LIBDIR)/libjulia.so\"

$(BIN)/libdl-embedding-debug$(EXE): $(SRCDIR)/libdl-embedding.c
	$(CC) $^ -o $@ $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(DEBUGFLAGS) -ldl -DLIBJULIA_PATH=\"$(JULIA_LIBDIR)/libjulia.so\"

ifneq ($(abspath $(BIN)),$(abspath $(SRCDIR)))
# for demonstration purposes, our demo code is also installed
# in $BIN, although this would likely not be typical
$(BIN)/LocalModule.jl: $(SRCDIR)/LocalModule.jl
	cp $< $@
endif

check: $(TARGETS) $(BIN)/LocalModule.jl
	$(foreach test,$(TESTS),$(JULIA) --depwarn=error $(call cygpath_w,$(SRCDIR)/$(test)-test.jl) $(call cygpath_w,$(BIN)/$(test)$(EXE)) &&) true
	@echo SUCCESS

clean:
	-rm -f $(TARGETS) $(DEBUG_TARGETS)

.PHONY: release debug clean check

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
