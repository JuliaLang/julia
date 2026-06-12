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

check: check-embedding

check-embedding: $(BIN)/embedding$(EXE) $(BIN)/libdl-embedding$(EXE) $(BIN)/LocalModule.jl
	$(BIN)/libdl-embedding$(EXE) # run w/o error
	$(JULIA) --depwarn=error $(SRCDIR)/embedding-test.jl $<
	@echo SUCCESS

#=============================================================================
# ASan host test: embeds the *stock* (non-sanitizer) Julia build inside an
# AddressSanitizer-instrumented host (a configuration not covered by the
# full-ASan CI job). See asan-embedding.c for details.

ASANFLAGS := -g -O1 -fno-omit-frame-pointer -fsanitize=address

CC_SUPPORTS_STATIC_ASAN := $(shell echo 'int main(void){return 0;}' | \
	$(CC) -x c - -fsanitize=address -static-libasan -o /dev/null 2>/dev/null && echo 1)
CC_SUPPORTS_ASAN := $(shell echo 'int main(void){return 0;}' | \
	$(CC) -x c - -fsanitize=address -o /dev/null 2>/dev/null && echo 1)

ASAN_TEST_ENV := \
	ASAN_OPTIONS="detect_leaks=0:allow_user_segv_handler=1:abort_on_error=1" \
	LBT_USE_RTLD_DEEPBIND=0

$(BIN)/asan-embedding$(EXE): $(SRCDIR)/asan-embedding.c
	$(CC) $^ -o $@ $(ASANFLAGS) $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

# baseline without ASan, to prove the test logic and the Julia build are healthy
$(BIN)/asan-embedding-plain$(EXE): $(SRCDIR)/asan-embedding.c
	$(CC) $^ -o $@ -g -O1 $(CPPFLAGS_ADD) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS)

ifeq ($(shell uname)-$(CC_SUPPORTS_STATIC_ASAN),Linux-1)
ASANFLAGS += -static-libasan
check: check-asan
else ifeq ($(shell uname)-$(CC_SUPPORTS_ASAN),Linux-1)
check: check-asan
endif

# JULIA_ASAN_COMPAT=0 (workarounds forced off) is expected to crash and can be
# run manually as a negative control.
check-asan: $(BIN)/asan-embedding-plain$(EXE) $(BIN)/asan-embedding$(EXE)
	$(BIN)/asan-embedding-plain$(EXE)
	env $(ASAN_TEST_ENV) $(BIN)/asan-embedding$(EXE)
	env $(ASAN_TEST_ENV) JULIA_ASAN_COMPAT=1 $(BIN)/asan-embedding$(EXE)
	@echo SUCCESS

clean:
	-rm -f $(BIN)/embedding-debug$(EXE) $(BIN)/embedding$(EXE) \
		$(BIN)/asan-embedding$(EXE) $(BIN)/asan-embedding-plain$(EXE)

.PHONY: release debug clean check check-embedding check-asan

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
