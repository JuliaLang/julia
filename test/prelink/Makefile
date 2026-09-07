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
# this source directory, where prelink.c is located
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# get the executable suffix, if any
EXE := $(suffix $(abspath $(JULIA)))

# get compiler and linker flags. (see: `contrib/julia-config.jl`)
JULIA_CONFIG := $(JULIA) -e 'include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "julia-config.jl"))' --
CFLAGS_ADD = $(shell $(JULIA_CONFIG) --cflags)
LDFLAGS_ADD = -lm $(shell $(JULIA_CONFIG) --ldflags --ldlibs) -ljulia-internal

# The image is at one address at every start only if the program does not move.
FIXED_FLAGS := -no-pie

# `--output-o` builds an image from source unless it is given one to start from.
JULIA_SYSIMAGE := $(shell $(JULIA) --startup-file=no -e 'print(unsafe_string(Base.JLOptions().image_file))')
IMAGE_FLAGS := --startup-file=no --sysimage='$(JULIA_SYSIMAGE)' --sysimage-prelink=yes \
	--project=$(SRCDIR)/Prelinked -t1,0

#=============================================================================

release: $(BIN)/prelink$(EXE)
debug:   $(BIN)/prelink$(EXE)

$(BIN)/image.o.a: $(SRCDIR)/image.jl $(SRCDIR)/Prelinked/src/Prelinked.jl
	$(JULIA) $(IMAGE_FLAGS) --output-o $@ $(SRCDIR)/image.jl $(SRCDIR)/Prelinked

$(BIN)/prelink$(EXE): $(SRCDIR)/prelink.c $(BIN)/image.o.a
	$(CC) $(SRCDIR)/prelink.c -o $@ $(FIXED_FLAGS) $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) \
		-Wl,--whole-archive $(BIN)/image.o.a -Wl,--no-whole-archive \
		$(LDFLAGS_ADD) $(LDFLAGS)

# The same image in a program that moves at every start.
$(BIN)/prelink-pie$(EXE): $(SRCDIR)/prelink.c $(BIN)/image.o.a
	$(CC) $(SRCDIR)/prelink.c -o $@ $(CPPFLAGS) $(CFLAGS_ADD) $(CFLAGS) \
		-Wl,--whole-archive $(BIN)/image.o.a -Wl,--no-whole-archive \
		$(LDFLAGS_ADD) $(LDFLAGS)

# Writing the file needs Linux.
ifeq ($(shell uname),Linux)
check: check-prelink
else
check:
	@echo "a pre-relocated system image needs Linux; nothing to check here"
endif

check-prelink: $(BIN)/prelink$(EXE) $(BIN)/prelink-pie$(EXE)
	$(JULIA) --depwarn=error $(SRCDIR)/prelink-test.jl $(BIN)
	@echo SUCCESS

clean:
	-rm -f $(BIN)/prelink$(EXE) $(BIN)/prelink-pie$(EXE) \
		$(BIN)/prelinked$(EXE) $(BIN)/image.o.a

.PHONY: release debug clean check check-prelink

# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
print-%:
	@echo '$*=$($*)'
