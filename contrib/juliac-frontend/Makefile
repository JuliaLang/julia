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

$(BIN)/julia-frontend-jl-o.a: $(SRCDIR)/frontend_lib.jl $(SRCDIR)/JuliaFrontend/src/*.jl \
		$(wildcard $(JULIAHOME)/JuliaSyntax/src/*.jl $(JULIAHOME)/JuliaSyntax/src/*/*.jl) \
		$(wildcard $(JULIAHOME)/JuliaLowering/src/*.jl) \
		$(JULIAC_BUILDSCRIPT) $(BIN)/Manifest.stamp $(build_private_libdir)/sys.$(SHLIB_EXT) | $(BIN)
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

#=============================================================================
# Self-contained standalone frontend: the runtime objects, support libraries
# and the JuliaSyntax/JuliaLowering image linked into a single shared library
# with all symbols internalized except the frontend trampoline ABI, so that
# it can live in the same process as a different (host) julia runtime.

STANDALONE_LIB := $(BIN)/libjulia-frontend-standalone.$(SHLIB_EXT)

# The private runtime: all of libjulia-internal's objects except
# null_sysimage.o (the embedded image provides jl_image_unpack instead).
JL_RT_OBJS = $(addprefix $(JULIAHOME)/src/,$(filter-out null_sysimage.o,\
	$(notdir $(shell $(MAKE) -s -C $(JULIAHOME)/src print-OBJS | sed 's/^OBJS=//'))))

# Bind the codegen and frontend trampoline names directly to their fallback
# implementations (codegen-stubs.c / frontend-stubs.c) inside the library —
# except the operator queries, which this runtime's own Julia code consults
# (e.g. Meta.isoperator during lowering's Expr compatibility handling) and
# which must therefore give real answers from the embedded classifier.
SELF_OP_QUERIES := jl_is_operator jl_is_unary_operator \
	jl_is_unary_and_binary_operator jl_is_syntactic_operator jl_operator_precedence
TRAMPOLINE_NAMES := $(filter-out YY $(SELF_OP_QUERIES),$(shell grep -oE '^[[:space:]]+YY\([a-zA-Z0-9_]+\)' \
	$(JULIAHOME)/src/jl_exported_funcs.inc | grep -oE '[a-zA-Z0-9_]+'))
DEFSYM_FLAGS := $(foreach n,$(TRAMPOLINE_NAMES),-Wl,--defsym,$(n)=$(n)_fallback)
DEFSYM_FLAGS += $(foreach n,$(SELF_OP_QUERIES),-Wl,--defsym,$(n)=$(patsubst jl_%,jlfe_%,$(n)))

# The runtime functions in JL_RUNTIME_EXPORTED_FUNCS are defined under ijl_*
# names; the CLI loader normally provides the jl_* trampolines for by-name
# lookups (ccall/cglobal). Provide them as plain aliases here.
RENAMED_RT_FUNCS := $(filter-out XX jl_setjmp,$(shell grep -oE '^[[:space:]]+XX\([a-zA-Z0-9_]+\)' \
	$(JULIAHOME)/src/jl_exported_funcs.inc | grep -oE '[a-zA-Z0-9_]+'))
DEFSYM_FLAGS += $(foreach n,$(RENAMED_RT_FUNCS),-Wl,--defsym,$(n)=i$(n))

FE_STANDALONE_CPPFLAGS := -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support \
	-I$(build_includedir) -I$(LIBUV_INC) -I$(UTF8PROC_INC) \
	-DJL_LIBRARY_EXPORTS_INTERNAL -D_GNU_SOURCE

$(BIN)/fe_standalone_entry.o: $(SRCDIR)/fe_standalone_entry.c | $(BIN)
	$(CC) $(JCFLAGS) $(FE_STANDALONE_CPPFLAGS) $(fPIC) -c $< -o $@

$(BIN)/fe_standalone_shim.o: $(SRCDIR)/fe_standalone_shim.c | $(BIN)
	$(CC) $(JCFLAGS) $(FE_STANDALONE_CPPFLAGS) $(fPIC) -c $< -o $@

# n.b. compiled without JL_LIBRARY_EXPORTS_INTERNAL; see the file comment
$(BIN)/fe_standalone_globals.o: $(SRCDIR)/fe_standalone_globals.c | $(BIN)
	$(CC) $(JCFLAGS) $(filter-out -DJL_LIBRARY_EXPORTS_INTERNAL,$(FE_STANDALONE_CPPFLAGS)) $(fPIC) -c $< -o $@

# The image archive carries its own definitions of a few runtime globals
# for fully-static links; weaken them so the runtime objects' definitions
# win (all references unify on one copy).
$(BIN)/julia-frontend-img-std.a: $(BIN)/julia-frontend-jl-o.a
	cp $< $@
	$(OBJCOPY) --weaken-symbol jl_tls_offset --weaken-symbol jl_small_typeof $@

# Isolation comes from how the library is loaded (RTLD_LOCAL | RTLD_DEEPBIND)
# plus -Bsymbolic, rather than from hiding symbols: the guest runtime needs
# its own jl_*/ijl_*/uv_* symbols visible to dlsym on its own handle (julia
# resolves runtime ccalls by name through jl_dlsym).
$(STANDALONE_LIB): $(BIN)/fe_standalone_entry.o $(BIN)/fe_standalone_shim.o $(BIN)/fe_standalone_globals.o \
		$(BIN)/julia-frontend-img-std.a $(SRCDIR)/Makefile
	$(CXX) -shared -static-libstdc++ -static-libgcc -o $@ \
		$(BIN)/fe_standalone_entry.o $(BIN)/fe_standalone_shim.o $(BIN)/fe_standalone_globals.o \
		-Wl,--whole-archive $(BIN)/julia-frontend-img-std.a -Wl,--no-whole-archive \
		$(JL_RT_OBJS) \
		$(JULIAHOME)/src/support/libsupport.a \
		$(build_libdir)/libtarget_parsing.a \
		$(build_libdir)/libuv.a $(build_libdir)/libutf8proc.a \
		$(build_libdir)/libunwind.a $(build_libdir)/libunwind-x86_64.a \
		$(DEFSYM_FLAGS) \
		-Wl,-Bsymbolic \
		-L$(build_libdir) -Wl,-rpath,$(build_libdir) -lzstd -ldl -lpthread -lm -lrt

$(BIN)/host_driver$(EXE): $(SRCDIR)/host_driver.c | $(BIN)
	$(CC) -o $@ $< $(CFLAGS_ADD) $(CFLAGS) $(LDFLAGS_ADD) $(LDFLAGS) -ldl

standalone: $(STANDALONE_LIB) $(BIN)/host_driver$(EXE)

check-standalone: standalone
	$(BIN)/host_driver$(EXE) $(STANDALONE_LIB)

clean:
	rm -rf $(BIN)

.PHONY: release check standalone check-standalone clean

print-%:
	@echo '$*=$($*)'
