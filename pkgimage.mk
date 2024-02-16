SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/stdlib/stdlib.mk


# set some influential environment variables
export JULIA_DEPOT_PATH := $(build_prefix)/share/julia
export JULIA_LOAD_PATH := @stdlib
unexport JULIA_PROJECT :=
unexport JULIA_BINDIR :=

export JULIA_FALLBACK_REPL := true

default: release
release: all-release
debug: all-debug
all: release debug

$(JULIA_DEPOT_PATH):
	mkdir -p $@

print-depot-path:
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e '@show Base.DEPOT_PATH')

all-release: $(addprefix cache-release-, $(STDLIBS))
all-debug:   $(addprefix cache-debug-, $(STDLIBS))

define stdlib_builder
ifneq ($(filter $(1),$(INDEPENDENT_STDLIBS) $(PKG_EXTS)),)
# Define target-specific export for `JULIA_CPU_TARGET`
$$(BUILDDIR)/stdlib/$1.release.image: export JULIA_CPU_TARGET=$(JULIA_CPU_TARGET)
$$(BUILDDIR)/stdlib/$1.debug.image: export JULIA_CPU_TARGET=$(JULIA_CPU_TARGET)
ifneq ($(filter $(1),$(INDEPENDENT_STDLIBS)),)
$$(BUILDDIR)/stdlib/$1.release.image: $$($1_SRCS) $$(addsuffix .release.image,$$(addprefix $$(BUILDDIR)/stdlib/,$2)) $(build_private_libdir)/sys.$(SHLIB_EXT)
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no --check-bounds=yes --debug=yes -e 'Base.compilecache(Base.identify_package("$1"))')
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no -e 'Base.compilecache(Base.identify_package("$1"))')
	touch $$@
$$(BUILDDIR)/stdlib/$1.debug.image: $$($1_SRCS) $$(addsuffix .debug.image,$$(addprefix $$(BUILDDIR)/stdlib/,$2)) $(build_private_libdir)/sys-debug.$(SHLIB_EXT)
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no --check-bounds=yes --debug=yes -e 'Base.compilecache(Base.identify_package("$1"))')
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no -e 'Base.compilecache(Base.identify_package("$1"))')
	touch $$@
endif
ifneq ($(filter $(1),$(PKG_EXTS)),)
# This is weird. It set up for multiple Pkg exts because that suits the structure here better
# but it hard codes the deps and `import REPL, Pkg`
$$(BUILDDIR)/stdlib/REPLExt.release.image: $$(REPLExt_SRCS) $$(BUILDDIR)/stdlib/Pkg.release.image $$(BUILDDIR)/stdlib/REPL.release.image
	@$$(call PRINT_JULIA, $$(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no --check-bounds=yes -e 'using REPL; using Pkg')
	@$$(call PRINT_JULIA, $$(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e 'using REPL; using Pkg')
	touch $$@
$$(BUILDDIR)/stdlib/REPLExt.debug.image: $$(REPLExt_SRCS) $(BUILDDIR)/stdlib/Pkg.debug.image $$(BUILDDIR)/stdlib/REPL.debug.image
	@$$(call PRINT_JULIA, $$(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no --check-bounds=yes -e 'using REPL; using Pkg')
	@$$(call PRINT_JULIA, $$(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e 'using REPL; using Pkg')
	touch $$@
endif
else
ifneq ($(filter $(1),$(STDLIBS_WITHIN_SYSIMG)),)
$$(BUILDDIR)/stdlib/$1.release.image:
	touch $$@
$$(BUILDDIR)/stdlib/$1.debug.image:
	touch $$@
else
$$(error $(1) neither in STDLIBS_WITHIN_SYSIMG nor INDEPENDENT_STDLIBS)
endif
endif
cache-release-$1: $$(BUILDDIR)/stdlib/$1.release.image
cache-debug-$1: $$(BUILDDIR)/stdlib/$1.debug.image
.SECONDARY: $$(BUILDDIR)/stdlib/$1.release.image $$(BUILDDIR)/stdlib/$1.debug.image
endef

# Note: you can check for the correctness of this tree by running `JULIA_DEBUG=nested_precomp make` and looking
# out for `Debug: Nested precompilation` logs.

# no dependencies
$(eval $(call stdlib_builder,MozillaCACerts_jll,))
$(eval $(call stdlib_builder,ArgTools,))
$(eval $(call stdlib_builder,Artifacts,))
$(eval $(call stdlib_builder,Base64,))
$(eval $(call stdlib_builder,CRC32c,))
$(eval $(call stdlib_builder,FileWatching,))
$(eval $(call stdlib_builder,Libdl,))
$(eval $(call stdlib_builder,Mmap,))
$(eval $(call stdlib_builder,NetworkOptions,))
$(eval $(call stdlib_builder,SHA,))
$(eval $(call stdlib_builder,Serialization,))
$(eval $(call stdlib_builder,Sockets,))
$(eval $(call stdlib_builder,Unicode,))
$(eval $(call stdlib_builder,Profile,))
$(eval $(call stdlib_builder,StyledStrings,))
$(eval $(call stdlib_builder,SuiteSparse_jll,))

# 1-depth packages
$(eval $(call stdlib_builder,GMP_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,LLVMLibUnwind_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,LibUV_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,LibUnwind_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,MbedTLS_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,nghttp2_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,OpenLibm_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,PCRE2_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,Zlib_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,dSFMT_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,libLLVM_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,libblastrampoline_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,p7zip_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,OpenBLAS_jll,Artifacts Libdl))
$(eval $(call stdlib_builder,Markdown,Base64))
$(eval $(call stdlib_builder,Printf,Unicode))
$(eval $(call stdlib_builder,Random,SHA))
$(eval $(call stdlib_builder,Logging,StyledStrings))
$(eval $(call stdlib_builder,Tar,ArgTools,SHA))
$(eval $(call stdlib_builder,DelimitedFiles,Mmap))
$(eval $(call stdlib_builder,JuliaSyntaxHighlighting,StyledStrings))

# 2-depth packages
$(eval $(call stdlib_builder,LLD_jll,Zlib_jll libLLVM_jll Artifacts Libdl))
$(eval $(call stdlib_builder,LibSSH2_jll,Artifacts Libdl MbedTLS_jll))
$(eval $(call stdlib_builder,MPFR_jll,Artifacts Libdl GMP_jll))
$(eval $(call stdlib_builder,LinearAlgebra,Libdl libblastrampoline_jll OpenBLAS_jll))
$(eval $(call stdlib_builder,Dates,Printf))
$(eval $(call stdlib_builder,Distributed,Random Serialization Sockets))
$(eval $(call stdlib_builder,Future,Random))
$(eval $(call stdlib_builder,UUIDs,Random SHA))
$(eval $(call stdlib_builder,InteractiveUtils,Markdown))

 # 3-depth packages
$(eval $(call stdlib_builder,LibGit2_jll,MbedTLS_jll LibSSH2_jll Artifacts Libdl))
$(eval $(call stdlib_builder,LibCURL_jll,LibSSH2_jll nghttp2_jll MbedTLS_jll Zlib_jll Artifacts Libdl))
$(eval $(call stdlib_builder,REPL,InteractiveUtils Markdown Sockets StyledStrings Unicode))
$(eval $(call stdlib_builder,SharedArrays,Distributed Mmap Random Serialization))
$(eval $(call stdlib_builder,TOML,Dates))
$(eval $(call stdlib_builder,Test,Logging Random Serialization InteractiveUtils))

# 4-depth packages
$(eval $(call stdlib_builder,LibGit2,LibGit2_jll NetworkOptions Printf SHA Base64))
$(eval $(call stdlib_builder,LibCURL,LibCURL_jll MozillaCACerts_jll))

# 5-depth packages
$(eval $(call stdlib_builder,Downloads,ArgTools FileWatching LibCURL NetworkOptions))

# 6-depth packages
$(eval $(call stdlib_builder,Pkg, Artifacts Dates Downloads FileWatching LibGit2 Libdl\
								  Logging Markdown Printf REPL Random SHA Serialization\
								  TOML Tar UUIDs p7zip_jll))

# 7-depth packages
$(eval $(call stdlib_builder,LazyArtifacts,Artifacts Pkg))
$(eval $(call stdlib_builder,REPLExt,Pkg REPL))

$(eval $(call stdlib_builder,SparseArrays,Libdl LinearAlgebra Random Serialization SuiteSparse_jll))
$(eval $(call stdlib_builder,Statistics,LinearAlgebra SparseArrays))
