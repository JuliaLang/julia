STDLIBS_WITHIN_SYSIMG := \
	ArgTools Artifacts Base64 CRC32c FileWatching Libdl NetworkOptions SHA Serialization \
	MbedTLS_jll libblastrampoline_jll OpenBLAS_jll Printf Random Tar LibSSH2_jll LibGit2_jll \
	LinearAlgebra Dates Future LibGit2 UUIDs TOML LibCURL Downloads Dates Logging \
	Sockets Unicode Markdown InteractiveUtils REPL nghttp2_jll LibCURL_jll MozillaCACerts_jll \
	Mmap

INDEPENDENT_STDLIBS := \
	GMP_jll LLVMLibUnwind_jll LibUV_jll LibUnwind_jll OpenLibm_jll PCRE2_jll \
	Zlib_jll dSFMT_jll libLLVM_jll LLD_jll MPFR_jll \
	DelimitedFiles Distributed SharedArrays SparseArrays Statistics Test LazyArtifacts \
	Profile Pkg


STDLIBS := $(STDLIBS_WITHIN_SYSIMG) $(INDEPENDENT_STDLIBS)
VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

SYSIMG_STDLIB_SRCS =
define STDLIB_srcs
$1_SRCS := $$(shell find $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/$1/src -name \*.jl) \
	$$(wildcard $$(build_prefix)/manifest/$$(VERSDIR)/$1) $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/$1/Project.toml
ifneq ($(filter $(1),$(STDLIBS_WITHIN_SYSIMG)),)
	SYSIMG_STDLIB_SRCS += $$($1_SRCS)
endif
endef

$(foreach stdlib,$(STDLIBS),$(eval $(call STDLIB_srcs,$(stdlib))))
