STDLIBS_WITHIN_SYSIMG := \
	Artifacts FileWatching Libdl SHA libblastrampoline_jll OpenBLAS_jll Random \
	LinearAlgebra Sockets

INDEPENDENT_STDLIBS := \
	ArgTools Base64 CRC32c Dates DelimitedFiles Distributed Downloads Future \
	InteractiveUtils JuliaSyntaxHighlighting LazyArtifacts LibGit2 LibCURL Logging \
	Markdown Mmap NetworkOptions Profile Printf Pkg REPL Serialization SharedArrays \
	SparseArrays Statistics StyledStrings SuiteSparse_jll Tar Test TOML Unicode UUIDs \
	dSFMT_jll GMP_jll libLLVM_jll LLD_jll LLVMLibUnwind_jll LibUnwind_jll LibUV_jll \
	LibCURL_jll LibSSH2_jll LibGit2_jll nghttp2_jll  MozillaCACerts_jll MbedTLS_jll \
	MPFR_jll OpenLibm_jll PCRE2_jll p7zip_jll Zlib_jll

PKG_EXTS := \
	REPLExt

STDLIBS := $(STDLIBS_WITHIN_SYSIMG) $(INDEPENDENT_STDLIBS) $(PKG_EXTS)
VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

SYSIMG_STDLIB_SRCS =
define STDLIB_srcs
ifneq ($(filter $(1),$(PKG_EXTS)),)
	$1_SRCS := $$(shell find $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/Pkg/ext/$1 -name \*.jl) \
	$$(wildcard $$(build_prefix)/manifest/$$(VERSDIR)/Pkg) $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/Pkg/Project.toml
else
	$1_SRCS := $$(shell find $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/$1/src -name \*.jl) \
	$$(wildcard $$(build_prefix)/manifest/$$(VERSDIR)/$1) $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/$1/Project.toml
endif

ifneq ($(filter $(1),$(STDLIBS_WITHIN_SYSIMG)),)
	SYSIMG_STDLIB_SRCS += $$($1_SRCS)
endif
endef

$(foreach stdlib,$(STDLIBS),$(eval $(call STDLIB_srcs,$(stdlib))))
