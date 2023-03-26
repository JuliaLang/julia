SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc

VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

export JULIA_DEPOT_PATH := $(build_prefix)/share/julia

$(JULIA_DEPOT_PATH):
	mkdir -p $@

print-depot-path:
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --startup-file=no -e '@show Base.DEPOT_PATH')

STDLIBS := ArgTools Artifacts Base64 CRC32c FileWatching Libdl NetworkOptions SHA Serialization \
		   GMP_jll LLVMLibUnwind_jll LibUV_jll LibUnwind_jll MbedTLS_jll OpenLibm_jll PCRE2_jll \
		   Zlib_jll dSFMT_jll libLLVM_jll libblastrampoline_jll OpenBLAS_jll Printf Random Tar \
		   LibSSH2_jll MPFR_jll LinearAlgebra Dates Distributed Future LibGit2 Profile SparseArrays UUIDs \
		   SharedArrays TOML Test LibCURL Downloads Pkg Dates LazyArtifacts Sockets Unicode Markdown \
		   InteractiveUtils REPL DelimitedFiles

all-release: $(addprefix cache-release-, $(STDLIBS))
all-debug:   $(addprefix cache-debug-, $(STDLIBS))

define pkgimg_builder
$1_SRCS := $$(shell find $$(build_datarootdir)/julia/stdlib/$$(VERSDIR)/$1/src -name \*.jl) \
    $$(wildcard $$(build_prefix)/manifest/$$(VERSDIR)/$1)
$$(BUILDDIR)/stdlib/$1.release.image: $$($1_SRCS) $$(addsuffix .release.image,$$(addprefix $$(BUILDDIR)/stdlib/,$2))
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no -e 'Base.compilecache(Base.identify_package("$1"))')
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no --check-bounds=yes -e 'Base.compilecache(Base.identify_package("$1"))')
	touch $$@
cache-release-$1: $$(BUILDDIR)/stdlib/$1.release.image
$$(BUILDDIR)/stdlib/$1.debug.image: $$($1_SRCS) $$(addsuffix .debug.image,$$(addprefix $$(BUILDDIR)/stdlib/,$2))
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no -e 'Base.compilecache(Base.identify_package("$1"))')
	@$$(call PRINT_JULIA, $$(call spawn,$$(JULIA_EXECUTABLE)) --startup-file=no --check-bounds=yes -e 'Base.compilecache(Base.identify_package("$1"))')
cache-debug-$1: $$(BUILDDIR)/stdlib/$1.debug.image
.SECONDARY: $$(BUILDDIR)/stdlib/$1.release.image $$(BUILDDIR)/stdlib/$1.debug.image
endef

# Used to just define them in the dependency graph
# reside in the system image
define sysimg_builder
$$(BUILDDIR)/stdlib/$1.release.image:
	touch $$@
cache-release-$1: $$(BUILDDIR)/stdlib/$1.release.image
$$(BUILDDIR)/stdlib/$1.debug.image:
	touch $$@
cache-debug-$1: $$(BUILDDIR)/stdlib/$1.debug.image
.SECONDARY: $$(BUILDDIR)/stdlib/$1.release.image $$(BUILDDIR)/stdlib/$1.debug.image
endef

# no dependencies
$(eval $(call pkgimg_builder,MozillaCACerts_jll,))
$(eval $(call sysimg_builder,ArgTools,))
$(eval $(call sysimg_builder,Artifacts,))
$(eval $(call sysimg_builder,Base64,))
$(eval $(call sysimg_builder,CRC32c,))
$(eval $(call sysimg_builder,FileWatching,))
$(eval $(call sysimg_builder,Libdl,))
$(eval $(call sysimg_builder,Logging,))
$(eval $(call sysimg_builder,Mmap,))
$(eval $(call sysimg_builder,NetworkOptions,))
$(eval $(call sysimg_builder,SHA,))
$(eval $(call sysimg_builder,Serialization,))
$(eval $(call sysimg_builder,Sockets,))
$(eval $(call sysimg_builder,Unicode,))

# 1-depth packages
$(eval $(call pkgimg_builder,GMP_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,LLVMLibUnwind_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,LibUV_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,LibUnwind_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,MbedTLS_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,nghttp2_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,OpenLibm_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,PCRE2_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,Zlib_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,dSFMT_jll,Artifacts Libdl))
$(eval $(call pkgimg_builder,libLLVM_jll,Artifacts Libdl))
$(eval $(call sysimg_builder,libblastrampoline_jll,Artifacts Libdl))
$(eval $(call sysimg_builder,OpenBLAS_jll,Artifacts Libdl))
$(eval $(call sysimg_builder,Markdown,Base64))
$(eval $(call sysimg_builder,Printf,Unicode))
$(eval $(call sysimg_builder,Random,Serialization SHA))
$(eval $(call sysimg_builder,Tar,ArgTools,SHA))
$(eval $(call pkgimg_builder,DelimitedFiles,Mmap))

# 2-depth packages
$(eval $(call pkgimg_builder,LLD_jll,Zlib_jll libLLVM_jll Artifacts Libdl))
$(eval $(call pkgimg_builder,LibSSH2_jll,Artifacts Libdl MbedTLS_jll))
$(eval $(call pkgimg_builder,MPFR_jll,Artifacts Libdl GMP_jll))
$(eval $(call sysimg_builder,LinearAlgebra,Libdl libblastrampoline_jll OpenBLAS_jll))
$(eval $(call sysimg_builder,Dates,Printf))
$(eval $(call sysimg_builder,Distributed,Random Serialization Sockets))
$(eval $(call sysimg_builder,Future,Random))
$(eval $(call sysimg_builder,InteractiveUtils,Markdown))
$(eval $(call sysimg_builder,LibGit2,NetworkOptions Printf SHA Base64))
$(eval $(call sysimg_builder,Profile,Printf))
$(eval $(call sysimg_builder,UUIDs,Random SHA))


 # 3-depth packages
 # LibGit2_jll
$(eval $(call pkgimg_builder,LibCURL_jll,LibSSH2_jll nghttp2_jll MbedTLS_jll Zlib_jll Artifacts Libdl))
$(eval $(call sysimg_builder,REPL,InteractiveUtils Markdown Sockets Unicode))
$(eval $(call sysimg_builder,SharedArrays,Distributed Mmap Random Serialization))
$(eval $(call sysimg_builder,TOML,Dates))
$(eval $(call sysimg_builder,Test,Logging Random Serialization InteractiveUtils))

# 4-depth packages
$(eval $(call sysimg_builder,LibCURL,LibCURL_jll MozillaCACerts_jll))

# 5-depth packages
$(eval $(call sysimg_builder,Downloads,ArgTools FileWatching LibCURL NetworkOptions))

# 6-depth packages
$(eval $(call sysimg_builder,Pkg,Dates LibGit2 Libdl Logging Printf Random SHA UUIDs)) # Markdown REPL

# 7-depth packages
$(eval $(call sysimg_builder,LazyArtifacts,Artifacts Pkg))

$(eval $(call pkgimg_builder,SparseArrays,Libdl LinearAlgebra Random Serialization))
# SuiteSparse_jll
# Statistics
