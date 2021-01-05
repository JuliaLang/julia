default: install

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
SRCCACHE := $(abspath $(SRCDIR)/srccache)
BUILDDIR := .

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/deps/tools/common.mk
include $(JULIAHOME)/deps/tools/stdlib-external.mk

VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

$(build_datarootdir)/julia/stdlib/$(VERSDIR):
	mkdir -p $@

JLLS = DSFMT GMP CURL LIBGIT2 LLVM LIBSSH2 LIBUV MBEDTLS MPFR NGHTTP2 \
       OPENBLAS OPENLIBM P7ZIP PCRE SUITESPARSE ZLIB LLVMUNWIND CSL \
       UNWIND

# Initialize this with JLLs that aren't in deps/Versions.make
JLL_NAMES := MozillaCACerts_jll
get-MozillaCACerts_jll:
install-MozillaCACerts_jll:

# Define rule to download `StdlibArtifacts.toml` files for each JLL we bundle.
define download-artifacts-toml
JLL_NAMES += $$($(1)_JLL_NAME)_jll
$(1)_STDLIB_PATH := $$(JULIAHOME)/stdlib/$$($(1)_JLL_NAME)_jll
$(1)_JLL_VER ?= $$(shell [ -f $$($(1)_STDLIB_PATH)/Project.toml ] && grep "^version" $$($(1)_STDLIB_PATH)/Project.toml | sed -E 's/version[[:space:]]*=[[:space:]]*"?([^"]+)"?/\1/')

$$($(1)_STDLIB_PATH)/StdlibArtifacts.toml:
	$(JLDOWNLOAD) $$@ https://github.com/JuliaBinaryWrappers/$$($(1)_JLL_NAME)_jll.jl/raw/$$($(1)_JLL_NAME)-v$$($(1)_JLL_VER)/Artifacts.toml
get-$$($(1)_JLL_NAME)_jll: $$($(1)_STDLIB_PATH)/StdlibArtifacts.toml
install-$$($(1)_JLL_NAME)_jll: get-$$($(1)_JLL_NAME)_jll
endef
$(foreach jll,$(JLLS),$(eval $(call download-artifacts-toml,$(jll))))


STDLIBS = Artifacts Base64 CRC32c Dates DelimitedFiles Distributed FileWatching \
          Future InteractiveUtils LazyArtifacts Libdl LibGit2 LinearAlgebra Logging \
          Markdown Mmap Printf Profile Random REPL Serialization SHA \
          SharedArrays Sockets SparseArrays SuiteSparse Test TOML Unicode UUIDs \
          $(JLL_NAMES)

STDLIBS_EXT = Pkg Statistics LibCURL Downloads ArgTools Tar NetworkOptions
PKG_GIT_URL := git://github.com/JuliaLang/Pkg.jl.git
PKG_TAR_URL = https://api.github.com/repos/JuliaLang/Pkg.jl/tarball/$1
STATISTICS_GIT_URL := git://github.com/JuliaLang/Statistics.jl.git
STATISTICS_TAR_URL = https://api.github.com/repos/JuliaLang/Statistics.jl/tarball/$1
LIBCURL_GIT_URL := git://github.com/JuliaWeb/LibCURL.jl.git
LIBCURL_TAR_URL = https://api.github.com/repos/JuliaWeb/LibCURL.jl/tarball/$1
DOWNLOADS_GIT_URL := git://github.com/JuliaLang/Downloads.jl.git
DOWNLOADS_TAR_URL = https://api.github.com/repos/JuliaLang/Downloads.jl/tarball/$1
ARGTOOLS_GIT_URL := git://github.com/JuliaIO/ArgTools.jl.git
ARGTOOLS_TAR_URL = https://api.github.com/repos/JuliaIO/ArgTools.jl/tarball/$1
TAR_GIT_URL := git://github.com/JuliaIO/Tar.jl.git
TAR_TAR_URL = https://api.github.com/repos/JuliaIO/Tar.jl/tarball/$1
NETWORKOPTIONS_GIT_URL := git://github.com/JuliaLang/NetworkOptions.jl.git
NETWORKOPTIONS_TAR_URL = https://api.github.com/repos/JuliaLang/NetworkOptions.jl/tarball/$1

$(foreach module, $(STDLIBS_EXT), $(eval $(call stdlib-external,$(module),$(shell echo $(module) | tr a-z A-Z))))

# Generate symlinks to all stdlibs at usr/share/julia/stdlib/vX.Y/
$(foreach module, $(STDLIBS), $(eval $(call symlink_target,$$(JULIAHOME)/stdlib/$(module),$$(build_datarootdir)/julia/stdlib/$$(VERSDIR),$(module))))

STDLIBS_LINK_TARGETS := $(addprefix $(build_datarootdir)/julia/stdlib/$(VERSDIR)/,$(STDLIBS))

getall get: $(addprefix get-, $(STDLIBS_EXT) $(JLL_NAMES))
install: $(addprefix install-, $(STDLIBS_EXT) $(JLL_NAMES)) $(STDLIBS_LINK_TARGETS)
clean: $(addprefix clean-, $(STDLIBS_EXT)) $(CLEAN_TARGETS)
distclean: $(addprefix distclean-, $(STDLIBS_EXT)) clean
checksumall: $(addprefix checksum-, $(STDLIBS_EXT))

DEP_LIBS_STAGED := $(STDLIBS_EXT)
include $(JULIAHOME)/deps/tools/uninstallers.mk
