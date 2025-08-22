default: install

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
SRCCACHE := $(abspath $(SRCDIR)/srccache)
BUILDDIR := .

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/tools/common.mk
include $(JULIAHOME)/deps/tools/stdlib-external.mk
# include all `*.version` files, import `LIBNAME_JLL_NAME` and `LIBNAME_JLL_VER`
# Note: Some deps do not have a `_jll` pkg: [libwhich, patchelf, utf8proc]
include $(JULIAHOME)/deps/*.version


VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)
DIRS := $(build_datarootdir)/julia/stdlib/$(VERSDIR) $(build_prefix)/manifest/$(VERSDIR)
$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

JLLS = DSFMT GMP CURL LIBGIT2 LLVM LIBSSH2 LIBUV OPENSSL MPFR NGHTTP2 \
       BLASTRAMPOLINE OPENBLAS OPENLIBM P7ZIP PCRE LIBSUITESPARSE ZLIB \
       ZSTD LLVMUNWIND CSL UNWIND LLD

# Initialize this with JLLs that aren't in "deps/$(LibName).version"
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

STDLIBS = Artifacts Base64 CRC32c Dates FileWatching \
          Future InteractiveUtils Libdl LibGit2 Logging \
          Markdown Mmap Printf Profile Random REPL Serialization \
          SharedArrays Sockets Test TOML Unicode UUIDs \
          $(JLL_NAMES)

STDLIBS_EXT = Pkg Statistics LazyArtifacts LibCURL DelimitedFiles Downloads ArgTools \
              Tar NetworkOptions SuiteSparse SparseArrays StyledStrings SHA Distributed \
              JuliaSyntaxHighlighting LinearAlgebra

$(foreach module, $(STDLIBS_EXT), $(eval $(call stdlib-external,$(module),$(shell echo $(module) | tr a-z A-Z))))

ifneq ($(filter $(STDLIBS),$(STDLIBS_EXT)),)
$(error ERROR duplicated STDLIBS in list)
endif

# Generate symlinks to all stdlibs at usr/share/julia/stdlib/vX.Y/
$(foreach module, $(STDLIBS), $(eval $(call symlink_target,$$(JULIAHOME)/stdlib/$(module),$$(build_datarootdir)/julia/stdlib/$$(VERSDIR),$(module))))

STDLIBS_LINK_TARGETS := $(addprefix $(build_datarootdir)/julia/stdlib/$(VERSDIR)/,$(STDLIBS))

getall get: $(addprefix get-, $(STDLIBS_EXT) $(JLL_NAMES))
install: version-check $(addprefix install-, $(STDLIBS_EXT) $(JLL_NAMES)) $(STDLIBS_LINK_TARGETS)
version-check: $(addprefix version-check-, $(STDLIBS_EXT))
uninstall: $(addprefix uninstall-, $(STDLIBS_EXT))
extstdlibclean:
	for module in $(STDLIBS_EXT) ; do \
		rm -rf $(JULIAHOME)/stdlib/$${module}-*; \
	done
clean: $(addprefix clean-, $(STDLIBS_EXT)) $(CLEAN_TARGETS) extstdlibclean
distclean: $(addprefix distclean-, $(STDLIBS_EXT)) clean
checksumall: $(addprefix checksum-, $(STDLIBS_EXT))

DEP_LIBS_STAGED_ALL := $(addprefix $(VERSDIR)/,$(STDLIBS_EXT))
include $(JULIAHOME)/deps/tools/uninstallers.mk
