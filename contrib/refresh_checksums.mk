# This file is a part of Julia. License is MIT: https://julialang.org/license

# Invoke this with no arguments to refresh all tarballs, or with a project name to refresh only that project.
#
# Example:
#   make -f contrib/refresh_checksums.mk gmp

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)

# Default target that will have everything else added to it as a dependency
all:

# Get this list via:
#    using BinaryBuilder
#    print("TRIPLETS=\"$(join(sort(triplet.(BinaryBuilder.supported_platforms(;experimental=true))), " "))\"")
TRIPLETS=aarch64-apple-darwin aarch64-linux-gnu aarch64-linux-musl armv6l-linux-gnueabihf armv6l-linux-musleabihf armv7l-linux-gnueabihf armv7l-linux-musleabihf i686-linux-gnu i686-linux-musl i686-w64-mingw32 powerpc64le-linux-gnu x86_64-apple-darwin x86_64-linux-gnu x86_64-linux-musl x86_64-unknown-freebsd x86_64-w64-mingw32
CLANG_TRIPLETS=$(filter %-darwin %-freebsd,$(TRIPLETS))
NON_CLANG_TRIPLETS=$(filter-out %-darwin %-freebsd,$(TRIPLETS))

# These are the projects currently using BinaryBuilder; both GCC-expanded and non-GCC-expanded:
BB_PROJECTS=mbedtls libssh2 nghttp2 mpfr curl libgit2 pcre libuv unwind osxunwind dsfmt objconv p7zip zlib suitesparse openlibm
BB_GCC_EXPANDED_PROJECTS=openblas csl
BB_CXX_EXPANDED_PROJECTS=gmp llvm clang llvm-tools
# These are non-BB source-only deps
NON_BB_PROJECTS=patchelf mozillacert lapack libwhich utf8proc

# Convert `llvm-tools` to `LLVM_TOOLS`
define makevar
$(shell echo $(1) | tr 'a-z' 'A-Z' | tr '-' '_')
endef

# If $(2) == `src`, this will generate a `USE_BINARYBUILDER_FOO=0` make flag
# It will also generate a `FOO_BB_TRIPLET=$(2)` make flag.
define make_flags
USE_BINARYBUILDER=$(if $(filter src,$(2)),0,1) $(call makevar,$(1))_BB_TRIPLET=$(if $(filter src,$(2)),,$(2)) LLVM_ASSERTIONS=$(if $(filter assert,$(3)),1,0) DEPS_GIT=0
endef

# checksum_bb_dep takes in (name, triplet), and generates a `checksum-$(1)-$(2)` target.
# note that `"src"` is a special triplet value.
# if $(3) is "assert", we set BINARYBUILDER_LLVM_ASSERTS=1
define checksum_dep
checksum-$(1)-$(2)-$(3):
ifeq ($$(VERBOSE),1)
	echo "make $(call make_flags,$(1),$(2),$(3)) checksum-$(1)"
endif
	@-$(MAKE) -C "$(JULIAHOME)/deps" $(call make_flags,$(1),$(2),$(3)) checksum-$(1)

# Add this guy to his project target (e.g. `make -f contrib/refresh_checksums.mk openblas`)
$(1): checksum-$(1)-$(2)-$(3)

# Add this guy to the `all` default target
all: checksum-$(1)-$(2)-$(3)
endef

# Generate targets for source hashes for all our projects
$(foreach project,$(BB_PROJECTS) $(BB_GCC_EXPANDED_PROJECTS) $(BB_CXX_EXPANDED_PROJECTS) $(NON_BB_PROJECTS),$(eval $(call checksum_dep,$(project),src)))

# Generate targets for triplet-specific hashes for all our BB projects
$(foreach project,$(BB_PROJECTS),$(foreach triplet,$(TRIPLETS),$(eval $(call checksum_dep,$(project),$(triplet)))))
$(foreach project,$(BB_GCC_EXPANDED_PROJECTS),$(foreach triplet,$(TRIPLETS),$(foreach libgfortran_version,libgfortran3 libgfortran4 libgfortran5,$(eval $(call checksum_dep,$(project),$(triplet)-$(libgfortran_version))))))

# Because MacOS and FreeBSD use clang, they don't actually use cxxstring_abi expansion:
$(foreach project,$(BB_CXX_EXPANDED_PROJECTS),$(foreach triplet,$(NON_CLANG_TRIPLETS),$(foreach cxxstring_abi,cxx11 cxx03,$(eval $(call checksum_dep,$(project),$(triplet)-$(cxxstring_abi))))))
$(foreach project,$(BB_CXX_EXPANDED_PROJECTS),$(foreach triplet,$(CLANG_TRIPLETS),$(eval $(call checksum_dep,$(project),$(triplet)))))

# Special libLLVM_asserts_jll targets
$(foreach triplet,$(NON_CLANG_TRIPLETS),$(foreach cxxstring_abi,cxx11 cxx03,$(eval $(call checksum_dep,llvm,$(triplet)-$(cxxstring_abi),assert))))
$(foreach triplet,$(CLANG_TRIPLETS),$(eval $(call checksum_dep,llvm,$(triplet),assert)))

# External stdlibs
checksum-stdlibs:
	@-$(MAKE) -C "$(JULIAHOME)/stdlib" checksumall
all: checksum-stdlibs

# doc unicode data
checksum-doc-unicodedata:
	@-$(MAKE) -C "$(JULIAHOME)/doc" checksum-unicodedata
all: checksum-doc-unicodedata

# Special LLVM source hashes for optional targets
checksum-llvm-special-src:
	@-$(MAKE) -C "$(JULIAHOME)/deps" USE_BINARYBUILDER_LLVM=0 DEPS_GIT=0 BUILD_LLDB=1 BUILD_LLVM_CLANG=1 BUILD_CUSTOM_LIBCXX=1 USECLANG=1 checksum-llvm
all: checksum-llvm-special-src

# This file is completely phony
.PHONY: checksum-*

# Debugging helper
print-%:
	@echo '$*=$(subst ','\'',$($*))'
