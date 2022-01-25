# This file is a part of Julia. License is MIT: https://julialang.org/license

# Invoke this with no arguments to refresh all tarballs, or with a project name to refresh only that project.
#
# Example:
#   make -f contrib/refresh_checksums.mk gmp

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)

# force a sane / stable configuration
export LC_ALL=C
export LANG=C
.SUFFIXES:

# Default target that will have everything else added to it as a dependency
all: checksum pack-checksum

# Get this list via:
#    using BinaryBuilder
#    print("TRIPLETS=\"$(join(sort(triplet.(BinaryBuilder.supported_platforms(;experimental=true))), " "))\"")
TRIPLETS=aarch64-apple-darwin aarch64-linux-gnu aarch64-linux-musl armv6l-linux-gnueabihf armv6l-linux-musleabihf armv7l-linux-gnueabihf armv7l-linux-musleabihf i686-linux-gnu i686-linux-musl i686-w64-mingw32 powerpc64le-linux-gnu x86_64-apple-darwin x86_64-linux-gnu x86_64-linux-musl x86_64-unknown-freebsd x86_64-w64-mingw32
CLANG_TRIPLETS=$(filter %-darwin %-freebsd,$(TRIPLETS))
NON_CLANG_TRIPLETS=$(filter-out %-darwin %-freebsd,$(TRIPLETS))

# These are the projects currently using BinaryBuilder; both GCC-expanded and non-GCC-expanded:
BB_PROJECTS=mbedtls libssh2 nghttp2 mpfr curl libgit2 pcre libuv unwind llvmunwind dsfmt objconv p7zip zlib libsuitesparse openlibm blastrampoline
BB_GCC_EXPANDED_PROJECTS=openblas csl
BB_CXX_EXPANDED_PROJECTS=gmp llvm clang llvm-tools
# These are non-BB source-only deps
NON_BB_PROJECTS=patchelf mozillacert lapack libwhich utf8proc

ifneq ($(VERBOSE),1)
QUIET_MAKE := -s
else
QUIET_MAKE :=
endif

# Convert `llvm-tools` to `LLVM_TOOLS`
define makevar
$(shell echo $(1) | tr 'a-z-' 'A-Z_')
endef

# If $(2) == `src`, this will generate a `USE_BINARYBUILDER_FOO=0` make flag
# It will also generate a `FOO_BB_TRIPLET=$(2)` make flag.
define make_flags
USE_BINARYBUILDER=$(if $(filter src,$(2)),0,1) $(if $(filter src,$(2)),FC_VERSION=7.0.0,) $(call makevar,$(1))_BB_TRIPLET=$(if $(filter src,$(2)),,$(2)) LLVM_ASSERTIONS=$(if $(filter assert,$(3)),1,0) DEPS_GIT=0
endef

# checksum_bb_dep takes in (name, triplet), and generates a `checksum-$(1)-$(2)` target.
# note that `"src"` is a special triplet value.
# if $(3) is "assert", we set BINARYBUILDER_LLVM_ASSERTS=1
define checksum_dep
checksum-$(1)-$(2)-$(3): clean-$(1)
	-+$(MAKE) $(QUIET_MAKE) -C "$(JULIAHOME)/deps" $(call make_flags,$(1),$(2),$(3)) checksum-$(1)
.PHONY: checksum-$(1)-$(2)-$(3)

# Add this guy to his project target
checksum-$(1): checksum-$(1)-$(2)-$(3)

# Add a dependency to the pack target
# TODO: can we make this so it only adds an ordering but not a dependency?
pack-checksum-$(1): | checksum-$(1)

# Add this guy to the `checksum` and `pack-checksum` default targets (e.g. `make -f contrib/refresh_checksums.mk openblas`)
checksum: checksum-$1
$1 pack-checksum: pack-checksum-$1
endef

# Generate targets for source hashes for all our projects
$(foreach project,$(BB_PROJECTS) $(BB_GCC_EXPANDED_PROJECTS) $(BB_CXX_EXPANDED_PROJECTS) $(NON_BB_PROJECTS),$(eval $(call checksum_dep,$(project),src)))

# Generate targets for triplet-specific hashes for all our BB projects
$(foreach project,$(BB_PROJECTS),$(foreach triplet,$(TRIPLETS),$(eval $(call checksum_dep,$(project),$(triplet)))))
$(foreach project,$(BB_GCC_EXPANDED_PROJECTS),$(foreach triplet,$(TRIPLETS),$(foreach libgfortran_version,libgfortran3 libgfortran4 libgfortran5,$(eval $(call checksum_dep,$(project),$(triplet)-$(libgfortran_version))))))

# Because MacOS and FreeBSD use clang, they don't actually use cxxstring_abi expansion:
$(foreach project,$(BB_CXX_EXPANDED_PROJECTS),$(foreach triplet,$(NON_CLANG_TRIPLETS),$(foreach cxxstring_abi,cxx11 cxx03,$(eval $(call checksum_dep,$(project),$(triplet)-$(cxxstring_abi))))))
$(foreach project,$(BB_CXX_EXPANDED_PROJECTS),$(foreach triplet,$(CLANG_TRIPLETS),$(eval $(call checksum_dep,$(project),$(triplet)))))

# Special libLLVM_asserts_jll/LLVM_assert_jll targets
$(foreach triplet,$(NON_CLANG_TRIPLETS),$(foreach cxxstring_abi,cxx11 cxx03,$(eval $(call checksum_dep,llvm,$(triplet)-$(cxxstring_abi),assert))))
$(foreach triplet,$(NON_CLANG_TRIPLETS),$(foreach cxxstring_abi,cxx11 cxx03,$(eval $(call checksum_dep,llvm-tools,$(triplet)-$(cxxstring_abi),assert))))
$(foreach triplet,$(CLANG_TRIPLETS),$(eval $(call checksum_dep,llvm,$(triplet),assert)))
$(foreach triplet,$(CLANG_TRIPLETS),$(eval $(call checksum_dep,llvm-tools,$(triplet),assert)))

# External stdlibs
checksum-stdlibs:
	-+$(MAKE) $(QUIET_MAKE) -C "$(JULIAHOME)/stdlib" checksumall
all: checksum-stdlibs
.PHONY: checksum-stdlibs

# doc unicode data
checksum-doc-unicodedata:
	-+$(MAKE) $(QUIET_MAKE) -C "$(JULIAHOME)/doc" checksum-unicodedata
all: checksum-doc-unicodedata
.PHONY: checksum-doc-unicodedata

# merge substring project names to avoid races
pack-checksum-llvm-tools: | pack-checksum-llvm
	@# nothing to do but disable the prefix rule
pack-checksum-llvm: | checksum-llvm-tools
pack-checksum-csl: | pack-checksum-compilersupportlibraries
	@# nothing to do but disable the prefix rule
pack-checksum-compilersupportlibraries: | checksum-csl
pack-checksum-libsuitesparse: | pack-checksum-suitesparse
	@# nothing to do but disable the prefix rule
pack-checksum-suitesparse: | checksum-libsuitesparse
# This is a bit tricky: we want llvmunwind to be separate from unwind and llvm,
# so we add a rule to process those first
pack-checksum-llvm pack-checksum-unwind: | pack-checksum-llvmunwind
# and the name for LLVMLibUnwind is awkward, so handle that with a regex
pack-checksum-llvmunwind: | pack-checksum-llvm.*unwind
	cd "$(JULIAHOME)/deps/checksums" && mv 'llvm.*unwind' llvmunwind

clean-%: FORCE
	-rm "$(JULIAHOME)/deps/checksums"/'$*'

# define how to pack parallel checksums into a single file format
pack-checksum-%: FORCE
	@echo making "$(JULIAHOME)/deps/checksums/"'$*'
	@cd "$(JULIAHOME)/deps/checksums" && \
		for each in $$(ls | grep -i '$*'); do \
			if [ -d "$$each" ]; then \
				for type in $$(ls "$$each"); do \
					echo "$$each"/"$$type"/$$(cat "$$each"/"$$type"); \
					rm "$$each"/"$$type"; \
				done; \
				rmdir "$$each"; \
			fi; \
		done >> '$*'
	@cd "$(JULIAHOME)/deps/checksums" && \
		sort '$*' > '$*.tmp' && \
		mv '$*.tmp' '$*'

# This file is completely phony
FORCE:
.PHONY: FORCE

# Debugging helper
print-%:
	@echo '$*=$(subst ','\'',$($*))'
