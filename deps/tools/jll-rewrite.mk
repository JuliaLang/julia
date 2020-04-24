# Define a set of targets for rewriting a JLL package to not depend on `Pkg`;
# or anything outside of `Base`, really.
#
# Parameters to the stdlib-external macro:
#
#   $1 = stdlib_name

define jll-rewrite

# Target name is lowercased prefix, e.g. "MbedTLS_jll" -> "mbedtls"
$(1)_TARGET_NAME := $(or $(3),$(firstword $(subst _, ,$(call lowercase,$(1)))))
# Convert from `MBEDTLS_JLL_SRC_DIR` to `MbedTLS_jll_SRC_DIR` for convenience
$(1)_SRC_DIR := $(BUILDDIR)/$$($(call uppercase,$(1)_SRC_DIR))

# We need to eliminate the JLL package's dependency on `Pkg`; this is because we need to
# load things like `LibGit2_jll`, needed by `LibGit2`, which is itself needed by `Pkg`.
# JLL packages only use Pkg for two things; introspecting the current platform so that
# it can choose which wrapper to load, and downloading artifacts if they're missing.
# We know all the necessary information beforehand, so we modify JLL packages a bit here
# to reduce the amount of work they're doing.  We choose, at compile-time, the platform
# to load, then we `sed` out the `artifact""` call and substitute an equivalent path
# based off of `Sys.STDLIB`.  Who doesn't love some horrific Make/bash/sed spaghetti?
#
# - ARTIFACT_INFO: extracted information from Artifacts.toml; essentially working around
#   the fact that we don't have a Julia `Pkg` around to parse it for us.
# - TRIPLET: the triplet selected from the available options in the JLL
# - WRAPPER: the chosen wrapper `.jl` file within `src/wrappers`, which will get copied
#   to GEN_SRC and used as the overall JLL module.
# - REL_PATH: the julia code that points to the known-good artifact directory
$$($(1)_SRC_DIR)/Artifacts.toml: | extract-$(1)

$$($(1)_SRC_DIR)/jll-rewritten: $$($(1)_SRC_DIR)/Artifacts.toml
	@GEN_SRC="$$($(1)_SRC_DIR)/src/$(1).jl"; \
	ARTIFACT_INFO="$$$$($(PYTHON) $(call python_cygpath,$(JULIAHOME)/contrib/extract_artifact_info.py) $$(call python_cygpath,$$<) $(BB_TRIPLET_LIBGFORTRAN_CXXABI))"; \
	TREEHASH="$$$$(echo $$$${ARTIFACT_INFO} | cut -d ' ' -f1 | xargs)"; \
	TRIPLET="$$$$(echo $$$${ARTIFACT_INFO} | cut -d ' ' -f3 | xargs)"; \
	WRAPPER="$$($(1)_SRC_DIR)/src/wrappers/$$$${TRIPLET}.jl"; \
	REL_PATH="joinpath(dirname(dirname(STDLIB)), \\\"artifacts\\\", \\\"$$$$TREEHASH\\\")"; \
	echo "module $(1)" > "$$$${GEN_SRC}"; \
	echo "using Base.Libc.Libdl" >> "$$$${GEN_SRC}"; \
	echo "using Base.Sys: STDLIB" >> "$$$${GEN_SRC}"; \
	echo "const PATH_list = String[]; const LIBPATH_list = String[];" >> "$$$${GEN_SRC}"; \
	sed -e "s/artifact\\\"$(subst _jll,,$(1))\\\"/$$$${REL_PATH}/" \
	    -e "s/^using \(.*\)/Base.@include_stdlib_jll(\"\1\"); using .\1/" <"$$$${WRAPPER}" >>"$$$${GEN_SRC}"; \
	echo "end # module $(1)" >> "$$$${GEN_SRC}"
	touch $$@

# Add rewrite rule to list of things necessary to satisfy `install-$1`
install-$$($(1)_TARGET_NAME): $$($(1)_SRC_DIR)/jll-rewritten
endef
