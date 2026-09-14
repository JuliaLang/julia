.PHONY: all clean clean-profiles clean-pgo-profiles clean-bolt-profiles restore-originals delete-originals \
	print-profile-artifacts top

# Abort recipes on the first failed command, including inside loops.
.SHELLFLAGS := -ec

STAGE0_BUILD:=$(CURDIR)/toolchain
STAGE1_BUILD:=$(CURDIR)/pgo-instrumented.build
STAGE2_BUILD:=$(CURDIR)/optimized.build

STAGE0_BINARIES:=$(STAGE0_BUILD)/usr/bin/
STAGE0_TOOLS:=$(STAGE0_BUILD)/usr/tools/

BOLT_PROFILE_DIR:=$(CURDIR)/profiles-bolt
PGO_PROFILE_DIR:=$(CURDIR)/profiles
PGO_PROFILE_FILE:=$(PGO_PROFILE_DIR)/merged.prof
PGO_PROFRAW_FILES:=$(wildcard $(PGO_PROFILE_DIR)/*.profraw)
BOLT_PROFRAW_FILES:=$(wildcard $(BOLT_PROFILE_DIR)/*-prof.[0-9]*.fdata)
JULIA_ROOT:=$(CURDIR)/../..
CONTRIB_DIR_REL:=contrib/$(notdir $(CURDIR))

# Stage 1 only exists to profile the compiler, so it does not need a system
# image for every CPU target of a release build. Stage 2 uses whatever
# JULIA_CPU_TARGET the caller passes.
STAGE1_CPU_TARGET ?= generic

# Ask the regular build system about the platform it would build for, so that
# the staged builds agree with it. BUILD_MACHINE is the target triple recorded
# in the system image (Sys.MACHINE); the stage-0 clang spells it differently
# than the default compiler (e.g. aarch64-apple-darwin vs arm64-apple-darwin),
# and a plain `make binary-dist` of the optimized build would then consider
# build_h.jl stale and rebuild the system image.
# Preserve command-line variables, but omit flags such as -n from the probe.
JULIA_BUILD_VARS:=$(shell MAKEFLAGS='-- $(subst ','"'"',$(MAKEOVERRIDES))' $(MAKE) -s --no-print-directory -C $(JULIA_ROOT) print-OS print-ARCH print-EXE print-BUILD_MACHINE)
HOST_OS:=$(patsubst OS=%,%,$(filter OS=%,$(JULIA_BUILD_VARS)))
HOST_ARCH:=$(patsubst ARCH=%,%,$(filter ARCH=%,$(JULIA_BUILD_VARS)))
EXE:=$(patsubst EXE=%,%,$(filter EXE=%,$(JULIA_BUILD_VARS)))
BUILD_MACHINE:=$(patsubst BUILD_MACHINE=%,%,$(filter BUILD_MACHINE=%,$(JULIA_BUILD_VARS)))

# Which optimizations to apply. PGO and ThinLTO work wherever the stage-0
# toolchain does. BOLT rewrites ELF binaries only, and only for x86-64 and
# AArch64, so it defaults to off everywhere else; set USE_BOLT explicitly to
# override. USE_PGO and USE_LTO are mostly there to bisect build problems.
USE_PGO ?= 1
USE_LTO ?= 1
ifeq ($(HOST_OS),Linux)
ifneq (,$(filter $(HOST_ARCH),x86_64 aarch64))
USE_BOLT ?= 1
endif
endif
USE_BOLT ?= 0

# Settings taken from https://github.com/rust-lang/rust/blob/master/src/tools/opt-dist/src/bolt.rs
BOLT_ARGS :=
# Reorder basic blocks within functions
BOLT_ARGS += -reorder-blocks=ext-tsp
# Reorder functions within the binary
BOLT_ARGS += -reorder-functions=cdsort
# Split function code into hot and cold regions
BOLT_ARGS += -split-functions
# Split as many basic blocks as possible
BOLT_ARGS += -split-all-cold
# Move jump tables to a separate section
BOLT_ARGS += -jump-tables=move
# Use regular size pages for code alignment
BOLT_ARGS += -no-huge-pages
# Fold functions with identical code
BOLT_ARGS += -icf=all
# Split using best available strategy (three-way splitting, Cache-Directed Sort)
# Disabled for libjulia-internal till https://github.com/llvm/llvm-project/issues/89508 is fixed
# BOLT_ARGS += -split-strategy=cdsplit
# Update DWARF debug info in the final binary
BOLT_ARGS += -update-debug-sections
# Print optimization statistics
BOLT_ARGS += -dyno-stats
# BOLT doesn't fully support computed gotos, https://github.com/llvm/llvm-project/issues/89117
# Use escaped regex as the name BOLT recognises is often a bit different, e.g. apply_cl/1(*2)
# This doesn't actually seem to do anything, the actual mitigation is not using --use-old-text
# which we do in the bolt target
BOLT_ARGS += -skip-funcs=.\*apply_cl.\*

# BOLT needs relocations in the libraries it rewrites. Only pass this when BOLT
# is enabled: --emit-relocs is an ELF linker flag.
# -fno-reorder-blocks-and-partition is needed on gcc >= 8.
ifeq ($(USE_BOLT),1)
BOLT_FLAGS := $\
	"BOLT_CFLAGS_GCC+=-fno-reorder-blocks-and-partition" $\
	"BOLT_LDFLAGS=-Wl,--emit-relocs"
else
BOLT_FLAGS :=
endif

LLVM_BOLT:=$(STAGE0_BINARIES)llvm-bolt
LLVM_MERGEFDATA:=$(STAGE0_BINARIES)merge-fdata
LLVM_CXXFILT:=$(STAGE0_TOOLS)llvm-cxxfilt
LLVM_PROFDATA:=$(STAGE0_TOOLS)llvm-profdata
LLVM_OBJCOPY:=$(STAGE0_TOOLS)llvm-objcopy

ifeq ($(USE_BOLT),1)
# Stop stage 2 after the libraries that BOLT rewrites, so that building the
# system image and the stdlib package images afterwards exercises the
# instrumented libraries and produces the BOLT profile.
STAGE2_TARGETS := julia-src-release julia-symlink julia-libccalltest \
				  julia-libccalllazyfoo julia-libccalllazybar julia-libllvmcalltest
# If you add new files to optimize, you need to add BOLT_LDFLAGS and BOLT_CFLAGS to the build of your new file.
# Resolve these symlinks in the recipes, after stage 2 creates them.
SYMLINKS_TO_OPTIMIZE := libLLVM.so libjulia-internal.so libjulia-codegen.so
else
# Without BOLT, stage 2 is the complete build.
STAGE2_TARGETS :=
SYMLINKS_TO_OPTIMIZE :=
endif

AFTER_INSTRUMENT_MESSAGE:='Run `make bolt-train` to build the rest of Julia with the $\
	instrumented libraries. You can then optionally collect more profiling data by running Julia $\
	with an appropriate workload; if you wish, run `make clean-bolt-profiles` before doing so to remove $\
	any profiling data generated by `make bolt-train`. You should end up with some data in $\
	$(BOLT_PROFILE_DIR). Afterwards run `make bolt`.'

# When building a single libLLVM.so we need to increase -vp-counters-per-site
# significantly
COUNTERS_PER_SITE:=6
# Note: profile counters are not atomic by default, https://discourse.llvm.org/t/profile-guided-optimization-pgo-related-questions-and-suggestions/75232/5

ifeq ($(USE_LTO),1)
LTO_FLAGS:=-flto=thin
else
LTO_FLAGS:=
endif

ifeq ($(USE_PGO),1)
PGO_GENERATE_FLAGS:=-fprofile-generate=$(PGO_PROFILE_DIR)
PGO_COUNTER_FLAGS:=-Xclang -mllvm -Xclang -vp-counters-per-site=$(COUNTERS_PER_SITE)
# Configure probes can collide with profiled functions such as main. Suppress
# missing/stale-profile warnings so CMake's -Werror checks test the requested
# option, not the profile's coverage. Otherwise LLVM can silently drop -fPIC
# and -fno-semantic-interposition after a profile hash mismatch.
PGO_USE_FLAGS:=-fprofile-use=$(PGO_PROFILE_FILE)
PGO_WARNING_FLAGS:=-Wno-backend-plugin -Wno-profile-instr-unprofiled $\
	-Wno-profile-instr-out-of-date
else
PGO_GENERATE_FLAGS:=
PGO_COUNTER_FLAGS:=
PGO_USE_FLAGS:=
PGO_WARNING_FLAGS:=
endif

AFTER_STAGE1_MESSAGE:='You can now optionally collect more profiling data for use in PGO by running Julia $\
	with an appropriate workload. If you wish, run `make clean-pgo-profiles` before doing so to remove any profiling data $\
	generated by building Julia. The profiles are written to $(PGO_PROFILE_DIR). $\
	Note that running extensive scripts may result in counter overflows, which can be detected by running $\
	`make top`. Afterwards run `make stage2`.'

# How to drive the toolchain on this platform. Everything else below is
# platform independent.
ifeq ($(HOST_OS),Darwin)
# The stage-0 clang does not come with Xcode, so tell it where the SDK is.
export SDKROOT ?= $(shell xcrun --show-sdk-path)
# Link with Xcode's ld64: the stage-0 ld64.lld cannot always read the text stubs
# of newer SDKs. ld64 has to use the stage-0 libLTO, since Xcode's cannot read
# the stage-0 bitcode, and clang only passes it along when it knows the linker
# version. Select ld64 explicitly for links driven by clang as well as make.
LINKER:=$(shell xcrun --find ld)
LINKER_FLAGS:=--ld-path=$(LINKER) -Wl,-lto_library,$(STAGE0_BUILD)/usr/lib/libLTO.dylib
LINK_ONLY_FLAGS:=
STAGE2_LINK_ONLY_FLAGS:=
# For the same reason, LLVM's static libraries cannot be created with Xcode's
# libtool, which does not index the stage-0 bitcode.
PLATFORM_FLAGS:="LLVM_LIBTOOL=$(STAGE0_TOOLS)llvm-libtool-darwin"
else ifeq ($(HOST_OS),WINNT)
# Use lld's MinGW driver; the ELF-only flags below do not apply to PE/COFF.
LINKER:=$(STAGE0_TOOLS)lld$(EXE)
LINKER_FLAGS:=-fuse-ld=lld
LINK_ONLY_FLAGS:=
STAGE2_LINK_ONLY_FLAGS:=
PLATFORM_FLAGS:=
else
LINKER:=$(STAGE0_TOOLS)ld.lld$(EXE)
LINKER_FLAGS:=-fuse-ld=lld
# Julia's version script names symbols that not every configuration defines.
LINK_ONLY_FLAGS:=-Wl,--undefined-version
STAGE2_LINK_ONLY_FLAGS:=-Wl,--icf=safe
PLATFORM_FLAGS:=
endif

TOOLCHAIN_FLAGS = $\
	"CC=$(STAGE0_TOOLS)clang$(EXE)" $\
	"BUILD_MACHINE=$(BUILD_MACHINE)" $\
	"CXX=$(STAGE0_TOOLS)clang++$(EXE)" $\
	"LD=$(LINKER)" $\
	"AR=$(STAGE0_TOOLS)llvm-ar$(EXE)" $\
	"RANLIB=$(STAGE0_TOOLS)llvm-ranlib$(EXE)" $\
	"CFLAGS+=$(STAGE_CFLAGS)" $\
	"CXXFLAGS+=$(STAGE_CXXFLAGS)" $\
	"LDFLAGS+=$(LINKER_FLAGS) $(LINK_ONLY_FLAGS) $(STAGE_LDFLAGS)" $\
	$(PLATFORM_FLAGS)

# Profile data worth keeping, as globs relative to the Julia source root; CI
# archives these so that a build can be reproduced or a regression bisected.
PROFILE_ARTIFACTS :=
ifeq ($(USE_PGO),1)
PROFILE_ARTIFACTS += $(CONTRIB_DIR_REL)/$(notdir $(PGO_PROFILE_DIR))/$(notdir $(PGO_PROFILE_FILE))
endif
ifeq ($(USE_BOLT),1)
PROFILE_ARTIFACTS += $(CONTRIB_DIR_REL)/$(notdir $(BOLT_PROFILE_DIR))/*.merged.fdata
endif

# The whole flow, in one invocation. Every step below is reachable on its own
# too, e.g. to collect extra profile data between stages. The stages announce
# themselves with a line that buildkite renders as a collapsible log section.
ifeq ($(USE_BOLT),1)
all: bolt
else
all: stage2
endif

$(STAGE0_BUILD) $(STAGE1_BUILD) $(STAGE2_BUILD):
	$(MAKE) -C $(JULIA_ROOT) O=$@ configure

# Stage 0: a toolchain to build with. BinaryBuilder has no BOLT, so that one is
# built from source; everything else is downloaded.
stage0: export USE_BINARYBUILDER_LLVM=1
stage0: | $(STAGE0_BUILD)
	@echo "--- Stage 0: toolchain"
	# The toolchain links against zstd, which macOS does not provide.
	$(MAKE) -C $(STAGE0_BUILD)/deps install-clang install-llvm install-lld install-llvm-tools install-zstd
ifeq ($(USE_BOLT),1)
	$(MAKE) -C $(STAGE0_BUILD)/deps install-BOLT
endif
	# Turn [cd]tors into init/fini_array sections in libclang_rt, since lld
	# doesn't do that, and otherwise the profile constructor is not executed
	find $(STAGE0_BUILD) -name 'libclang_rt.profile-*.a' -exec $(LLVM_OBJCOPY) --rename-section .ctors=.init_array --rename-section .dtors=.fini_array {} +
	touch $@

# Stage 1: an instrumented Julia. Building its system image and package images
# is the workload that the PGO profile is collected from.
stage1: STAGE_CFLAGS:=$(PGO_GENERATE_FLAGS) $(PGO_COUNTER_FLAGS)
stage1: STAGE_CXXFLAGS:=$(PGO_GENERATE_FLAGS) $(PGO_COUNTER_FLAGS)
stage1: STAGE_LDFLAGS:=$(LTO_FLAGS) $(PGO_GENERATE_FLAGS)
stage1: export USE_BINARYBUILDER_LLVM=0
stage1: stage0 | $(STAGE1_BUILD)
	@echo "--- Stage 1: instrumented build"
	# Building the dependencies runs instrumented build tools, e.g. llvm-tblgen,
	# which share code with libLLVM but not Julia's workload. Keep their
	# profiles out of the profile directory.
	LLVM_PROFILE_FILE=$(STAGE1_BUILD)/deps-profiles/%m.profraw $(MAKE) -C $(STAGE1_BUILD) $(TOOLCHAIN_FLAGS) JULIA_CPU_TARGET=$(STAGE1_CPU_TARGET) julia-deps
	$(MAKE) -C $(STAGE1_BUILD) $(TOOLCHAIN_FLAGS) JULIA_CPU_TARGET=$(STAGE1_CPU_TARGET)
	# Remove the empty profile emitted by the instrumented launcher.
	rm -f $(JULIA_ROOT)/base/default.profraw
	touch $@
	@echo $(AFTER_STAGE1_MESSAGE)

# Merge whatever profiles are around. The glob is expanded by the shell, not by
# make, so that this also works in the same invocation that runs stage 1.
$(PGO_PROFILE_FILE): stage1 $(PGO_PROFRAW_FILES)
	$(LLVM_PROFDATA) merge -output=$@ $(PGO_PROFILE_DIR)/*.profraw

# show top 50 functions
top: $(PGO_PROFILE_FILE)
	$(LLVM_PROFDATA) show --topn=50 $< | $(LLVM_CXXFILT)

# Stage 2: the optimized Julia. With BOLT this stops after the libraries that
# get rewritten (see STAGE2_TARGETS); bolt-train builds the rest.
#
# Emit bitcode and ensure configure checks link it with the stage-0 LTO support.
# The linker flags only matter when linking, so compile-only invocations need
# -Qunused-arguments or -Werror'd compile probes fail (e.g. LLVM's -fPIC check,
# which would silently disable PIC and break the libLLVM link).
stage2 bolt-train: STAGE_CFLAGS:=$(PGO_USE_FLAGS) $(PGO_WARNING_FLAGS) $(LTO_FLAGS) $(LINKER_FLAGS) -Qunused-arguments
stage2 bolt-train: STAGE_CXXFLAGS:=$(PGO_USE_FLAGS) $(PGO_WARNING_FLAGS) $(LTO_FLAGS) $(LINKER_FLAGS) -Qunused-arguments
stage2 bolt-train: STAGE_LDFLAGS:=$(LTO_FLAGS) $(PGO_USE_FLAGS) $(STAGE2_LINK_ONLY_FLAGS)
stage2 bolt-train: export USE_BINARYBUILDER_LLVM=0
ifeq ($(USE_PGO),1)
stage2: $(PGO_PROFILE_FILE)
else
stage2: stage0
endif
stage2: | $(STAGE2_BUILD)
	@echo "--- Stage 2: optimized build"
	$(MAKE) -C $(STAGE2_BUILD) $(TOOLCHAIN_FLAGS) $(BOLT_FLAGS) $(STAGE2_TARGETS)
	touch $@

# Keep a copy of the libraries as linked, since both the instrumentation and
# the final rewrite below start from them.
bolt-originals: stage2
	for link in $(SYMLINKS_TO_OPTIMIZE); do \
		file=$$(readlink $(STAGE2_BUILD)/usr/lib/$$link); \
		abs_file=$(STAGE2_BUILD)/usr/lib/$$file; \
		cp -a $$abs_file "$$abs_file.original"; \
	done
	touch $@

# Match the page alignment of the final rewrite. Preserve library mtimes so
# that make does not rebuild their dependents solely due to instrumentation.
bolt-instrument: bolt-originals
	@echo "--- BOLT: instrument"
	mkdir -p $(BOLT_PROFILE_DIR)
	for link in $(SYMLINKS_TO_OPTIMIZE); do \
		file=$$(readlink $(STAGE2_BUILD)/usr/lib/$$link); \
		abs_file=$(STAGE2_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -o $$abs_file --instrument --instrumentation-file-append-pid --instrumentation-file="$(BOLT_PROFILE_DIR)/$$file-prof" -no-huge-pages; \
		touch -r "$$abs_file.original" $$abs_file; \
		printf "\n"; \
	done
	touch $@
	@echo $(AFTER_INSTRUMENT_MESSAGE)

# Build the rest of Julia against the instrumented libraries: the system image
# and the stdlib package images are the BOLT training workload.
bolt-train: bolt-instrument
	@echo "--- BOLT: training build"
	# Keep $(BOLT_FLAGS) here: anything relinked during this stage must carry
	# --emit-relocs, or the later BOLT rewrite of it fails.
	$(MAKE) -C $(STAGE2_BUILD) $(TOOLCHAIN_FLAGS) $(BOLT_FLAGS)
	touch $@

bolt-merge: bolt-train $(BOLT_PROFRAW_FILES)
	for link in $(SYMLINKS_TO_OPTIMIZE); do \
		file=$$(readlink $(STAGE2_BUILD)/usr/lib/$$link); \
		$(LLVM_MERGEFDATA) "$(BOLT_PROFILE_DIR)/$$file-prof."[0-9]*.fdata > "$(BOLT_PROFILE_DIR)/$$file-prof.merged.fdata"; \
	done
	touch $@

# Reuse old text segments to reduce library size, except for libjulia-internal:
# BOLT does not fully support its computed gotos (llvm/llvm-project#89117).
# Preserve mtimes so the rewrite does not trigger dependent builds.
bolt: bolt-merge
	@echo "--- BOLT: rewrite the libraries"
	for link in $(SYMLINKS_TO_OPTIMIZE); do \
		file=$$(readlink $(STAGE2_BUILD)/usr/lib/$$link); \
		abs_file=$(STAGE2_BUILD)/usr/lib/$$file; \
		if [ "$$link" = libjulia-internal.so ]; then extra_args=; else extra_args="--use-old-text -split-strategy=cdsplit"; fi; \
		$(LLVM_BOLT) "$$abs_file.original" -data "$(BOLT_PROFILE_DIR)/$$file-prof.merged.fdata" -o $$abs_file $(BOLT_ARGS) $$extra_args; \
		touch -r "$$abs_file.original" $$abs_file; \
	done
	touch $@

# The pre-BOLT copies are only needed to redo a rewrite; drop them before
# packaging the build, or restore them to undo the rewrite.
restore-originals:
	for link in $(SYMLINKS_TO_OPTIMIZE); do \
		file=$$(readlink $(STAGE2_BUILD)/usr/lib/$$link); \
		abs_file=$(STAGE2_BUILD)/usr/lib/$$file; \
		cp -pP "$$abs_file.original" $$abs_file; \
	done
	rm -f bolt

delete-originals:
	for link in $(SYMLINKS_TO_OPTIMIZE); do \
		file=$$(readlink $(STAGE2_BUILD)/usr/lib/$$link); \
		rm -f "$(STAGE2_BUILD)/usr/lib/$$file.original"; \
	done

print-profile-artifacts:
	@echo '$(PROFILE_ARTIFACTS)'

clean-profiles: clean-pgo-profiles clean-bolt-profiles

clean-pgo-profiles:
	rm -rf $(PGO_PROFILE_DIR)

clean-bolt-profiles:
	rm -rf $(BOLT_PROFILE_DIR)
	mkdir -p $(BOLT_PROFILE_DIR)
	rm -f bolt-merge bolt

# Only the stamps: this makes the next build redo the stages incrementally.
clean:
	rm -f stage0 stage1 stage2 bolt bolt-originals bolt-instrument bolt-train bolt-merge
	rm -f $(PGO_PROFILE_FILE)
