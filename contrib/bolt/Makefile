.PHONY: clean clean-profiles restore_originals

# Settings taken from https://github.com/rust-lang/rust/blob/master/src/tools/opt-dist/src/bolt.rs
BOLT_ARGS :=
# Reorder basic blocks within functions
BOLT_ARGS += -reorder-blocks=ext-tsp
# Reorder functions within the binary
BOLT_ARGS += -reorder-functions=cdsort
# Split function code into hot and code regions
BOLT_ARGS += -split-functions
# Split as many basic blocks as possible
BOLT_ARGS += -split-all-cold
# Move jump tables to a separate section
BOLT_ARGS += -jump-tables=move
# Use regular size pages for code alignment
BOLT_ARGS += -no-huge-pages
# Fold functions with identical code
BOLT_ARGS += -icf=1
# Split using best available strategy (three-way splitting, Cache-Directed Sort)
BOLT_ARGS += -split-strategy=cdsplit
# The following flag saves about 16 MiB of libLLVM.so size.
# However, it succeeds very non-deterministically.
# Try to reuse old text segments to reduce binary size
BOLT_ARGS += --use-old-text
# Update DWARF debug info in the final binary
BOLT_ARGS += -update-debug-sections
# Print optimization statistics
BOLT_ARGS += -dyno-stats

# -fno-reorder-blocks-and-partition is needed on gcc >= 8.
BOLT_FLAGS := $\
	"BOLT_CXXFLAGS_GCC=-fno-reorder-blocks-and-partition" $\
	"BOLT_LDFLAGS=-Wl,--emit-relocs"

STAGE0_BUILD:=$(CURDIR)/toolchain
STAGE1_BUILD:=$(CURDIR)/bolt.build

STAGE0_BINARIES:=$(STAGE0_BUILD)/usr/bin/

PROFILE_DIR:=$(CURDIR)/profiles-bolt
JULIA_ROOT:=$(CURDIR)/../..

LLVM_BOLT:=$(STAGE0_BINARIES)llvm-bolt
LLVM_MERGEFDATA:=$(STAGE0_BINARIES)merge-fdata

# TODO: work out why libjulia-codegen.so doesn't produce a profile.
# If you add new files to optimize, you need to add BOLT_LDFLAGS and BOLT_CXXFLAGS to the build of your new file.
SYMLINKS_TO_OPTIMIZE := libLLVM.so libjulia-internal.so
FILES_TO_OPTIMIZE := $(shell for file in $(SYMLINKS_TO_OPTIMIZE); do readlink $(STAGE1_BUILD)/usr/lib/$$file; done)

# TODO: Fixup Message
AFTER_INSTRUMENT_MESSAGE:='Run `make clean-profiles` to start with a clean slate. $\
    Then run Julia to collect realistic profile data, for example: `$(STAGE1_BUILD)/julia -e $\
    '\''using Pkg; Pkg.add("LoopVectorization; Pkg.test("LoopVectorization'\''`. This $\
	should produce about 15MB of data in $(PROFILE_DIR). Afterwards run `make merge_data && make bolt`.'

$(STAGE0_BUILD) $(STAGE1_BUILD):
	$(MAKE) -C $(JULIA_ROOT) O=$@ configure

# This is solely to install llvm-bolt and merge-fdata
stage0: | $(STAGE0_BUILD)
	$(MAKE) -C $(STAGE0_BUILD)/deps install-bolt && \
	touch $@

# Build with our custom flags, binary builder doesn't use them so we need to build LLVM for now.
# We manually skip package image creation so that we can profile it
$(STAGE1_BUILD): stage0
stage1: export USE_BINARYBUILDER_LLVM=0
stage1: | $(STAGE1_BUILD)
	$(MAKE) -C $(STAGE1_BUILD) julia-release $(BOLT_FLAGS) && touch $@

copy_originals: stage1
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		cp $$abs_file "$$abs_file.original"; \
	done && \
	touch $@

bolt_instrument: copy_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -o $$abs_file --instrument --instrumentation-file-append-pid --instrumentation-file="$(PROFILE_DIR)/$$file-prof" -no-huge-pages; \
		mkdir -p $$(dirname "$(PROFILE_DIR)/$$file-prof"); \
		printf "\n"; \
	done && \
	touch $@
	@echo $(AFTER_INSTRUMENT_MESSAGE)

pkgimage: stage1
	$(MAKE) -C $(STAGE1_BUILD) stdlibs-cache-release

merge_data: bolt_instrument
	for file in $(FILES_TO_OPTIMIZE); do \
		profiles=$(PROFILE_DIR)/$$file-prof.*.fdata; \
		$(LLVM_MERGEFDATA) $$profiles > "$(PROFILE_DIR)/$$file-prof.merged.fdata"; \
	done && \
	touch $@

bolt: merge_data
	for file in $(FILES_TO_OPTIMIZE); do \
        abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -data "$(PROFILE_DIR)/$$file-prof.merged.fdata" -o $$abs_file $(BOLT_ARGS); \
    done && \
    touch $@

install: stage1
	$(MAKE) -C $(STAGE1_BUILD) USE_BINARYBUILDER_LLVM=0 install

clean-profiles:
	rm -rf $(PROFILE_DIR)

clean:
	rm -f stage0 stage1 bolt copy_originals merge_data bolt_instrument

restore_originals: copy_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		cp -P "$$abs_file.original" $$abs_file; \
	done

delete_originals: copy_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		rm "$$abs_file.original"; \
	done
