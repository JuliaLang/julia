.PHONY: top clean clean-profiles

STAGE0_BUILD:=$(CURDIR)/toolchain
STAGE1_BUILD:=$(CURDIR)/bolt.build

STAGE0_TOOLS:=$(STAGE0_BUILD)/usr/tools/

PROFILE_DIR:=$(CURDIR)/profiles-bolt
PROFILE_FILE:=$(PROFILE_DIR)/merged.fdata
PROFRAW_FILES:=$(wildcard $(PROFILE_DIR)/*.fdata)
JULIA_ROOT:=$(CURDIR)/../..

LLVM_BOLT:=$(STAGE0_TOOLS)llvm-bolt
LLVM_MERGEFDATA:=$(STAGE0_TOOLS)merge-fdata

FILES_TO_OPTIMIZE := libLLVM.so libjulia-codegen.so libjulia-internal.so julia/sys.so

AFTER_INSTRUMENT_MESSAGE:='Run `make clean-profiles` to start with a clean slate. $\
    Then run Julia to collect realistic profile data, for example: `$(STAGE1_BUILD)/julia -O3 -e $\
    '\''using Pkg; Pkg.add("LoopVectorization; Pkg.test("LoopVectorization'\''`. This $\
	should produce about 15MB of data in $(PROFILE_DIR). Afterwards run `make bolt`.'

$(STAGE0_BUILD) $(STAGE1_BUILD):
	$(MAKE) -C $(JULIA_ROOT) O=$@ configure

# This is solely to install llvm-bolt and merge-fdata
stage0: export USE_BINARYBUILDER_LLVM=1
stage0: | $(STAGE0_BUILD)
	$(MAKE) -C $(STAGE0_BUILD)/deps install-llvm && \
	touch $@

# Build with --emit-relocs, binary builder doesn't do this so we need to build LLVM for now.
# Manually skip package image creation.
$(STAGE1_BUILD): stage0
stage1: export USE_BINARYBUILDER_LLVM=0
stage1: | $(STAGE1_BUILD)
	$(MAKE) -C $(STAGE1_BUILD) julia-release CFLAGS+=-fno-reorder-blocks-and-partition CXXFLAGS+=-fno-reorder-blocks-and-partition LDFLAGS+=-Wl,--emit-relocs && touch $@

move_originals: stage1
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		mv $$abs_file "$$abs_file.original"; \
	done && \
	touch $@

bolt_instrument: move_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -o $$abs_file --instrument --instrumentation-file-append-pid --instrumentation-file="$(PROFILE_DIR)/$$file-prof"; \
	done
	@echo $(AFTER_STAGE1_MESSAGE) && \
	touch $@

packageimage: stage1
	$(MAKE) -C $(STAGE1_BUILD) stdlibs-cache-release

merge_data: stage1
	for file in $(FILES_TO_OPTIMIZE); do \
		profiles=$(PROFILE_DIR)/$$file-prof.*.fdata; \
		$(LLVM_MERGEFDATA) $$profiles > "$(PROFILE_DIR)/$$file-prof.merged.fdata"; \
	done && \
	touch $@

move_instrumented: stage1
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		mv $$abs_file "$$abs_file.instrumented"; \
	done && \
	touch $@

# Settings taken from https://github.com/rust-lang/rust/blob/master/src/tools/opt-dist/src/bolt.rs
# Use https://github.com/rust-lang/rust/pull/119418/files on LLVM 17/18
bolt: merge-data move_instrumented
	for file in $(FILES_TO_OPTIMIZE); do \
        abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
        $(LLVM_BOLT) "$$abs_file.original" -data "$(PROFILE_DIR)/$$file-prof.merged.fdata" -o $$abs_file \
        -reorder-blocks=ext-tsp \
        -reorder-functions=hfsort+ \
        -split-functions \
        -split-all-cold \
        -jump-tables=move \
        -icf=1 \
        --use-old-text \
        -update-debug-sections \
        -dyno-stats; \
    done && \
    touch $@

install: stage1
	$(MAKE) -C $(STAGE1_BUILD) USE_BINARYBUILDER_LLVM=0 install

# $(PROFILE_FILE): stage1 $(PROFRAW_FILES)
# 	$(LLVM_MERGEFDATA) $(PROFRAW_FILES) > $@

clean-profiles:
	rm -rf $(PROFILE_DIR)

clean:
	rm -f stage0 stage1 $(PROFILE_FILE)
