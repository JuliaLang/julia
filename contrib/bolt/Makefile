.PHONY: clean clean-profiles restore_originals

STAGE0_BUILD:=$(CURDIR)/toolchain
STAGE1_BUILD:=$(CURDIR)/bolt.build

STAGE0_TOOLS:=$(STAGE0_BUILD)/usr/tools/

PROFILE_DIR:=$(CURDIR)/profiles-bolt
PROFILE_FILE:=$(PROFILE_DIR)/merged.fdata
PROFRAW_FILES:=$(wildcard $(PROFILE_DIR)/*.fdata)
JULIA_ROOT:=$(CURDIR)/../..

LLVM_BOLT:=$(STAGE0_TOOLS)llvm-bolt
LLVM_MERGEFDATA:=$(STAGE0_TOOLS)merge-fdata

# libjulia-codegen.so doesn't produce a profile. Not sure why, but bolt decreases the binary size significantly, so still enabled.
# Probably a bug that it doesn't. TODO: FIXIT
FILES_TO_OPTIMIZE_NO_PROFILE := $(shell readlink $(STAGE1_BUILD)/usr/lib/libjulia-codegen.so)

SYMLINKS_TO_OPTIMIZE := libLLVM.so libjulia-internal.so
FILES_TO_OPTIMIZE := $(shell for file in $(SYMLINKS_TO_OPTIMIZE); do readlink $(STAGE1_BUILD)/usr/lib/$$file; done)
FILES_TO_OPTIMIZE += julia/sys.so

AFTER_INSTRUMENT_MESSAGE:='Run `make clean-profiles` to start with a clean slate. $\
    Then run Julia to collect realistic profile data, for example: `$(STAGE1_BUILD)/julia -e $\
    '\''using Pkg; Pkg.add("LoopVectorization; Pkg.test("LoopVectorization'\''`. This $\
	should produce about 15MB of data in $(PROFILE_DIR). Afterwards run `make merge_data && make bolt`.'

$(STAGE0_BUILD) $(STAGE1_BUILD):
	$(MAKE) -C $(JULIA_ROOT) O=$@ configure

# This is solely to install llvm-bolt and merge-fdata
stage0: export USE_BINARYBUILDER_LLVM=1
stage0: | $(STAGE0_BUILD)
	$(MAKE) -C $(STAGE0_BUILD)/deps install-llvm && \
	touch $@

# Build with --emit-relocs, binary builder doesn't do this so we need to build LLVM for now.
# -fno-reorder-blocks-and-partition is needed on gcc >= 8, remove the flag on clang as unsupported and unnecessary.
# Manually skip package image creation.
$(STAGE1_BUILD): stage0
stage1: export USE_BINARYBUILDER_LLVM=0
stage1: | $(STAGE1_BUILD)
	$(MAKE) -C $(STAGE1_BUILD) julia-release CFLAGS+=-fno-reorder-blocks-and-partition CXXFLAGS+=-fno-reorder-blocks-and-partition LDFLAGS+=-Wl,--emit-relocs && touch $@

move_originals: stage1
	for file in $(FILES_TO_OPTIMIZE) $(FILES_TO_OPTIMIZE_NO_PROFILE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		mv $$abs_file "$$abs_file.original"; \
	done && \
	touch $@

bolt_instrument: move_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -o $$abs_file --instrument --instrumentation-file-append-pid --instrumentation-file="$(PROFILE_DIR)/$$file-prof" --thread-count=4; \
		mkdir -p $$(dirname "$(PROFILE_DIR)/$$file-prof");\
	done && \
	@echo $(AFTER_INSTRUMENT_MESSAGE) && \
	touch $@

pkgimage: stage1
	$(MAKE) -C $(STAGE1_BUILD) stdlibs-cache-release

merge_data: stage1
	for file in $(FILES_TO_OPTIMIZE); do \
		profiles=$(PROFILE_DIR)/$$file-prof.*.fdata; \
		$(LLVM_MERGEFDATA) $$profiles > "$(PROFILE_DIR)/$$file-prof.merged.fdata"; \
	done && \
	touch $@

# Settings taken from https://github.com/rust-lang/rust/blob/master/src/tools/opt-dist/src/bolt.rs
# TODO: Use the new changes in https://github.com/rust-lang/rust/pull/119418/files once we get to LLVM 17/18
bolt: merge_data
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
		--thread-count=4 \
		-dyno-stats; \
    done && \
	for file in $(FILES_TO_OPTIMIZE_NO_PROFILE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
			$(LLVM_BOLT) "$$abs_file.original" -o $$abs_file \
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

clean-pkgimage:
	$(MAKE) -C $(STAGE1_BUILD) -f pkgimage.mk clean

rebuild-pkgimage: | clean-pkgimage pkgimage

install: stage1
	$(MAKE) -C $(STAGE1_BUILD) USE_BINARYBUILDER_LLVM=0 install

clean-profiles:
	rm -rf $(PROFILE_DIR)

clean:
	rm -f stage0 stage1 bolt move_originals merge_data $(PROFILE_FILE)

restore_originals: move_originals
	for file in $(FILES_TO_OPTIMIZE) $(FILES_TO_OPTIMIZE_NO_PROFILE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		cp -P "$$abs_file.original" $$abs_file; \
	done

delete_originals: move_originals
	for file in $(FILES_TO_OPTIMIZE) $(FILES_TO_OPTIMIZE_NO_PROFILE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		# rm "$$abs_file.original"; \
		echo "Would remove $$abs_file.original"; \
	done