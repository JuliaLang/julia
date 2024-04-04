.PHONY: clean clean-profiles restore_originals

BOLTCXXFLAGS_GCC := -fno-reorder-blocks-and-partition
BOLTLDFLAGS := -Wl,--emit-relocs
BOLTFLAGS := $\
	"BOLTCXXFLAGS=$(BOLTCXXFLAGS_GCC)" $\
	"BOLTLDFLAGS=$(BOLTLDFLAGS)"

STAGE0_BUILD:=$(CURDIR)/toolchain
STAGE1_BUILD:=$(CURDIR)/bolt.build

STAGE0_TOOLS:=$(STAGE0_BUILD)/usr/tools/

PROFILE_DIR:=$(CURDIR)/profiles-bolt
PROFRAW_FILES:=$(wildcard $(PROFILE_DIR)/*.fdata)
JULIA_ROOT:=$(CURDIR)/../..

LLVM_BOLT:=$(STAGE0_TOOLS)llvm-bolt
LLVM_MERGEFDATA:=$(STAGE0_TOOLS)merge-fdata

# TODO: work out why libjulia-codegen.so doesn't produce a profile.
SYMLINKS_TO_OPTIMIZE := libLLVM.so libjulia-internal.so # If you update this, you need to add BOLTLDFLAGS and BOLTCXXFLAGS to the build of your new file.
FILES_TO_OPTIMIZE := $(shell for file in $(SYMLINKS_TO_OPTIMIZE); do readlink $(STAGE1_BUILD)/usr/lib/$$file; done)

# TODO: Fixup Message
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
# -fno-reorder-blocks-and-partition is needed on gcc >= 8.
# Manually skip package image creation so that we can profile building it
$(STAGE1_BUILD): stage0
stage1: export USE_BINARYBUILDER_LLVM=0
stage1: | $(STAGE1_BUILD)
	$(MAKE) -C $(STAGE1_BUILD) julia-release $(BOLTFLAGS) && touch $@

copy_originals: stage1
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		cp $$abs_file "$$abs_file.original"; \
	done && \
	touch $@

bolt_instrument: copy_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -o $$abs_file --instrument --instrumentation-file-append-pid --instrumentation-file="$(PROFILE_DIR)/$$file-prof" --thread-count=4 -no-huge-pages; \
		mkdir -p $$(dirname "$(PROFILE_DIR)/$$file-prof"); \
		printf "\n"; \
	done && \
	touch $@
	@echo $(AFTER_INSTRUMENT_MESSAGE)

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
bolt: copy_originals
	for file in $(FILES_TO_OPTIMIZE); do \
        abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		$(LLVM_BOLT) "$$abs_file.original" -data "$(PROFILE_DIR)/$$file-prof.merged.fdata" -o $$abs_file \
		-reorder-blocks=ext-tsp \
		-reorder-functions=hfsort+ \
		-split-functions \
		-split-all-cold \
		-jump-tables=move \
		-no-huge-pages \
		-icf=1 \
		--use-old-text \
		-update-debug-sections \
		--thread-count=4 \
		-dyno-stats; \
    done && \
    touch $@

# clean-pkgimage:
# 	$(MAKE) -C $(STAGE1_BUILD) -f pkgimage.mk clean

# rebuild-pkgimage: | clean-pkgimage pkgimage

# # TODO: Enable only for sys.so
# rebuild-sysimg:
# 	# Binary sizes don't decrease for anythign but sys.so. I don't see why performance would increase for the others but haven't checked.
# 	# rm $(STAGE1_BUILD)/usr/lib/julia/sys.so $(STAGE1_BUILD)/usr/lib/julia/sys-o.a $(STAGE1_BUILD)/usr/lib/julia/sys.ji $(STAGE1_BUILD)/usr/lib/julia/corecompiler.ji
# 	$(MAKE) -C $(STAGE1_BUILD) julia-sysimg-release

install: stage1
	$(MAKE) -C $(STAGE1_BUILD) USE_BINARYBUILDER_LLVM=0 install

clean-profiles:
	rm -rf $(PROFILE_DIR)

clean:
	rm -f stage0 stage1 bolt move_originals merge_data

restore_originals: move_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		cp -P "$$abs_file.original" $$abs_file; \
	done

delete_originals: move_originals
	for file in $(FILES_TO_OPTIMIZE); do \
		abs_file=$(STAGE1_BUILD)/usr/lib/$$file; \
		# rm "$$abs_file.original"; \
		echo "Would remove $$abs_file.original"; \
	done