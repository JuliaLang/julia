## MMTK ##

# Both MMTK_MOVING and MMTK_PLAN should be specified in the Make.user file.
# At this point, since we only support non-moving this is always set to 0
# FIXME: change it to `?:` when introducing moving plans
MMTK_MOVING := 0
MMTK_VARS := MMTK_PLAN=$(MMTK_PLAN) MMTK_MOVING=$(MMTK_MOVING)

$(eval $(call git-external,mmtk_julia,MMTK_JULIA,,,$(BUILDDIR)))
get-mmtk_julia: $(MMTK_JULIA_SRC_FILE)

# Download the binding, build it from source
ifeq (${MMTK_JULIA_DIR},$(BUILDROOT)/usr/lib/mmtk_julia)

MMTK_JULIA_DIR=$(BUILDROOT)/deps/$(BUILDDIR)/$(MMTK_JULIA_SRC_DIR)
MMTK_JULIA_LIB_PATH=$(MMTK_JULIA_DIR)/mmtk/target/$(MMTK_BUILD)
PROJECT_DIRS := JULIA_PATH=$(JULIAHOME) JULIA_BUILDROOT=$(BUILDROOT) MMTK_JULIA_DIR=$(MMTK_JULIA_DIR)

$(BUILDDIR)/$(MMTK_JULIA_SRC_DIR)/build-compiled: $(BUILDROOT)/usr/lib/libmmtk_julia.so
	@echo 1 > $@

# NB: use the absolute dir when creating the symlink
$(BUILDROOT)/usr/lib/libmmtk_julia.so: $(MMTK_JULIA_LIB_PATH)/libmmtk_julia.so
	@ln -sf $(MMTK_JULIA_LIB_PATH)/libmmtk_julia.so $@

$(MMTK_JULIA_LIB_PATH)/libmmtk_julia.so: $(BUILDDIR)/$(MMTK_JULIA_SRC_DIR)/source-extracted
	@$(PROJECT_DIRS) $(MMTK_VARS) $(MAKE) -C $(MMTK_JULIA_DIR) $(MMTK_BUILD)

extract-mmtk_julia: $(BUILDDIR)/$(MMTK_JULIA_SRC_DIR)/source-extracted
configure-mmtk_julia: extract-mmtk_julia
compile-mmtk_julia: $(BUILDROOT)/usr/lib/libmmtk_julia.so
fastcheck-mmtk_julia: #none
check-mmtk_julia: compile-mmtk_julia

$(eval $(call symlink_install,mmtk_julia,$$(MMTK_JULIA_SRC_DIR),$$(BUILDROOT)/usr/lib))

# In this case, there is a custom version of the binding in MMTK_JULIA_DIR
# Build it and symlink libmmtk_julia.so file into $(BUILDROOT)/usr/lib
else

PROJECT_DIRS := JULIA_PATH=$(JULIAHOME) JULIA_BUILDROOT=$(BUILDROOT) MMTK_JULIA_DIR=$(MMTK_JULIA_DIR)
MMTK_JULIA_LIB_PATH=$(MMTK_JULIA_DIR)/mmtk/target/$(MMTK_BUILD)

install-mmtk_julia: compile-mmtk_julia $(build_prefix)/manifest/mmtk_julia

compile-mmtk_julia: $(BUILDROOT)/usr/lib/libmmtk_julia.so

version-check-mmtk_julia: $(MMTK_JULIA_DIR)/mmtk/target/$(MMTK_BUILD)/libmmtk_julia.so

# NB: This will NOT run `cargo build` if there are changes in the Rust source files
# inside the binding repo. However the target below should remake the symlink if there
# are changes in the libmmtk_julia.so from the custom MMTK_JULIA_DIR folder
$(BUILDROOT)/usr/lib/libmmtk_julia.so: $(MMTK_JULIA_DIR)/mmtk/target/$(MMTK_BUILD)/libmmtk_julia.so
	@ln -sf $(MMTK_JULIA_DIR)/mmtk/target/$(MMTK_BUILD)/libmmtk_julia.so $@

$(MMTK_JULIA_DIR)/mmtk/target/$(MMTK_BUILD)/libmmtk_julia.so:
	@$(PROJECT_DIRS) $(MMTK_VARS) $(MAKE) -C $(MMTK_JULIA_DIR) $(MMTK_BUILD)

MMTK_JULIA_VER := mmtk_julia_custom

UNINSTALL_mmtk_julia := $(MMTK_JULIA_VER) manual_mmtk_julia

define manual_mmtk_julia
uninstall-mmtk_julia:
	-rm -f $(build_prefix)/manifest/mmtk_julia
	-rm -f $(BUILDROOT)/usr/lib/libmmtk_julia.so
endef

$(build_prefix)/manifest/mmtk_julia: $(BUILDROOT)/usr/lib/libmmtk_julia.so
	@echo $(UNINSTALL_mmtk_julia) > $@

endif # MMTK_JULIA_DIR
