## nvtx ##
include $(SRCDIR)/nvtx.version

NVTX_GIT_URL := https://github.com/NVIDIA/NVTX.git
NVTX_TAR_URL = https://api.github.com/repos/NVIDIA/NVTX/tarball/$1
$(eval $(call git-external,nvtx,NVTX,,,$(SRCCACHE)))

$(BUILDDIR)/$(NVTX_SRC_DIR)/build-configured: $(SRCCACHE)/$(NVTX_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	echo 1 > $@

$(BUILDDIR)/$(NVTX_SRC_DIR)/build-compiled: $(BUILDDIR)/$(NVTX_SRC_DIR)/build-configured
	echo 1 > $@

define NVTX_INSTALL
	cp -a $(SRCCACHE)/$(NVTX_SRC_DIR)/c/include $2/$$(build_includedir)/
endef

$(eval $(call staged-install, \
	nvtx,$(NVTX_SRC_DIR), \
	NVTX_INSTALL,,,))

get-nvtx: $(NVTX_SRC_FILE)
extract-nvtx: $(SRCCACHE)/$(NVTX_SRC_DIR)/source-extracted
configure-nvtx: $(BUILDDIR)/$(NVTX_SRC_DIR)/build-configured
compile-nvtx: $(BUILDDIR)/$(NVTX_SRC_DIR)/build-compiled
fastcheck-nvtx: #none
check-nvtx: #none

clean-nvtx:
	-rm -f $(BUILDDIR)/$(NVTX_SRC_DIR)/build-compiled
