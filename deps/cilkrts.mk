CILKRTS_GIT_URL := git://github.com/CilkHub/cilkrts
CILKRTS_TAR_URL = https://api.github.com/repos/CilkHub/cilkrts/tarball/$1
$(eval $(call git-external,cilkrts,CILKRTS,CMakeLists.txt,,$(SRCCACHE)))


CILKRTS_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release
# LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)"
$(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-configured: $(SRCCACHE)/$(CILKRTS_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(CILKRTS_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-compiled: $(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-checked: $(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	cilkrts,$(CILKRTS_SRC_DIR), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),,))

clean-cilkrts:
	-rm $(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-configured \
		$(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(CILKRTS_SRC_DIR) clean

get-cilkrts: $(CILKRTS_SRC_FILE)
extract-cilkrts: $(BUILDDIR)/$(CILKRTS_SRC_DIR)/source-extracted
configure-cilkrts: $(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-configured
compile-cilkrts: $(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-compiled
check-cilkrts: $(BUILDDIR)/$(CILKRTS_SRC_DIR)/build-checked
