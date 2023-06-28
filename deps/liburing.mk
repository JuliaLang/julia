## liburing ##
include $(SRCDIR)/liburing.version

LIBURING_GIT_URL := https://github.com/axboe/liburing.git
LIBURING_TAR_URL = https://api.github.com/repos/axboe/liburing/tarball/$1
$(eval $(call git-external,liburing,LIBURING,CMakeLists.txt,,$(SRCCACHE)))

LIBURING_CONFIGURE_FLAGS := --prefix=$(build_prefix) --libdir=$(build_libdir)\
							--cc="$(CC)" --cxx="$(CXX)"

# liburing doesn't support out-of-tree
$(BUILDDIR)/$(LIBURING_SRC_DIR)/build-configured: $(SRCCACHE)/$(LIBURING_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cp -r $(dir $<)/* $(dir $@)/
	cd $(dir $@) && \
	./configure $(LIBURING_CONFIGURE_FLAGS)
	echo 1 > $@

$(BUILDDIR)/$(LIBURING_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBURING_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(eval $(call staged-install, \
	liburing,$(LIBURING_SRC_DIR), \
	MAKE_INSTALL,,,))

get-liburing: $(LIBURING_SRC_FILE)
extract-liburing: $(SRCCACHE)/$(LIBURING_SRC_DIR)/source-extracted
configure-liburing: $(BUILDDIR)/$(LIBURING_SRC_DIR)/build-configured
compile-liburing: $(BUILDDIR)/$(LIBURING_SRC_DIR)/build-compiled
fastcheck-liburing: #none
check-liburing: #none

clean-liburing:
	-rm -f $(BUILDDIR)/$(LIBURING_SRC_DIR)/build-compiled
