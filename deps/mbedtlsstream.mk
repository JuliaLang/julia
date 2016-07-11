## mbedtlsstream

MBEDTLSSTREAM_GIT_URL := git://github.com/wildart/mbedtlsstream.git
MBEDTLSSTREAM_TAR_URL = https://api.github.com/repos/wildart/mbedtlsstream/tarball/$1
$(eval $(call git-external,mbedtlsstream,MBEDTLSSTREAM,CMakeLists.txt,build/libmbedtlsstream.$(SHLIB_EXT),$(SRCDIR)/srccache))

MBEDTLSSTREAM_OBJ_TARGET := $(build_shlibdir)/libmbedtlsstream.$(SHLIB_EXT)
MBEDTLSSTREAM_OBJ_SOURCE := $(BUILDDIR)/$(MBEDTLSSTREAM_SRC_DIR)/libmbedtlsstream.$(SHLIB_EXT)

MBEDTLSSTREAM_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_PREFIX_PATH=$(build_prefix) \
		-DCMAKE_INSTALL_RPATH=$(build_prefix) \
		-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE

$(BUILDDIR)/$(MBEDTLSSTREAM_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(MBEDTLSSTREAM_SRC_DIR)/CMakeLists.txt $(HTTPPARSER_OBJ_TARGET) $(LIBGIT2_OBJ_TARGET) $(MBEDTLS_OBJ_TARGET)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(MBEDTLSSTREAM_OPTS)
	touch -c $@

$(MBEDTLSSTREAM_OBJ_SOURCE): $(BUILDDIR)/$(MBEDTLSSTREAM_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<)
	touch -c $@

$(MBEDTLSSTREAM_OBJ_TARGET): $(MBEDTLSSTREAM_OBJ_SOURCE) | $(build_shlibdir)
ifeq ($(BUILD_OS),WINNT)
	cp $< $@
else
	$(call make-install,$(MBEDTLSSTREAM_SRC_DIR),)
endif
	touch -c $@

clean-mbedtlsstream:
	-rm -rf $(BUILDDIR)/$(MBEDTLSSTREAM_SRC_DIR)
	-rm -f $(MBEDTLSSTREAM_OBJ_TARGET)

get-mbedtlsstream: get-mbedtlsstream
configure-mbedtlsstream: $(BUILDDIR)/$(MBEDTLSSTREAM_SRC_DIR)/Makefile
compile-mbedtlsstream: $(MBEDTLSSTREAM_OBJ_SOURCE)
check-mbedtlsstream: check-mbedtlsstream
install-mbedtlsstream: $(MBEDTLSSTREAM_OBJ_TARGET)
