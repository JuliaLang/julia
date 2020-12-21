## mbedtls

ifneq ($(USE_BINARYBUILDER_MBEDTLS), 1)
MBEDTLS_SRC = mbedtls-$(MBEDTLS_VER)
MBEDTLS_URL = https://github.com/ARMmbed/mbedtls/archive/v$(MBEDTLS_VER).tar.gz

MBEDTLS_OPTS := $(CMAKE_COMMON) -DUSE_SHARED_MBEDTLS_LIBRARY=ON \
    -DUSE_STATIC_MBEDTLS_LIBRARY=OFF -DENABLE_PROGRAMS=OFF -DCMAKE_BUILD_TYPE=Release

MBEDTLS_OPTS += -DENABLE_ZLIB_SUPPORT=OFF
ifeq ($(BUILD_OS),WINNT)
MBEDTLS_OPTS += -G"MSYS Makefiles"
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
MBEDTLS_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCCACHE)/$(MBEDTLS_SRC).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(MBEDTLS_URL)

$(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted: $(SRCCACHE)/$(MBEDTLS_SRC).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	# Force-enable MD4
	sed "s|//#define MBEDTLS_MD4_C|#define MBEDTLS_MD4_C|" -i $(SRCCACHE)/$(MBEDTLS_SRC)/include/mbedtls/config.h
	touch -c $(SRCCACHE)/$(MBEDTLS_SRC)/CMakeLists.txt # old target
	echo 1 > $@

checksum-mbedtls: $(SRCCACHE)/$(MBEDTLS_SRC).tar.gz
	$(JLCHECKSUM) $<

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-cmake-findpy.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
	# Apply workaround for CMake 3.18.2 bug (https://github.com/ARMmbed/mbedtls/pull/3691).
	# This patch merged upstream shortly after MBedTLS's 2.25.0 minor release, so chances
	# are it will be included at least in their next minor release (2.26.0?).
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls-cmake-findpy.patch
	echo 1 > @$

$(BUILDDIR)/$(MBEDTLS_SRC)/build-configured: \
	$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-cmake-findpy.patch-applied

$(BUILDDIR)/$(MBEDTLS_SRC)/build-configured: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(MBEDTLS_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled: $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(MBEDTLS_SRC)/build-checked: $(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

ifeq ($(OS),WINNT)
define MBEDTLS_INSTALL
	mkdir -p $2/$$(build_shlibdir)
	cp $1/library/libmbedcrypto.$$(SHLIB_EXT) $2/$$(build_shlibdir)
	cp $1/library/libmbedx509.$$(SHLIB_EXT) $2/$$(build_shlibdir)
	cp $1/library/libmbedtls.$$(SHLIB_EXT) $2/$$(build_shlibdir)
endef
else
define MBEDTLS_INSTALL
	$(call MAKE_INSTALL,$1,$2,)
endef
endif
$(eval $(call staged-install, \
	mbedtls,$(MBEDTLS_SRC), \
	MBEDTLS_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedtls.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedx509.0.dylib @rpath/libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.3.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.3.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedcrypto.$$(SHLIB_EXT)))


clean-mbedtls:
	-rm $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured \
		$(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(MBEDTLS_SRC) clean

distclean-mbedtls:
	-rm -rf $(SRCCACHE)/$(MBEDTLS_SRC).tar.gz \
		$(SRCCACHE)/$(MBEDTLS_SRC) \
		$(BUILDDIR)/$(MBEDTLS_SRC)


get-mbedtls: $(SRCCACHE)/$(MBEDTLS_SRC).tar.gz
extract-mbedtls: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
configure-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured
compile-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
# tests disabled since they are known to fail
fastcheck-mbedtls: #check-mbedtls
check-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC)/build-checked

else # USE_BINARYBUILDER_MBEDTLS

$(eval $(call bb-install,mbedtls,MBEDTLS,false))

endif
