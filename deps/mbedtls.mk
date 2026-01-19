## mbedtls
include $(SRCDIR)/mbedtls.version

ifneq ($(USE_BINARYBUILDER_MBEDTLS), 1)
MBEDTLS_SRC = mbedtls-$(MBEDTLS_VER)
MBEDTLS_URL = https://github.com/Mbed-TLS/mbedtls/archive/v$(MBEDTLS_VER).tar.gz

MBEDTLS_OPTS := $(CMAKE_COMMON) -DUSE_SHARED_MBEDTLS_LIBRARY=ON \
    -DUSE_STATIC_MBEDTLS_LIBRARY=OFF -DENABLE_PROGRAMS=OFF -DCMAKE_BUILD_TYPE=Release

MBEDTLS_OPTS += -DENABLE_ZLIB_SUPPORT=OFF -DMBEDTLS_FATAL_WARNINGS=OFF
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
	sed -i.org "s|//#define MBEDTLS_MD4_C|#define MBEDTLS_MD4_C|" $(SRCCACHE)/$(MBEDTLS_SRC)/include/mbedtls/config.h
	touch -c $(SRCCACHE)/$(MBEDTLS_SRC)/CMakeLists.txt # old target
	echo 1 > $@

checksum-mbedtls: $(SRCCACHE)/$(MBEDTLS_SRC).tar.gz
	$(JLCHECKSUM) $<

# $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
$(BUILDDIR)/$(MBEDTLS_SRC)/build-configured: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch14.patch-applied
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
	$$(INSTALL_NAME_CHANGE_CMD) libmbedx509.1.dylib @rpath/libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.7.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.7.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedcrypto.$$(SHLIB_EXT)))


clean-mbedtls:
	-rm -f $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured \
		$(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(MBEDTLS_SRC) clean

distclean-mbedtls:
	rm -rf $(SRCCACHE)/$(MBEDTLS_SRC).tar.gz \
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

### Patches
# We carry eleven security patches from Debian (2.16.9)
# The patches are numbered 01 through 13, but we skip numbers 06 and 07

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch01.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/01-CVE-2025-52496.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch02.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch01.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/02-CVE-2025-47917.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch03.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch02.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/03-CVE-2025-47917-test.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch04.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch03.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/04-CVE-2025-47917-2.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch05.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch04.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/05-CVE-2025-47917-2-test.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch08.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch05.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/08-CVE-2025-48965-1.patch
	echo 1 > $@

# We skip numbers 06 and 07

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch09.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch08.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/09-CVE-2025-48965-1-test.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch10.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch09.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/10-CVE-2025-52497-0-1.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch11.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch10.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/11-CVE-2025-52497-0-2.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch12.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch11.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/12-CVE-2025-52497.patch
	echo 1 > $@

$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch13.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch12.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/13-CVE-2025-52497-test.patch
	echo 1 > $@

# Patch 14 isn't a security patch.
# We just need to edit CMakeLists.txt, because otherwise newer Cmakes will error with:
# > Compatibility with CMake < 3.5 has been removed from CMake.
$(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch14.patch-applied: $(SRCCACHE)/$(MBEDTLS_SRC)/mbedtls-patch13.patch-applied
	cd $(SRCCACHE)/$(MBEDTLS_SRC) && \
		patch -p1 -f < $(SRCDIR)/patches/mbedtls/14-CMakeLists.patch

	echo 1 > $@
