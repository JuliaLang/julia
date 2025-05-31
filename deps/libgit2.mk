## libgit2
ifneq ($(USE_BINARYBUILDER_LIBGIT2),1)

LIBGIT2_GIT_URL := https://github.com/libgit2/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/libgit2/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,CMakeLists.txt,,$(SRCCACHE)))

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/libssh2
endif

ifeq ($(USE_SYSTEM_OPENSSL), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/openssl
endif

LIBGIT2_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DUSE_THREADS=ON -DUSE_BUNDLED_ZLIB=ON -DUSE_SSH=ON -DBUILD_CLI=OFF
ifeq ($(OS),WINNT)
LIBGIT2_OPTS += -DWIN32=ON -DMINGW=ON
ifeq ($(USE_SYSTEM_LIBSSH2), 0)
LIBGIT2_OPTS += -DLIBSSH2_LIBRARIES=libssh2.dll
LIBGIT2_OPTS += -DLIBSSH2_LIBRARY_DIRS=$(build_prefix)/lib
LIBGIT2_OPTS += -DLIBSSH2_INCLUDE_DIRS=$(build_prefix)/include
endif # USE_SYSTEM_LIBSSH2=0
ifneq ($(ARCH),x86_64)
ifneq ($(USECLANG),1)
LIBGIT2_OPTS += -DCMAKE_C_FLAGS="-mincoming-stack-boundary=2"
endif
endif
ifeq ($(BUILD_OS),WINNT)
LIBGIT2_OPTS += -G"MSYS Makefiles"
else
LIBGIT2_OPTS += -DBUILD_TESTS=OFF -DDLLTOOL=`which $(CROSS_COMPILE)dlltool`
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
endif
ifeq ($(OS),OpenBSD)
# iconv.h is third-party
LIBGIT2_OPTS += -DCMAKE_C_FLAGS="-I/usr/local/include"
endif

ifneq (,$(findstring $(OS),Linux FreeBSD OpenBSD))
LIBGIT2_OPTS += -DUSE_HTTPS="OpenSSL" -DUSE_SHA1="CollisionDetection" -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

# use the bundled distribution of libpcre. we should consider linking against the
# pcre2 library we're building anyway, but this is currently how Yggdrasil does it.
LIBGIT2_OPTS += -DREGEX_BACKEND="builtin"

LIBGIT2_SRC_PATH := $(SRCCACHE)/$(LIBGIT2_SRC_DIR)

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(LIBGIT2_SRC_PATH)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) -G"Unix Makefiles" $(dir $<) $(LIBGIT2_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

define LIBGIT2_INSTALL

ifeq ($$(OS),WINNT)
	mkdir -p $2/$$(build_shlibdir)
	cp $1/libgit2.$$(SHLIB_EXT) $2/$$(build_shlibdir)/libgit2.$$(SHLIB_EXT)
else
	$(call MAKE_INSTALL,$1,$2,$3)
endif
endef
$(eval $(call staged-install, \
	libgit2,$(LIBGIT2_SRC_DIR), \
	LIBGIT2_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libgit2.$$(SHLIB_EXT) $$(build_shlibdir)/libgit2.$$(SHLIB_EXT)))

clean-libgit2:
	-rm -f $(build_datarootdir)/julia/cert.pem
	-rm -f $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean

get-libgit2: $(LIBGIT2_SRC_FILE)
extract-libgit2: $(SRCCACHE)/$(LIBGIT2_SRC_DIR)/source-extracted
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
compile-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
fastcheck-libgit2: #none
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked
$(build_prefix)/manifest/libgit2: $(build_datarootdir)/julia/cert.pem # use libgit2 install status

else # USE_BINARYBUILDER_LIBGIT2

$(eval $(call bb-install,libgit2,LIBGIT2,false))

# BB tarball doesn't create a manifest, so directly depend the `install` target
install-libgit2: $(build_datarootdir)/julia/cert.pem
endif

# Also download and install a cacert.pem file, regardless of whether or not
# we're using BinaryBuilder-sourced binaries
$(SRCCACHE)/cacert-$(MOZILLA_CACERT_VERSION).pem:
	$(JLDOWNLOAD) $@ https://curl.haxx.se/ca/cacert-$(MOZILLA_CACERT_VERSION).pem

$(build_datarootdir)/julia/cert.pem: $(SRCCACHE)/cacert-$(MOZILLA_CACERT_VERSION).pem | $(build_datarootdir)
	$(JLCHECKSUM) $<
	mkdir -p $(build_datarootdir)/julia
	cp $< $@

checksum-mozillacert: $(SRCCACHE)/cacert-$(MOZILLA_CACERT_VERSION).pem
	$(JLCHECKSUM) $<

# When "get"'ing libgit2, download the .pem
get-libgit2: $(SRCCACHE)/cacert-$(MOZILLA_CACERT_VERSION).pem
