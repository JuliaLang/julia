## libgit2

LIBGIT2_GIT_URL := git://github.com/libgit2/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/libgit2/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,CMakeLists.txt,,$(SRCCACHE)))

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/libssh2
endif

ifeq ($(USE_SYSTEM_MBEDTLS), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/mbedtls
endif

ifneq ($(OS),WINNT)
ifeq ($(USE_SYSTEM_CURL), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/curl
endif
endif

ifneq ($(USE_BINARYBUILDER_LIBGIT2),1)

LIBGIT2_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DTHREADSAFE=ON -DUSE_BUNDLED_ZLIB=ON
ifeq ($(OS),WINNT)
LIBGIT2_OPTS += -DWIN32=ON -DMINGW=ON
ifneq ($(ARCH),x86_64)
ifneq ($(USECLANG),1)
LIBGIT2_OPTS += -DCMAKE_C_FLAGS="-mincoming-stack-boundary=2"
endif
endif
ifeq ($(BUILD_OS),WINNT)
LIBGIT2_OPTS += -G"MSYS Makefiles"
else
LIBGIT2_OPTS += -DBUILD_CLAR=OFF -DDLLTOOL=`which $(CROSS_COMPILE)dlltool`
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
else
LIBGIT2_OPTS += -DCURL_INCLUDE_DIRS=$(build_includedir) -DCURL_LIBRARIES="curl"
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
LIBGIT2_OPTS += -DUSE_HTTPS="mbedTLS" -DSHA1_BACKEND="CollisionDetection" -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

LIBGIT2_SRC_PATH := $(SRCCACHE)/$(LIBGIT2_SRC_DIR)

$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-agent-nonfatal.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-case-sensitive.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-case-sensitive.patch
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: \
	$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-case-sensitive.patch-applied \

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(LIBGIT2_SRC_PATH)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LIBGIT2_OPTS)
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
	-rm $(build_datarootdir)/julia/cert.pem
	-rm $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean

get-libgit2: $(LIBGIT2_SRC_FILE)
extract-libgit2: $(SRCCACHE)/$(LIBGIT2_SRC_DIR)/source-extracted
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
compile-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
fastcheck-libgit2: #none
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked
$(build_prefix)/manifest/libgit2: $(build_datarootdir)/julia/cert.pem # use libgit2 install status

else # USE_BINARYBUILDER_LIBGIT2

LIBGIT2_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/LibGit2_jll.jl/releases/download/LibGit2-v$(LIBGIT2_VER)+$(LIBGIT2_BB_REL)
LIBGIT2_BB_NAME := LibGit2.v$(LIBGIT2_VER)
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

# When "get"'ing libgit2, download the .pem
get-libgit2: $(SRCCACHE)/cacert-$(MOZILLA_CACERT_VERSION).pem

