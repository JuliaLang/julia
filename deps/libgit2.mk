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

LIBGIT2_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DTHREADSAFE=ON
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
LIBGIT2_OPTS += -DCURL_INCLUDE_DIRS=$(build_includedir) -DCURL_LIBRARIES="-L$(build_shlibdir) -lcurl"
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
LIBGIT2_OPTS += -DUSE_HTTPS=ON -DTLS_BACKEND="mbedTLS" -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

LIBGIT2_SRC_PATH := $(SRCCACHE)/$(LIBGIT2_SRC_DIR)

$(LIBGIT2_SRC_PATH)/libgit2-ssh.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p0 -f < $(SRCDIR)/patches/libgit2-ssh.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied: $(SRCCACHE)/$(LIBGIT2_SRC_DIR)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-ssh.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-mbedtls.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-agent-nonfatal.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-mbedtls-verify.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-mbedtls-verify.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-mbedtls-fixup.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-mbedtls-verify.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-mbedtls-fixup.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-ssh-loop.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-mbedtls-fixup.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-ssh-loop.patch
	echo 1 > $@

$(build_datarootdir)/julia/cert.pem:
	$(JLDOWNLOAD) $(shell pwd)/cacert-2018-01-17.pem https://curl.haxx.se/ca/cacert-2018-01-17.pem
	$(JLCHECKSUM) $(shell pwd)/cacert-2018-01-17.pem
	mkdir -p $(build_datarootdir)/julia
	mv $(shell pwd)/cacert-2018-01-17.pem $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: \
	$(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-ssh.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-mbedtls-verify.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-mbedtls-fixup.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-ssh-loop.patch-applied \

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(LIBGIT2_SRC_PATH)/source-extracted $(build_datarootdir)/julia/cert.pem
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
	-rm $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean

get-libgit2: $(LIBGIT2_SRC_FILE) $(build_datarootdir)/julia/cert.pem
extract-libgit2: $(SRCCACHE)/$(LIBGIT2_SRC_DIR)/source-extracted
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
compile-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
fastcheck-libgit2: #none
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked
