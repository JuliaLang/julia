## libgit2

LIBGIT2_GIT_URL := git://github.com/libgit2/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/libgit2/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,CMakeLists.txt,,$(SRCDIR)/srccache))

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

ifeq ($(OS),Linux)
LIBGIT2_OPTS += -DUSE_OPENSSL=OFF -DUSE_MBEDTLS=ON -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

ifeq ($(OS),FreeBSD)
LIBGIT2_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

# We need to bundle ca certs on linux now that we're using libgit2 with ssl
ifeq ($(OS),Linux)
OPENSSL_DIR=$(shell openssl version -d | cut -d '"' -f 2)
# This certfile location observed on Ubuntu 16.04
ifeq ($(shell [ -e $(OPENSSL_DIR)/certs/ca-certificates.crt ] && echo exists),exists)
CERTFILE=$(OPENSSL_DIR)/certs/ca-certificates.crt
# This certfile location observed on openSUSE Leap 42.1
else ifeq ($(shell [ -e $(OPENSSL_DIR)/ca-bundle.pem ] && echo exists),exists)
CERTFILE=$(OPENSSL_DIR)/ca-bundle.pem
# This certfile location observed on Ubuntu 14.04
else ifeq ($(shell [ -e $(OPENSSL_DIR)/cert.pem ] && echo exists),exists)
CERTFILE=$(OPENSSL_DIR)/cert.pem
# This certfile location observed on Debian 7
else ifeq ($(shell [ -e $(OPENSSL_DIR)/certs/ca.pem ] && echo exists),exists)
CERTFILE=$(OPENSSL_DIR)/certs/ca.pem
endif
endif # Linux

LIBGIT2_SRC_PATH := $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)

$(LIBGIT2_SRC_PATH)/libgit2-ssh.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p0 -f < $(SRCDIR)/patches/libgit2-ssh.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-ssh.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-mbedtls.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-agent-nonfatal.patch
	echo 1 > $@

$(LIBGIT2_SRC_PATH)/libgit2-mbedtls-writer-fix.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted | $(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-mbedtls-writer-fix.patch
	echo 1 > $@

$(build_datarootdir)/julia/cert.pem: $(CERTFILE)
	mkdir -p $(build_datarootdir)/julia
	-cp $(CERTFILE) $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: \
	$(LIBGIT2_SRC_PATH)/libgit2-mbedtls.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-ssh.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied \
	$(LIBGIT2_SRC_PATH)/libgit2-mbedtls-writer-fix.patch-applied

ifneq ($(CERTFILE),)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(build_datarootdir)/julia/cert.pem
endif

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
	-rm $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean

get-libgit2: $(LIBGIT2_SRC_FILE)
extract-libgit2: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
compile-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
fastcheck-libgit2: #none
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked
