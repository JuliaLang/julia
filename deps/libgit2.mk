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

ifeq ($(USE_SYSTEM_CURL), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/curl
endif

ifneq ($(USE_BINARYBUILDER_LIBGIT2),1)

LIBGIT2_CMAKE_LIBPATH = $(build_shlibdir)
LIBGIT2_OPTS = $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DTHREADSAFE=ON "-DCMAKE_LIBRARY_PATH=$(subst $(SPACE),;,$(LIBGIT2_CMAKE_LIBPATH))"
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
LIBGIT2_OPTS += -DBUILD_CLAR=OFF -DDLLTOOL=$(shell which $(CROSS_COMPILE)dlltool)
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
else
LIBGIT2_OPTS += -DCURL_INCLUDE_DIRS=$(LIBCURL_INCDIR) -DCURL_LIBRARIES="curl"
LIBGIT2_CMAKE_LIBPATH += $(LIBCURL_LIBDIR)
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
LIBGIT2_OPTS += -DUSE_HTTPS="mbedTLS" -DSHA1_BACKEND="CollisionDetection" -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
LIBGIT2_OPTS += "-DMBEDTLS_INCLUDE_DIR=$(MBEDTLS_INCDIR)"
LIBGIT2_CMAKE_LIBPATH += $(MBEDTLS_LIBDIR)
endif

LIBGIT2_CMAKE_LIBPATH += $(ZLIB_LIBDIR)
LIBGIT2_CMAKE_LIBPATH += $(LIBSSH2_LIBDIR)

LIBGIT2_SRC_PATH := $(SRCCACHE)/$(LIBGIT2_SRC_DIR)

$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied: $(LIBGIT2_SRC_PATH)/source-extracted
	cd $(LIBGIT2_SRC_PATH) && \
		patch -p1 -f < $(SRCDIR)/patches/libgit2-agent-nonfatal.patch
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: \
	$(LIBGIT2_SRC_PATH)/libgit2-agent-nonfatal.patch-applied \

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

# If we built our own libgit2, we need to generate a fake LibGit2_jll package to load it in:
$(eval $(call jll-generate,LibGit2_jll,libgit2=\"libgit2\",,e37daf67-58a4-590a-8e99-b0245dd2ffc5,\
		                   MbedTLS_jll=c8ffd9c3-330d-5841-b78e-0817d7145fa1 \
						   LibSSH2_jll=29816b5a-b9ab-546f-933c-edad1886dfa8 \
						   LibCURL_jll=deac9b47-8bc7-5906-a0fe-35ac56dc84c0))

else # USE_BINARYBUILDER_LIBGIT2

# Install LibGit2_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,LibGit2_jll))
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
$(build_prefix)/manifest/libgit2: $(build_datarootdir)/julia/cert.pem
