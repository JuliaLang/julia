## LMDB ##
ifneq ($(USE_BINARYBUILDER_LMDB), 1)
LMDB_GIT_URL := https://github.com/LMDB/lmdb.git
LMDB_TAR_URL = https://api.github.com/repos/LMDB/lmdb/tarball/$1
$(eval $(call git-external,lmdb,LMDB,,,$(SRCCACHE)))

LMDB_SRC_SUBDIR := libraries/liblmdb
LMDB_BUILD_OPTS := CC="$(CC)" AR="$(AR)" prefix=$(abspath $(build_prefix))
LMDB_BUILD_OPTS += XCFLAGS="$(CFLAGS) $(fPIC)"

$(BUILDDIR)/$(LMDB_SRC_DIR)/build-configured: $(SRCCACHE)/$(LMDB_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	echo 1 > $@

$(BUILDDIR)/$(LMDB_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LMDB_SRC_DIR)/build-configured
	$(MAKE) -C $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR) $(MAKE_COMMON) $(LMDB_BUILD_OPTS) liblmdb$(SHLIB_EXT)
	echo 1 > $@

define LMDB_INSTALL
	mkdir -p $2/$$(build_shlibdir)
	cp $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR)/liblmdb$(SHLIB_EXT) $2/$$(build_shlibdir)/
	$(INSTALL_NAME_CMD)liblmdb.$(SHLIB_EXT) $2/$$(build_shlibdir)/liblmdb.$(SHLIB_EXT)
endef
$(eval $(call staged-install, \
	lmdb,$(LMDB_SRC_DIR), \
	LMDB_INSTALL,,,))

clean-lmdb:
	-rm -f $(BUILDDIR)/$(LMDB_SRC_DIR)/build-configured $(BUILDDIR)/$(LMDB_SRC_DIR)/build-compiled
	-if [ -d $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR) ]; then $(MAKE) -C $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR) $(MAKE_COMMON) clean; fi

get-lmdb: $(LMDB_SRC_FILE)
extract-lmdb: $(SRCCACHE)/$(LMDB_SRC_DIR)/source-extracted
configure-lmdb: $(BUILDDIR)/$(LMDB_SRC_DIR)/build-configured
compile-lmdb: $(BUILDDIR)/$(LMDB_SRC_DIR)/build-compiled
fastcheck-lmdb: check-lmdb
check-lmdb: compile-lmdb

else # USE_BINARYBUILDER_LMDB

$(eval $(call bb-install,lmdb,LMDB,false))

endif # USE_BINARYBUILDER_LMDB
