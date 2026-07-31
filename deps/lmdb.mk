## LMDB ##
# LMDB is statically linked into libjulia-codegen (see src/objcache.cpp), so we
# always build it from source as a static archive rather than consuming the
# shared library shipped by LMDB_jll.
LMDB_GIT_URL := https://github.com/LMDB/lmdb.git
LMDB_TAR_URL = https://api.github.com/repos/LMDB/lmdb/tarball/$1
$(eval $(call git-external,lmdb,LMDB,,,$(SRCCACHE)))

LMDB_SRC_SUBDIR := libraries/liblmdb
LMDB_BUILD_OPTS := CC="$(CC)" AR="$(AR)" prefix=$(abspath $(build_prefix))
# -fPIC so the static archive can be linked into the libjulia-codegen shared library
LMDB_BUILD_OPTS += XCFLAGS="$(CFLAGS) $(fPIC)"
# Select LMDB's robust lock backend, including SysV semaphores on Apple/BSD.
LMDB_BUILD_OPTS += CPPFLAGS="-DMDB_USE_ROBUST=1"

$(SRCCACHE)/$(LMDB_SRC_DIR)/mingw-types.patch-applied: $(SRCCACHE)/$(LMDB_SRC_DIR)/source-extracted
	cd $(SRCCACHE)/$(LMDB_SRC_DIR) && \
		patch -p1 -f < $(SRCDIR)/patches/0001-Fix-MinGW-Windows-type-errors.patch
	echo 1 > $@

$(SRCCACHE)/$(LMDB_SRC_DIR)/remote-fs.patch-applied: $(SRCCACHE)/$(LMDB_SRC_DIR)/mingw-types.patch-applied
	cd $(SRCCACHE)/$(LMDB_SRC_DIR) && \
		patch -p1 -f < $(SRCDIR)/patches/0002-Refuse-to-open-environments-on-network-filesystems.patch
	echo 1 > $@

$(SRCCACHE)/$(LMDB_SRC_DIR)/pidns.patch-applied: $(SRCCACHE)/$(LMDB_SRC_DIR)/remote-fs.patch-applied
	cd $(SRCCACHE)/$(LMDB_SRC_DIR) && \
		patch -p1 -f < $(SRCDIR)/patches/0003-Refuse-to-open-environments-across-pid-namespaces.patch
	echo 1 > $@

$(SRCCACHE)/$(LMDB_SRC_DIR)/source-patched: $(SRCCACHE)/$(LMDB_SRC_DIR)/pidns.patch-applied
	echo 1 > $@

$(BUILDDIR)/$(LMDB_SRC_DIR)/build-configured: $(SRCCACHE)/$(LMDB_SRC_DIR)/source-patched
	mkdir -p $(dir $@)
	echo 1 > $@

$(BUILDDIR)/$(LMDB_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LMDB_SRC_DIR)/build-configured
	$(MAKE) -C $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR) $(MAKE_COMMON) $(LMDB_BUILD_OPTS) liblmdb.a
	echo 1 > $@

define LMDB_INSTALL
	mkdir -p $2/$$(build_libdir)
	mkdir -p $2/$$(build_includedir)
	cp $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR)/liblmdb.a $2/$$(build_libdir)/
	cp $(SRCCACHE)/$(LMDB_SRC_DIR)/$(LMDB_SRC_SUBDIR)/lmdb.h $2/$$(build_includedir)/
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
