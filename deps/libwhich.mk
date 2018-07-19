## LIBWHICH ##
LIBWHICH_GIT_URL := git://github.com/vtjnash/libwhich.git
LIBWHICH_TAR_URL = https://api.github.com/repos/vtjnash/libwhich/tarball/$1
$(eval $(call git-external,libwhich,LIBWHICH,,,$(BUILDDIR)))

LIBWHICH_OBJ_LIB := $(build_depsbindir)/libwhich
LIBWHICH_MFLAGS := CC="$(CC)"

$(BUILDDIR)/$(LIBWHICH_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBWHICH_SRC_DIR)/source-extracted
	$(MAKE) -C $(dir $<) $(LIBWHICH_MFLAGS) libwhich
	echo 1 > $@

$(BUILDDIR)/$(LIBWHICH_SRC_DIR)/build-checked: $(BUILDDIR)/$(LIBWHICH_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(LIBWHICH_MFLAGS) check
endif
	echo 1 > $@

define LIBWHICH_INSTALL
	mkdir -p $2/$$(build_depsbindir)
	cp $1/libwhich $2/$$(build_depsbindir)
endef
$(eval $(call staged-install, \
	libwhich,$(LIBWHICH_SRC_DIR), \
	LIBWHICH_INSTALL,,,))

clean-libwhich:
	-rm $(BUILDDIR)/$(LIBWHICH_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBWHICH_SRC_DIR) clean

get-libwhich: $(LIBWHICH_SRC_FILE)
extract-libwhich: $(BUILDDIR)/$(LIBWHICH_SRC_DIR)/source-extracted
configure-libwhich: extract-libwhich
compile-libwhich: $(BUILDDIR)/$(LIBWHICH_SRC_DIR)/build-compiled
fastcheck-libwhich: check-libwhich
check-libwhich: $(BUILDDIR)/$(LIBWHICH_SRC_DIR)/build-checked
