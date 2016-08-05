## UTF8PROC ##
UTF8PROC_GIT_URL := git://github.com/JuliaLang/utf8proc.git
UTF8PROC_TAR_URL = https://api.github.com/repos/JuliaLang/utf8proc/tarball/$1
$(eval $(call git-external,utf8proc,UTF8PROC,,,$(BUILDDIR)))

UTF8PROC_OBJ_LIB    := $(build_libdir)/libutf8proc.a
UTF8PROC_OBJ_HEADER := $(build_includedir)/utf8proc.h
UTF8PROC_CFLAGS     := -O2
UTF8PROC_MFLAGS     := CC="$(CC) $(DEPS_CFLAGS)" CFLAGS="$(CFLAGS) $(UTF8PROC_CFLAGS)" PICFLAG="$(fPIC)" AR="$(AR)"

$(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/source-extracted
	$(MAKE) -C $(dir $<) $(UTF8PROC_MFLAGS) libutf8proc.a
	echo 1 > $@

$(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-checked: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(UTF8PROC_MFLAGS) check
endif
	echo 1 > $@

$(UTF8PROC_OBJ_LIB): $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled
	cp -f $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/libutf8proc.a $@

$(UTF8PROC_OBJ_HEADER): $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled
	cp -f $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/utf8proc.h $@

$(build_prefix)/manifest/utf8proc : $(UTF8PROC_OBJ_LIB) $(UTF8PROC_OBJ_HEADER) | $(build_prefix)/manifest
	echo $(UTF8PROC_SHA1) > $@

clean-utf8proc:
	-rm -rf $(build_prefix)/manifest/utf8proc $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled
	-rm -rf $(build_libdir)/libutf8proc.a $(build_includedir)/utf8proc.h
	-$(MAKE) -C $(BUILDDIR)/$(UTF8PROC_SRC_DIR) clean

get-utf8proc: $(UTF8PROC_SRC_FILE)
extract-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/source-extracted
configure-utf8proc: extract-utf8proc
compile-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled
check-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-checked
install-utf8proc: $(build_prefix)/manifest/utf8proc
