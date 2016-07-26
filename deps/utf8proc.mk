## UTF8PROC ##
UTF8PROC_GIT_URL := git://github.com/JuliaLang/utf8proc.git
UTF8PROC_TAR_URL = https://api.github.com/repos/JuliaLang/utf8proc/tarball/$1
$(eval $(call git-external,utf8proc,UTF8PROC,Makefile,libutf8proc.a,$(BUILDDIR)))

UTF8PROC_SRC_TARGET := $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/libutf8proc.a
UTF8PROC_OBJ_LIB    := $(build_libdir)/libutf8proc.a
UTF8PROC_OBJ_HEADER := $(build_includedir)/utf8proc.h
UTF8PROC_OBJ_TARGET := $(UTF8PROC_OBJ_LIB) $(UTF8PROC_OBJ_HEADER)
UTF8PROC_CFLAGS     := -O2
UTF8PROC_MFLAGS     := CC="$(CC) $(DEPS_CFLAGS)" CFLAGS="$(CFLAGS) $(UTF8PROC_CFLAGS)" PICFLAG="$(fPIC)" AR="$(AR)"

$(UTF8PROC_SRC_TARGET): $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) $(UTF8PROC_MFLAGS) libutf8proc.a
	touch -c $@
$(BUILDDIR)/$(UTF8PROC_SRC_DIR)/checked: $(UTF8PROC_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(UTF8PROC_MFLAGS) check
endif
	echo 1 > $@
$(UTF8PROC_OBJ_LIB): $(UTF8PROC_SRC_TARGET)
	cp -f $< $@
$(UTF8PROC_OBJ_HEADER): $(UTF8PROC_SRC_TARGET)
	cp -f $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/utf8proc.h $@

clean-utf8proc:
	-$(MAKE) -C $(BUILDDIR)/$(UTF8PROC_SRC_DIR) clean
	-rm -rf $(build_libdir)/libutf8proc.a $(build_includedir)/utf8proc.h

get-utf8proc: $(UTF8PROC_SRC_FILE)
configure-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/Makefile
compile-utf8proc: $(UTF8PROC_SRC_TARGET)
check-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/checked
install-utf8proc: $(UTF8PROC_OBJ_TARGET)
