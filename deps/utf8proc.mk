## UTF8PROC ##
UTF8PROC_GIT_URL := git://github.com/JuliaLang/utf8proc.git
UTF8PROC_TAR_URL = https://api.github.com/repos/JuliaLang/utf8proc/tarball/$1
$(eval $(call git-external,utf8proc,UTF8PROC,,,$(BUILDDIR)))


ifneq ($(USE_BINARYBUILDER_UTF8PROC),1)

UTF8PROC_OBJ_LIB    := $(build_libdir)/libutf8proc.a
UTF8PROC_OBJ_HEADER := $(build_includedir)/utf8proc.h
UTF8PROC_CFLAGS     := -O2
UTF8PROC_MFLAGS     := CC="$(CC)" CFLAGS="$(CFLAGS) $(UTF8PROC_CFLAGS)" PICFLAG="$(fPIC)" AR="$(AR)"
UTF8PROC_BUILDDIR   := $(BUILDDIR)/$(UTF8PROC_SRC_DIR)

$(UTF8PROC_BUILDDIR)/build-compiled: $(UTF8PROC_BUILDDIR)/source-extracted
	$(MAKE) -C $(dir $<) $(UTF8PROC_MFLAGS) libutf8proc.a
	echo 1 > $@

$(UTF8PROC_BUILDDIR)/build-checked: $(UTF8PROC_BUILDDIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(UTF8PROC_MFLAGS) check
endif
	echo 1 > $@

define UTF8PROC_INSTALL
	mkdir -p $2/$$(build_includedir) $2/$$(build_libdir)
	cp $1/utf8proc.h $2/$$(build_includedir)
	cp $1/libutf8proc.a $2/$$(build_libdir)
	cp $1/libutf8proc.$(shlib_ext) $2/$$(build_shlibdir)
endef
$(eval $(call staged-install, \
	utf8proc,$(UTF8PROC_SRC_DIR), \
	UTF8PROC_INSTALL,,,))

clean-utf8proc:
	-rm $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(UTF8PROC_SRC_DIR) clean

get-utf8proc: $(UTF8PROC_SRC_FILE)
extract-utf8proc: $(UTF8PROC_BUILDDIR)/source-extracted
configure-utf8proc: extract-utf8proc
compile-utf8proc: $(UTF8PROC_BUILDDIR)/build-compiled
# utf8proc tests disabled since they require a download
fastcheck-utf8proc: #check-utf8proc
check-utf8proc: $(UTF8PROC_BUILDDIR)/build-checked

# If we built our own utf8proc, we need to generate a fake utf8proc_jll package to load it in:
$(eval $(call jll-generate,utf8proc_jll,libutf8proc=\"libutf8proc\",,00992c89-a35c-5347-9984-e6609dacc59a,))

else # USE_BINARYBUILDER_UTF8PROC

# Install utf8proc_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,utf8proc_jll))
endif