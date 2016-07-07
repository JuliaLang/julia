## http_parser

HTTPPARSER_GIT_URL := git://github.com/nodejs/http-parser.git
HTTPPARSER_TAR_URL = https://api.github.com/repos/nodejs/http-parser/tarball/$1
$(eval $(call git-external,http_parser,HTTPPARSER,Makefile,libhttp_parser.$(SHLIB_EXT),$(BUILDDIR)))

HTTPPARSER_OBJ_TARGET := $(build_shlibdir)/libhttp_parser.$(SHLIB_EXT)
HTTPPARSER_OBJ_SOURCE := $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/libhttp_parser.$(SHLIB_EXT)

$(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/patched: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/Makefile
	-cd $(BUILDDIR)/$(HTTPPARSER_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/http_parser-win.patch
	echo 1 > $@

$(HTTPPARSER_OBJ_SOURCE): $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/patched
	$(MAKE) -C $(dir $<) library $(MAKE_COMMON)
	touch -c $@

$(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/checked: $(HTTPPARSER_OBJ_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(HTTPPARSER_OBJ_TARGET): $(HTTPPARSER_OBJ_SOURCE)
	$(MAKE) -C $(dir $<) install PREFIX=$(build_prefix)
ifeq ($(OS),WINNT)
	cp $(build_libdir)/libhttp_parser.$(SHLIB_EXT) $(HTTPPARSER_OBJ_TARGET)
endif
	touch -c $(HTTPPARSER_OBJ_TARGET)

clean-http_parser:
	-$(MAKE) -C $(dir $(HTTPPARSER_OBJ_SOURCE)) uninstall PREFIX=$(build_prefix)
	-rm -rf $(HTTPPARSER_OBJ_TARGET)
	-rm -rf $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)

get-http_parser: $(HTTPPARSER_SRC_FILE)
configure-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/patched
compile-http_parser: $(HTTPPARSER_OBJ_SOURCE)
check-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/checked
install-http_parser: $(HTTPPARSER_OBJ_TARGET)
