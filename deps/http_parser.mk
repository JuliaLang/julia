## http_parser

HTTPPARSER_GIT_URL := git://github.com/nodejs/http-parser.git
HTTPPARSER_TAR_URL = https://api.github.com/repos/nodejs/http-parser/tarball/$1
$(eval $(call git-external,http_parser,HTTPPARSER,Makefile,libhttpparser.$(SHLIB_EXT),$(BUILDDIR)))

HTTPPARSER_OBJ_TARGET := $(build_shlibdir)/libhttp_parser.$(SHLIB_EXT)
HTTPPARSER_OBJ_SOURCE := $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/libhttp_parser.$(SHLIB_EXT)

$(HTTPPARSER_OBJ_SOURCE): $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) library $(MAKE_COMMON)
	touch -c $@

$(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/checked: $(HTTPPARSER_OBJ_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(HTTPPARSER_OBJ_TARGET): $(HTTPPARSER_OBJ_SOURCE)
	$(call make-install,$(HTTPPARSER_SRC_DIR), library PREFIX=$(build_staging)/$(HTTPPARSER_SRC_DIR)$(build_prefix))
	touch -c $(HTTPPARSER_OBJ_TARGET)

clean-http_parser:
	-rm -rf $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)
	-rm -f $(HTTPPARSER_OBJ_TARGET)

get-http_parser: $(HTTPPARSER_SRC_FILE)
configure-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/Makefile
compile-http_parser: $(HTTPPARSER_OBJ_SOURCE)
check-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/checked
install-http_parser: $(HTTPPARSER_OBJ_TARGET)
