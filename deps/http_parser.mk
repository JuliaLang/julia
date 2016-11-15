## http_parser

HTTPPARSER_GIT_URL := git://github.com/nodejs/http-parser.git
HTTPPARSER_TAR_URL = https://api.github.com/repos/nodejs/http-parser/tarball/$1
$(eval $(call git-external,http_parser,HTTPPARSER,,,$(BUILDDIR)))

# Manually set the PLATFORM flag to whatever $(OS) is, because at least one
# person (@staticfloat) has the PLATFORM envvar set to something else.
# Also manually set PREFIX to DESTDIR
HTTPPARSER_FLAGS := PLATFORM=$(shell echo $(OS) | tr A-Z a-z) PREFIX=$(build_prefix)

$(eval $(call staged-install, \
	http_parser,$$(HTTPPARSER_SRC_DIR), \
	MAKE_INSTALL,$(HTTPPARSER_FLAGS),, \
	$(INSTALL_NAME_CMD)libhttp_parser.$(SHLIB_EXT) $(build_shlibdir)/libhttp_parser.$(SHLIB_EXT)))

$(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/http_parser-makefile.patch-applied: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/source-extracted
	cd $(BUILDDIR)/$(HTTPPARSER_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/http_parser-makefile.patch
	echo 1 > $@

$(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-compiled: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/source-extracted $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/http_parser-makefile.patch-applied
	$(MAKE) -C $(dir $<) $(HTTPPARSER_FLAGS) library
	echo 1 > $@

$(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-checked: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-compiled
	$(MAKE) -C $(dir $<) $(HTTPPARSER_FLAGS) test
	echo 1 > $@

clean-http_parser:
	-rm -f $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-compiled $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-checked $(build_shlibdir)/libhttp-parser.$(SHLIB_EXT)
	-$(MAKE) -C $(BUILDDIR)/$(HTTPPARSER_SRC_DIR) $(HTTPPARSER_FLAGS) clean


get-http_parser: $(HTTPPARSER_SRC_FILE)
extract-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/source-extracted
configure-http_parser: extract-http_parser
compile-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-compiled
fastcheck-http_parser: check-http_parser
check-http_parser: $(BUILDDIR)/$(HTTPPARSER_SRC_DIR)/build-checked
