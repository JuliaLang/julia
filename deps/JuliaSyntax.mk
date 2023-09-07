$(eval $(call git-external,JuliaSyntax,JULIASYNTAX,,,$(BUILDDIR)))

$(BUILDDIR)/$(JULIASYNTAX_SRC_DIR)/build-compiled: $(BUILDDIR)/$(JULIASYNTAX_SRC_DIR)/source-extracted
	@# no build steps
	echo 1 > $@

$(eval $(call symlink_install,JuliaSyntax,$$(JULIASYNTAX_SRC_DIR),$$(BUILDROOT)/base))

clean-JuliaSyntax:
	-rm -f $(BUILDDIR)/$(JULIASYNTAX_SRC_DIR)/build-compiled
get-JuliaSyntax: $(JULIASYNTAX_SRC_FILE)
extract-JuliaSyntax: $(BUILDDIR)/$(JULIASYNTAX_SRC_DIR)/source-extracted
configure-JuliaSyntax: extract-JuliaSyntax
compile-JuliaSyntax: $(BUILDDIR)/$(JULIASYNTAX_SRC_DIR)/build-compiled
fastcheck-JuliaSyntax: check-JuliaSyntax
check-JuliaSyntax: compile-JuliaSyntax
