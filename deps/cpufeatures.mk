## CPUFEATURES - standalone CPU feature detection library ##
include $(SRCDIR)/cpufeatures.version

CPUFEATURES_SRC_DIR := $(BUILDDIR)/cpufeatures-$(CPUFEATURES_VER)

$(SRCCACHE)/cpufeatures-$(CPUFEATURES_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(CPUFEATURES_TAR_URL)
	touch -c $@

$(CPUFEATURES_SRC_DIR)/source-extracted: $(SRCCACHE)/cpufeatures-$(CPUFEATURES_VER).tar.gz
	rm -rf $(dir $@)
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	echo 1 > $@

checksum-cpufeatures: $(SRCCACHE)/cpufeatures-$(CPUFEATURES_VER).tar.gz
	$(JLCHECKSUM) $<

$(CPUFEATURES_SRC_DIR)/build-compiled: $(CPUFEATURES_SRC_DIR)/source-extracted
	$(MAKE) -C $(CPUFEATURES_SRC_DIR) lib \
		CXX="$(CXX)" \
		CXXFLAGS="$(JCXXFLAGS) -O2" \
		ARCH=$(ARCH)
	echo 1 > $@

define CPUFEATURES_INSTALL
	mkdir -p $2/$$(build_includedir)/cpufeatures
	mkdir -p $2/$$(build_libdir)
	cp $1/include/*.h $2/$$(build_includedir)/cpufeatures/
	cp $1/generated/target_tables_*.h $2/$$(build_includedir)/cpufeatures/
	cp $1/build/libtarget_parsing.a $2/$$(build_libdir)/
endef
$(eval $(call staged-install, \
	cpufeatures,cpufeatures-$(CPUFEATURES_VER), \
	CPUFEATURES_INSTALL,,,,))

clean-cpufeatures:
	-rm -f $(CPUFEATURES_SRC_DIR)/build-compiled

distclean-cpufeatures:
	rm -rf $(SRCCACHE)/cpufeatures*.tar.gz $(CPUFEATURES_SRC_DIR)

get-cpufeatures: $(SRCCACHE)/cpufeatures-$(CPUFEATURES_VER).tar.gz
extract-cpufeatures: $(CPUFEATURES_SRC_DIR)/source-extracted
configure-cpufeatures: extract-cpufeatures
compile-cpufeatures: $(CPUFEATURES_SRC_DIR)/build-compiled
fastcheck-cpufeatures: check-cpufeatures
check-cpufeatures: compile-cpufeatures
