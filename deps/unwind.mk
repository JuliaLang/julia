## UNWIND ##

ifneq ($(USE_BINARYBUILDER_LIBUNWIND),1)
LIBUNWIND_CFLAGS := -U_FORTIFY_SOURCE $(fPIC) -lz
LIBUNWIND_CPPFLAGS :=

ifeq ($(USE_SYSTEM_ZLIB),0)
$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured: | $(build_prefix)/manifest/zlib
endif

ifeq ($(USE_SYSTEM_LLVM),0)
$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured: | $(build_prefix)/manifest/llvm
endif

$(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/libunwind/libunwind/releases/download/v$(UNWIND_VER_TAG)/libunwind-$(UNWIND_VER).tar.gz

$(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted: $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xfz $<
	touch -c $(SRCCACHE)/libunwind-$(UNWIND_VER)/configure # old target
	echo 1 > $@

checksum-unwind: $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-prefer-extbl.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f < $(SRCDIR)/patches/libunwind-prefer-extbl.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-static-arm.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-prefer-extbl.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f < $(SRCDIR)/patches/libunwind-static-arm.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-cfa-rsp.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-static-arm.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f -u < $(SRCDIR)/patches/libunwind-cfa-rsp.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-dwarf-table.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-cfa-rsp.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f -u -l < $(SRCDIR)/patches/libunwind-dwarf-table.patch
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-dwarf-table.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) CPPFLAGS="$(CPPFLAGS) $(LIBUNWIND_CPPFLAGS)" CFLAGS="$(CFLAGS) $(LIBUNWIND_CFLAGS)" --enable-shared --disable-minidebuginfo --disable-tests --enable-zlibdebuginfo
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-checked: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	unwind,libunwind-$(UNWIND_VER), \
	MAKE_INSTALL,,,))

clean-unwind:
	-rm -f $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/libunwind-$(UNWIND_VER) clean

distclean-unwind:
	rm -rf $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz \
		$(SRCCACHE)/libunwind-$(UNWIND_VER) \
		$(BUILDDIR)/libunwind-$(UNWIND_VER)

get-unwind: $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz
extract-unwind: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted
configure-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured
compile-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
#todo: libunwind tests are known to fail, so they aren't run
fastcheck-unwind: #none
check-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-checked


## LLVM libunwind ##

LLVMUNWIND_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=MinSizeRel -DLIBUNWIND_ENABLE_PEDANTIC=OFF -DLLVM_CONFIG_PATH=$(build_depsbindir)/llvm-config

$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER).tar.xz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/llvm/llvm-project/releases/download/llvmorg-$(LLVMUNWIND_VER)/libunwind-$(LLVMUNWIND_VER).src.tar.xz

$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/source-extracted: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER).tar.xz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xf $<
	mv $(SRCCACHE)/libunwind-$(LLVMUNWIND_VER).src $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)
	echo 1 > $@

$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-prologue-epilogue.patch-applied: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/source-extracted
	cd $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER) && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-prologue-epilogue.patch
	echo 1 > $@

$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-force-dwarf.patch-applied: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-prologue-epilogue.patch-applied
	cd $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER) && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-force-dwarf.patch
	echo 1 > $@

$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-revert-monorepo-requirement.patch-applied: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-force-dwarf.patch-applied
	cd $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER) && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-revert-monorepo-requirement.patch
	echo 1 > $@

$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-freebsd-libgcc-api-compat.patch-applied: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-revert-monorepo-requirement.patch-applied
	cd $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER) && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-freebsd-libgcc-api-compat.patch
	echo 1 > $@

checksum-llvmunwind: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER).tar.xz
	$(JLCHECKSUM) $<

$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/source-extracted $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/llvm-libunwind-freebsd-libgcc-api-compat.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LLVMUNWIND_OPTS)
	echo 1 > $@

$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-compiled: $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(eval $(call staged-install, \
	llvmunwind,llvmunwind-$(LLVMUNWIND_VER), \
	MAKE_INSTALL,,, \
	cp -fR $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/include/* $(build_includedir)))

clean-llvmunwind:
	-rm -f $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-compiled
	rm -rf $(build_includedir)/mach-o/ $(build_includedir)/unwind.h $(build_includedir)/libunwind.h
	-$(MAKE) -C $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER) clean

distclean-llvmunwind:
	rm -rf $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER).tar.xz \
		$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER) \
		$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)

get-llvmunwind: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER).tar.xz
extract-llvmunwind: $(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER)/source-extracted
configure-llvmunwind: $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured
compile-llvmunwind: $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-compiled
fastcheck-llvmunwind: check-llvmunwind
check-llvmunwind: # no test/check provided by Makefile

else # USE_BINARYBUILDER_LIBUNWIND

$(eval $(call bb-install,unwind,UNWIND,false))

$(eval $(call bb-install,llvmunwind,LLVMUNWIND,false))

endif
