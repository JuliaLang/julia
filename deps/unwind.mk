## UNWIND ##
include $(SRCDIR)/unwind.version
include $(SRCDIR)/llvmunwind.version

ifneq ($(USE_BINARYBUILDER_LIBUNWIND),1)
LIBUNWIND_CFLAGS := -U_FORTIFY_SOURCE $(fPIC) $(SANITIZE_OPTS)
LIBUNWIND_CPPFLAGS := -I$(build_includedir)
LIBUNWIND_LDFLAGS := -L$(build_shlibdir)

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

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-configure-static-lzma.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p0 -f -u -l < $(SRCDIR)/patches/libunwind-configure-static-lzma.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-revert_prelink_unwind.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-configure-static-lzma.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f -u -l < $(SRCDIR)/patches/libunwind-revert_prelink_unwind.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-aarch64-inline-asm.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-revert_prelink_unwind.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f -u -l < $(SRCDIR)/patches/libunwind-aarch64-inline-asm.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-disable-initial-exec-tls.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-aarch64-inline-asm.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f -u -l < $(SRCDIR)/patches/libunwind-disable-initial-exec-tls.patch
	echo 1 > $@

# note minidebuginfo requires liblzma, which we do not have a source build for
# (it will be enabled in BinaryBuilder-based downloads however)
# since https://github.com/JuliaPackaging/Yggdrasil/commit/0149e021be9badcb331007c62442a4f554f3003c
$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-disable-initial-exec-tls.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) CPPFLAGS="$(CPPFLAGS) $(LIBUNWIND_CPPFLAGS)" CFLAGS="$(CFLAGS) $(LIBUNWIND_CFLAGS)" LDFLAGS="$(LDFLAGS) $(LIBUNWIND_LDFLAGS)" --enable-shared --disable-minidebuginfo --disable-tests --enable-zlibdebuginfo --disable-conservative-checks --enable-per-thread-cache
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

LLVMUNWIND_OPTS := $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) \
	-DCMAKE_BUILD_TYPE=MinSizeRel \
	-DLIBUNWIND_ENABLE_PEDANTIC=OFF \
	-DLIBUNWIND_INCLUDE_DOCS=OFF \
	-DLIBUNWIND_INCLUDE_TESTS=OFF \
	-DLIBUNWIND_INSTALL_HEADERS=ON \
	-DLIBUNWIND_ENABLE_ASSERTIONS=OFF \
	-DLLVM_CONFIG_PATH=$(build_depsbindir)/llvm-config \
	-DLLVM_ENABLE_RUNTIMES="libunwind" \
	-DLLVM_PATH=$(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/llvm

$(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER).tar.xz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/llvm/llvm-project/releases/download/llvmorg-$(LLVMUNWIND_VER)/llvm-project-$(LLVMUNWIND_VER).src.tar.xz

$(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/source-extracted: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER).tar.xz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xf $<
	mv $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER).src $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)
	echo 1 > $@

$(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/llvm-libunwind-prologue-epilogue.patch-applied: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/source-extracted
	cd $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-prologue-epilogue.patch
	echo 1 > $@

$(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/llvm-libunwind-force-dwarf.patch-applied: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/llvm-libunwind-prologue-epilogue.patch-applied
	cd $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-force-dwarf.patch
	echo 1 > $@

$(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/llvm-libunwind-freebsd-libgcc-api-compat.patch-applied: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/llvm-libunwind-force-dwarf.patch-applied
	cd $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind && patch -p2 -f < $(SRCDIR)/patches/llvm-libunwind-freebsd-libgcc-api-compat.patch
	echo 1 > $@

checksum-llvmunwind: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER).tar.xz
	$(JLCHECKSUM) $<

$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/source-extracted $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/llvm-libunwind-freebsd-libgcc-api-compat.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) -S $(dir $<)/runtimes $(LLVMUNWIND_OPTS)
	echo 1 > $@

$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-compiled: $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured
	cd $(dir $<) && \
	$(if $(filter $(CMAKE_GENERATOR),make), \
		  $(MAKE), \
		  $(CMAKE) --build . --target unwind)
	echo 1 > $@

LIBUNWIND_INSTALL = \
	cd $1 && mkdir -p $2$$(build_depsbindir) && \
	$$(CMAKE) -DCMAKE_INSTALL_PREFIX="$2$$(build_prefix)" -P libunwind/cmake_install.cmake

$(eval $(call staged-install, \
	llvmunwind,llvmunwind-$(LLVMUNWIND_VER), \
	LIBUNWIND_INSTALL,,, \
	cp -fR $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/libunwind/* $(build_includedir)))

clean-llvmunwind:
	-rm -f $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-configured $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)/build-compiled
	rm -rf $(build_includedir)/mach-o/ $(build_includedir)/unwind.h $(build_includedir)/libunwind.h
	-$(MAKE) -C $(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER) clean

distclean-llvmunwind:
	rm -rf $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER).tar.xz \
		$(SRCCACHE)/llvmunwind-$(LLVMUNWIND_VER) \
		$(BUILDDIR)/llvmunwind-$(LLVMUNWIND_VER)

get-llvmunwind: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER).tar.xz
extract-llvmunwind: $(SRCCACHE)/llvm-project-$(LLVMUNWIND_VER)/source-extracted
configure-llvmunwind: $(BUILDDIR)/llvm-project-$(LLVMUNWIND_VER)/build-configured
compile-llvmunwind: $(BUILDDIR)/llvm-project-$(LLVMUNWIND_VER)/build-compiled
fastcheck-llvmunwind: check-llvmunwind
check-llvmunwind: # no test/check provided by Makefile

else # USE_BINARYBUILDER_LIBUNWIND

$(eval $(call bb-install,unwind,UNWIND,false))

$(eval $(call bb-install,llvmunwind,LLVMUNWIND,false))

endif
