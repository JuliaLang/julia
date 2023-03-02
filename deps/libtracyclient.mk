## LIBTRACYCLIENT ##
ifneq ($(USE_BINARYBUILDER_LIBTRACYCLIENT),1)
LIBTRACYCLIENT_GIT_URL:=https://github.com/wolfpld/tracy.git
LIBTRACYCLIENT_TAR_URL=https://api.github.com/repos/wolfpld/tracy/tarball/$1
$(eval $(call git-external,libtracyclient,LIBTRACYCLIENT,,,$(BUILDDIR)))

LIBTRACYCLIENT_BUILDDIR := $(BUILDDIR)/$(LIBTRACYCLIENT_SRC_DIR)
LIBTRACYCLIENT_SRCCACHE := $(SRCCACHE)/$(LIBTRACYCLIENT_SRC_DIR)

LIBTRACYCLIENT_CMAKE :=
LIBTRACYCLIENT_CMAKE += -DBUILD_SHARED_LIBS=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_FIBERS=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_NO_BROADCAST=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_ONLY_LOCALHOST=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_NO_CODE_TRANSFER=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_NO_FRAME_IMAGE=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_NO_CRASH_HANDLER=ON
LIBTRACYCLIENT_CMAKE += -DTRACY_ON_DEMAND=ON

$(LIBTRACYCLIENT_BUILDDIR)/cmake-patch-applied: $(LIBTRACYCLIENT_BUILDDIR)/source-extracted
ifneq ($(OS),WINNT)
	echo "target_compile_definitions(TracyClient PUBLIC __STDC_FORMAT_MACROS)" >> $(LIBTRACYCLIENT_BUILDDIR)/CMakeLists.txt
else
	echo "target_compile_definitions(TracyClient PUBLIC WINVER=0x0602 _WIN32_WINNT=0x0602)" >> $(LIBTRACYCLIENT_BUILDDIR)/CMakeLists.txt
endif
	echo 1 > $@

$(LIBTRACYCLIENT_BUILDDIR)/libTracyClient-freebsd-elfw.patch-applied: $(LIBTRACYCLIENT_BUILDDIR)/cmake-patch-applied
	cd $(LIBTRACYCLIENT_BUILDDIR) && \
		patch -p1 -f < $(SRCDIR)/patches/libTracyClient-freebsd-elfw.patch
	echo 1 > $@

$(LIBTRACYCLIENT_BUILDDIR)/libTracyClient-no-crash-handler.patch-applied: $(LIBTRACYCLIENT_BUILDDIR)/libTracyClient-freebsd-elfw.patch-applied
	cd $(LIBTRACYCLIENT_BUILDDIR) && \
		patch -p1 -f < $(SRCDIR)/patches/libTracyClient-no-crash-handler.patch
	echo 1 > $@

$(LIBTRACYCLIENT_BUILDDIR)/build-configured: $(LIBTRACYCLIENT_BUILDDIR)/libTracyClient-no-crash-handler.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		$(CMAKE) . $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) $(LIBTRACYCLIENT_CMAKE) \
		|| { echo '*** To install a newer version of cmake, run contrib/download_cmake.sh ***' && false; }
	echo 1 > $@

$(LIBTRACYCLIENT_BUILDDIR)/build-compiled: $(LIBTRACYCLIENT_BUILDDIR)/build-configured
	cd $(LIBTRACYCLIENT_BUILDDIR) && \
		$(if $(filter $(CMAKE_GENERATOR),make), \
		  $(MAKE), \
		  $(CMAKE) --build .)
	echo 1 > $@

$(eval $(call staged-install, \
	libtracyclient,$$(LIBTRACYCLIENT_SRC_DIR), \
	MAKE_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libtracyclient.$$(SHLIB_EXT) $$(build_shlibdir)/libtracyclient.$$(SHLIB_EXT)))

clean-libtracyclient:
	rm -rf $(LIBTRACYCLIENT_BUILDDIR)/build-configured $(LIBTRACYCLIENT_BUILDDIR)/build-compiled
	-$(MAKE) -C $(LIBTRACYCLIENT_BUILDDIR) clean

get-libtracyclient: $(LIBTRACYCLIENT_SRC_FILE)
extract-libtracyclient: $(LIBTRACYCLIENT_BUILDDIR)/source-extracted
configure-libtracyclient: $(LIBTRACYCLIENT_BUILDDIR)/build-configured
compile-libtracyclient: $(LIBTRACYCLIENT_BUILDDIR)/build-compiled
fastcheck-libtracyclient: check-libtracyclient
check-libtracyclient: compile-libtracyclient

else # USE_BINARYBUILDER_LIBTRACYCLIENT

$(eval $(call bb-install,libtracyclient,LIBTRACYCLIENT,false))

endif
