## ittapi ##
include $(SRCDIR)/ittapi.version

ITTAPI_GIT_URL := https://github.com/intel/ittapi.git
ITTAPI_TAR_URL = https://api.github.com/repos/intel/ittapi/tarball/$1
$(eval $(call git-external,ittapi,ITTAPI,CMakeLists.txt,,$(SRCCACHE)))

ITTAPI_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DITT_API_IPT_SUPPORT= -DITT_API_FORTRAN_SUPPORT=0

$(BUILDDIR)/$(ITTAPI_SRC_DIR)/build-configured: $(SRCCACHE)/$(ITTAPI_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(ITTAPI_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(ITTAPI_SRC_DIR)/build-compiled: $(BUILDDIR)/$(ITTAPI_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

define ITTAPI_INSTALL
	mkdir -p $2/$$(build_libdir)
	mkdir -p $2/$$(build_includedir)/ittapi
	cp -a $1/bin/libittnotify.a $2/$$(build_libdir)
	cp -a $1/bin/libjitprofiling.a $2/$$(build_libdir)
	# cp -a $1/bin/libadvisor.a $2/$$(build_libdir)
	cp -a $(SRCCACHE)/$(ITTAPI_SRC_DIR)/include/ittnotify.h $2/$$(build_includedir)/ittapi/
	cp -a $(SRCCACHE)/$(ITTAPI_SRC_DIR)/include/ittnotify-zca.h $2/$$(build_includedir)/ittapi/
	cp -a $(SRCCACHE)/$(ITTAPI_SRC_DIR)/include/jitprofiling.h $2/$$(build_includedir)/ittapi/
endef

$(eval $(call staged-install, \
	ittapi,$(ITTAPI_SRC_DIR), \
	ITTAPI_INSTALL,,,))

get-ittapi: $(ITTAPI_SRC_FILE)
extract-ittapi: $(SRCCACHE)/$(ITTAPI_SRC_DIR)/source-extracted
configure-ittapi: $(BUILDDIR)/$(ITTAPI_SRC_DIR)/build-configured
compile-ittapi: $(BUILDDIR)/$(ITTAPI_SRC_DIR)/build-compiled
fastcheck-ittapi: #none
check-ittapi: #none

clean-ittapi:
	-rm -f $(BUILDDIR)/$(ITTAPI_SRC_DIR)/build-compiled $(build_libdir)/libopenlibm.a
