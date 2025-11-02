## XXHASH ##
ifneq ($(USE_BINARYBUILDER_XXHASH),1)
XXHASH_GIT_URL := https://github.com/Cyan4973/xxHash.git
XXHASH_TAR_URL = https://api.github.com/repos/Cyan4973/xxHash/tarball/$1
$(eval $(call git-external,xxhash,XXHASH,,,$(BUILDDIR)))

XXHASH_SRCDIR := $(BUILDDIR)/$(XXHASH_SRC_DIR)
XXHASH_BUILDDIR := $(XXHASH_SRCDIR)/cmake-build

XXHASH_CMAKE_FLAGS :=
XXHASH_CMAKE_FLAGS += -DBUILD_SHARED_LIBS=OFF
XXHASH_CMAKE_FLAGS += -DXXHASH_BUILD_XXHSUM=OFF

$(XXHASH_SRCDIR)/build-configured: $(XXHASH_SRCDIR)/source-extracted
	mkdir -p $(XXHASH_BUILDDIR)
	cd $(XXHASH_BUILDDIR) && \
		$(CMAKE) ../cmake_unofficial $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) $(XXHASH_CMAKE_FLAGS) \
		|| { echo '*** To install a newer version of cmake, run contrib/download_cmake.sh ***' && false; }
	echo 1 > $@

$(XXHASH_SRCDIR)/build-compiled: $(XXHASH_SRCDIR)/build-configured
	cd $(XXHASH_BUILDDIR) && \
		$(if $(filter $(CMAKE_GENERATOR),make), \
		  $(MAKE), \
		  $(CMAKE) --build .)
	echo 1 > $@

define XXHASH_INSTALL
	$(CMAKE) --install "$1/cmake-build" --prefix "$2$(build_prefix)"
endef
$(eval $(call staged-install, \
	xxhash,$(XXHASH_SRC_DIR), \
	XXHASH_INSTALL,,, \
))

clean-xxhash:
	-rm -rf $(XXHASH_BUILDDIR)

get-xxhash: $(XXHASH_SRC_FILE)
extract-xxhash: $(XXHASH_SRCDIR)/source-extracted
configure-xxhash: $(XXHASH_BUILDDIR)/build-configured
compile-xxhash: $(XXHASH_BUILDDIR)/build-compiled
fastcheck-xxhash: check-xxhash
check-xxhash: compile-xxhash

else # USE_BINARYBUILDER_XXHASH

$(eval $(call bb-install,xxhash,XXHASH,false))

endif
