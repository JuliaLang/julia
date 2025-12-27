## ARCHSPEC_CPP ##
# Force git mode since archspec_cpp has git submodules that aren't included in tarballs
DEPS_GIT += archspec
ARCHSPEC_GIT_URL := https://github.com/gbaraldi/archspec_cpp.git
ARCHSPEC_TAR_URL = https://api.github.com/repos/gbaraldi/archspec_cpp/tarball/$1
$(eval $(call git-external,archspec,ARCHSPEC,,,$(BUILDDIR)))

ARCHSPEC_OBJ_LIB    := $(build_libdir)/libarchspec.a
ARCHSPEC_CXXFLAGS   := -std=c++17 -O2 -fno-exceptions -fno-rtti -DJSON_NOEXCEPTION $(fPIC)
ARCHSPEC_MFLAGS     := CXX="$(CXX)" CXXFLAGS="$(CXXFLAGS) $(ARCHSPEC_CXXFLAGS)" AR="$(AR)"
ARCHSPEC_BUILDDIR   := $(BUILDDIR)/$(ARCHSPEC_SRC_DIR)

# Initialize only the json submodule (needed for nlohmann/json headers)
$(ARCHSPEC_BUILDDIR)/submodules-initialized: $(ARCHSPEC_BUILDDIR)/source-extracted
	cd $(ARCHSPEC_BUILDDIR) && git submodule update --init extern/json
	echo 1 > $@

$(ARCHSPEC_BUILDDIR)/build-compiled: $(ARCHSPEC_BUILDDIR)/submodules-initialized
	$(MAKE) -C $(ARCHSPEC_BUILDDIR) $(ARCHSPEC_MFLAGS) directories build/lib/libarchspec.a
	echo 1 > $@

define ARCHSPEC_INSTALL
	mkdir -p $2/$$(build_includedir)/archspec $2/$$(build_includedir)/nlohmann $2/$$(build_libdir)
	cp $1/include/archspec/*.hpp $2/$$(build_includedir)/archspec/
	cp $1/extern/json/single_include/nlohmann/json.hpp $2/$$(build_includedir)/nlohmann/
	cp $1/extern/json/single_include/nlohmann/json_fwd.hpp $2/$$(build_includedir)/nlohmann/
	cp $1/build/lib/libarchspec.a $2/$$(build_libdir)
endef
$(eval $(call staged-install, \
	archspec,$(ARCHSPEC_SRC_DIR), \
	ARCHSPEC_INSTALL,,,))

clean-archspec:
	-rm -f $(ARCHSPEC_BUILDDIR)/build-compiled
	-$(MAKE) -C $(ARCHSPEC_BUILDDIR) clean

get-archspec: $(ARCHSPEC_SRC_FILE)
extract-archspec: $(ARCHSPEC_BUILDDIR)/source-extracted
configure-archspec: extract-archspec
compile-archspec: $(ARCHSPEC_BUILDDIR)/build-compiled
fastcheck-archspec: check-archspec
check-archspec: compile-archspec
