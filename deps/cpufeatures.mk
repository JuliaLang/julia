## CPUFEATURES ##
CPUFEATURES_GIT_URL := https://github.com/gbaraldi/cpufeatures.git
CPUFEATURES_TAR_URL = https://api.github.com/repos/gbaraldi/cpufeatures/tarball/$1
$(eval $(call git-external,cpufeatures,CPUFEATURES,,,$(BUILDDIR)))

CPUFEATURES_CFLAGS  := -O2 -Wall -Wextra $(fPIC) $(SANITIZE_OPTS)
CPUFEATURES_CXXFLAGS := -std=c++17 -O2 -Wall -Wextra -fno-exceptions -fno-rtti $(fPIC) $(SANITIZE_OPTS)
CPUFEATURES_MFLAGS  := CC="$(CC)" CXX="$(CXX)" CFLAGS="$(CFLAGS) $(CPUFEATURES_CFLAGS)" CXXFLAGS="$(CXXFLAGS) $(CPUFEATURES_CXXFLAGS)" AR="$(AR)" ARCH=$(ARCH)
CPUFEATURES_BUILDDIR := $(BUILDDIR)/$(CPUFEATURES_SRC_DIR)

$(CPUFEATURES_BUILDDIR)/build-compiled: $(CPUFEATURES_BUILDDIR)/source-extracted
	$(MAKE) -C $(dir $<) $(CPUFEATURES_MFLAGS) lib
	echo 1 > $@

define CPUFEATURES_INSTALL
	mkdir -p $2/$$(build_includedir)/cpufeatures
	mkdir -p $2/$$(build_libdir)
	cp $1/include/*.h $2/$$(build_includedir)/cpufeatures/
	cp $1/generated/target_tables_*.h $2/$$(build_includedir)/cpufeatures/
	cp $1/build/libtarget_parsing.a $2/$$(build_libdir)/
endef
$(eval $(call staged-install, \
	cpufeatures,$(CPUFEATURES_SRC_DIR), \
	CPUFEATURES_INSTALL,,,))

clean-cpufeatures:
	-rm -f $(CPUFEATURES_BUILDDIR)/build-compiled

get-cpufeatures: $(CPUFEATURES_SRC_FILE)
extract-cpufeatures: $(CPUFEATURES_BUILDDIR)/source-extracted
configure-cpufeatures: extract-cpufeatures
compile-cpufeatures: $(CPUFEATURES_BUILDDIR)/build-compiled
fastcheck-cpufeatures: check-cpufeatures
check-cpufeatures: compile-cpufeatures
