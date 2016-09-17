## Some shared configuration options ##

CONFIGURE_COMMON := --prefix=$(abspath $(build_prefix)) --build=$(BUILD_MACHINE) --libdir=$(abspath $(build_libdir)) --bindir=$(abspath $(build_depsbindir)) $(CUSTOM_LD_LIBRARY_PATH)
ifneq ($(XC_HOST),)
CONFIGURE_COMMON += --host=$(XC_HOST)
endif
ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
CONFIGURE_COMMON += LDFLAGS="$(LDFLAGS) -Wl,--stack,8388608"
endif
endif
CONFIGURE_COMMON += F77="$(FC)" CC="$(CC) $(DEPS_CFLAGS)" CXX="$(CXX) $(DEPS_CXXFLAGS)"

CMAKE_CC_ARG := $(CC_ARG) $(DEPS_CFLAGS)
CMAKE_CXX_ARG := $(CXX_ARG) $(DEPS_CXXFLAGS)

CMAKE_COMMON := -DCMAKE_INSTALL_PREFIX:PATH=$(build_prefix) -DCMAKE_PREFIX_PATH=$(build_prefix)
CMAKE_COMMON += -DCMAKE_INSTALL_LIBDIR=$(build_libdir) -DCMAKE_INSTALL_BINDIR=$(build_bindir)
CMAKE_COMMON += -DLIB_INSTALL_DIR=$(build_shlibdir)
ifneq ($(VERBOSE), 0)
CMAKE_COMMON += -DCMAKE_VERBOSE_MAKEFILE=ON
endif
# The call to which here is to work around https://cmake.org/Bug/view.php?id=14366
CMAKE_COMMON += -DCMAKE_C_COMPILER="$$(which $(CC_BASE))"
ifneq ($(strip $(CMAKE_CC_ARG)),)
CMAKE_COMMON += -DCMAKE_C_COMPILER_ARG1="$(CMAKE_CC_ARG)"
endif
CMAKE_COMMON += -DCMAKE_CXX_COMPILER="$(CXX_BASE)"
ifneq ($(strip $(CMAKE_CXX_ARG)),)
CMAKE_COMMON += -DCMAKE_CXX_COMPILER_ARG1="$(CMAKE_CXX_ARG)"
endif

ifeq ($(OS),WINNT)
CMAKE_COMMON += -DCMAKE_SYSTEM_NAME=Windows
ifneq ($(BUILD_OS),WINNT)
CMAKE_COMMON += -DCMAKE_RC_COMPILER="$$(which $(CROSS_COMPILE)windres)"
endif
endif

# For now this is LLVM specific, but I expect it won't be in the future
ifeq ($(LLVM_USE_CMAKE),1)
ifeq ($(CMAKE_GENERATOR),Ninja)
CMAKE_GENERATOR_COMMAND := -G Ninja
else ifeq ($(CMAKE_GENERATOR),make)
CMAKE_GENERATOR_COMMAND := -G "Unix Makefiles"
else
$(error Unknown CMake generator '$(CMAKE_GENERATOR)'. Options are 'Ninja' and 'make')
endif
endif

# If the top-level Makefile is called with environment variables,
# they will override the values passed above to ./configure
MAKE_COMMON := DESTDIR="" prefix=$(build_prefix) bindir=$(build_depsbindir) libdir=$(build_libdir) shlibdir=$(build_shlibdir) libexecdir=$(build_libexecdir) datarootdir=$(build_datarootdir) includedir=$(build_includedir) sysconfdir=$(build_sysconfdir) O=

#Platform specific flags

ifeq ($(OS), WINNT)
LIBTOOL_CCLD := CCLD="$(CC) -no-undefined -avoid-version"
endif

# Cross-deps flags

USE_BLAS_FFLAGS :=
ifeq ($(USE_BLAS64), 1)
ifeq ($(USEIFC),1)
USE_BLAS_FFLAGS += -i8
else
USE_BLAS_FFLAGS += -fdefault-integer-8
endif
endif

ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
USE_BLAS_FFLAGS += -cpp -ffree-line-length-0 -ffixed-line-length-0 \
                   -Dsasum=sasum_gfort -Dscasum=scasum_gfort \
                   -Dscnrm2=scnrm2_gfort -Dsdot=sdot_gfort \
                   -Dsdsdot=sdsdot_gfort -Dsnrm2=snrm2_gfort \
                   -Dcdotc=cdotc_gfort -Dcdotu=cdotu_gfort \
                   -Dzdotc=zdotc_gfort -Dzdotu=zdotu_gfort \
                   \
                   -DSASUM=SASUM_GFORT -DSCASUM=SCASUM_GFORT \
                   -DSCNRM2=SCNRM2_GFORT -DSDOT=SDOT_GFORT \
                   -DSDSDOT=SDSDOT_GFORT -DSNRM2=SNRM2_GFORT \
                   -DCDOTC=CDOTC_GFORT -DCDOTU=CDOTU_GFORT \
                   -DZDOTC=ZDOTC_GFORT -DZDOTU=ZDOTU_GFORT
endif
endif
endif


## PATHS ##
# sort is used to remove potential duplicates
DIRS := $(sort $(build_bindir) $(build_depsbindir) $(build_libdir) $(build_includedir) $(build_sysconfdir) $(build_datarootdir) $(build_staging) $(build_prefix)/manifest)

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(build_prefix): | $(DIRS)
$(eval $(call dir_target,$(SRCDIR)/srccache))


upper = $(shell echo $1 | tr a-z A-Z)

## A rule for calling `make install` ##
# example usage:
#   $(call staged-install, \
#       1 target, \               # name
#       2 rel-build-directory, \  # BUILDDIR-relative path to binaries
#       3 MAKE_INSTALL, \         # will be called with args SRCDIR,DESTDIR,$4
#       4 add-args, \             # extra args for $3
#       5 (unused), \
#       6 post-install)           # post-install commands
#
# this rule ensures that make install is more nearly atomic
# so it's harder to get half-installed (or half-reinstalled) dependencies
# # and enables sharing deps compiles, uninstall, and fast reinstall
MAKE_INSTALL = +$$(MAKE) -C $1 install $$(MAKE_COMMON) $3 DESTDIR="$2"

define SHLIBFILE_INSTALL
	mkdir -p $2/$$(build_shlibdir)
	cp $3 $2/$$(build_shlibdir)
endef

define BINFILE_INSTALL
	mkdir -p $2/$$(build_depsbindir)
	cp $3 $2/$$(build_depsbindir)
endef

define staged-install
stage-$(strip $1): $$(build_staging)/$2.tgz
install-$(strip $1): $$(build_prefix)/manifest/$(strip $1)
uninstall-$(strip $1):
	-rm $$(build_prefix)/manifest/$(strip $1)
	-cd $$(build_prefix) && rm -dv -- $$$$($(TAR) -tzf $$(build_staging)/$2.tgz --exclude './$$$$')

ifeq (exists, $$(shell [ -e $$(build_staging)/$2.tgz ] && echo exists ))
# clean depends on uninstall only if the staged file exists
distclean-$(strip $1) clean-$(strip $1): uninstall-$(strip $1)
else
# uninstall depends on staging only if the staged file doesn't exist
# otherwise, uninstall doesn't actually want the file to be updated first
uninstall-$(strip $1): | $$(build_staging)/$2.tgz
endif

reinstall-$(strip $1):
	+$$(MAKE) uninstall-$(strip $1)
	-rm $$(build_staging)/$2.tgz
	+$$(MAKE) stage-$(strip $1)
	+$$(MAKE) install-$(strip $1)

$$(build_staging)/$2.tgz: $$(BUILDDIR)/$2/build-compiled
	rm -rf $$(build_staging)/$2
	mkdir -p $$(build_staging)/$2$$(build_prefix)
	$(call $3,$$(BUILDDIR)/$2,$$(build_staging)/$2,$4)
	cd $$(build_staging)/$2$$(build_prefix) && tar -czf $$@.tmp .
	rm -rf $$(build_staging)/$2
	mv $$@.tmp $$@

$$(build_prefix)/manifest/$(strip $1): $$(build_staging)/$2.tgz | $(build_prefix)/manifest
	mkdir -p $$(build_prefix)
	$(UNTAR) $$< -C $$(build_prefix)
	$6
	echo $2 > $$@
endef

ifneq (bsdtar,$(findstring bsdtar,$(TAR_TEST)))
#gnu tar
UNTAR = $(TAR) -xzf
else
#bsd tar
UNTAR = $(TAR) -xUzf
endif


## phony targets ##

.PHONY: default get extract configure compile fastcheck check install uninstall reinstall cleanall distcleanall \
	get-* extract-* configure-* compile-* fastcheck-* check-* install-* uninstall-* reinstall-* clean-* distclean-* \
	update-llvm
