# Interrogate the fortran compiler (which is always GCC based) on where it is keeping its libraries
STD_LIB_PATH := $(shell LANG=C $(FC) -print-search-dirs 2>/dev/null | grep '^programs: =' | sed -e "s/^programs: =//")
STD_LIB_PATH += :$(shell LANG=C $(FC) -print-search-dirs 2>/dev/null | grep '^libraries: =' | sed -e "s/^libraries: =//")
ifneq (,$(findstring CYGWIN,$(BUILD_OS))) # the cygwin-mingw32 compiler lies about it search directory paths
STD_LIB_PATH := $(shell echo '$(STD_LIB_PATH)' | sed -e "s!/lib/!/bin/!g")
endif

# Given a colon-separated list of paths in $(2), find the location of the library given in $(1)
define pathsearch
$(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))
endef

# CSL bundles lots of system compiler libraries, and while it is quite bleeding-edge
# as compared to what most distros ship, if someone tries to build an older branch,
# the version of CSL that ships with that branch may become relatively old.  This is
# not a problem for code that is built in BB, but when we build Julia with the system
# compiler, that compiler uses the version of `libstdc++` that it is bundled with,
# and we can get linker errors when trying to run that `julia` executable with the
# `libstdc++` that comes from the (now old) BB-built CSL.
#
# To fix this, we take note when the system `libstdc++.so` is newer than whatever we
# would get from CSL (by searching for a `GLIBCXX_3.4.X` symbol that does not exist
# in our CSL, but would in a newer one), and default to `USE_BINARYBUILDER_CSL=0` in
# this case.
CSL_NEXT_GLIBCXX_VERSION=GLIBCXX_3\.4\.30|GLIBCXX_3\.5\.|GLIBCXX_4\.

# First, check to see if BB is disabled on a global setting
ifeq ($(USE_BINARYBUILDER),0)
USE_BINARYBUILDER_CSL ?= 0
else
# If it's not, check to see if it's disabled by a USE_SYSTEM_xxx flag
ifeq ($(USE_SYSTEM_CSL),1)
USE_BINARYBUILDER_CSL ?= 0
else
# If it's not, see if we should disable it due to `libstdc++` being newer:
LIBSTDCXX_PATH := $(eval $(call pathsearch,libstdc++,$(STD_LIB_PATH)))
ifneq (,$(and $(LIBSTDCXX_PATH),$(shell objdump -p $(LIBSTDCXX_PATH) | grep $(CSL_NEXT_GLIBCXX_VERSION))))
# Found `libstdc++`, grepped it for strings and found a `GLIBCXX` symbol
# that is newer that whatever we have in CSL.  Default to not using BB.
USE_BINARYBUILDER_CSL ?= 0
else
# Either we didn't find `libstdc++` (e.g. we're using `clang`), or we
# found it and couldn't find the new symbol in it (it's older than what
# BB provides, so let's use BB instead)
USE_BINARYBUILDER_CSL ?= 1
endif
endif
endif

ifeq ($(USE_BINARYBUILDER_CSL),0)
define copy_csl
install-csl: | $$(build_shlibdir) $$(build_shlibdir)/$(1)
$$(build_shlibdir)/$(1): | $$(build_shlibdir)
	-@SRC_LIB=$$(call pathsearch,$(1),$$(STD_LIB_PATH)); \
	[ -n "$$$${SRC_LIB}" ] && cp $$$${SRC_LIB} $$(build_shlibdir)
endef

# libgfortran has multiple names; we're just going to copy any version we can find
# Since we're only looking in the location given by `$(FC)` this should only succeed for one.
$(eval $(call copy_csl,$(call versioned_libname,libgfortran,3)))
$(eval $(call copy_csl,$(call versioned_libname,libgfortran,4)))
$(eval $(call copy_csl,$(call versioned_libname,libgfortran,5)))

# These are all libraries that we should always have
$(eval $(call copy_csl,$(call versioned_libname,libquadmath,0)))
$(eval $(call copy_csl,$(call versioned_libname,libstdc++,6)))
$(eval $(call copy_csl,$(call versioned_libname,libssp,0)))
$(eval $(call copy_csl,$(call versioned_libname,libatomic,1)))
$(eval $(call copy_csl,$(call versioned_libname,libgomp,1)))

ifeq ($(OS),WINNT)
# Windwos has special gcc_s names
ifeq ($(ARCH),i686)
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s_sjlj,1)))
else
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s_seh,1)))
endif
else
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s,1)))
endif
# winpthread is only Windows, pthread is only others
ifeq ($(OS),WINNT)
$(eval $(call copy_csl,$(call versioned_libname,libwinpthread,1)))
else
$(eval $(call copy_csl,$(call versioned_libname,libpthread,0)))
endif

get-csl:
clean-csl:
	-rm -f $(build_shlibdir)/libgfortran*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libquadmath*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libstdc++*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libc++*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libgcc_s*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libssp*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libpthread*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libwinpthread*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libatomic*$(SHLIB_EXT)*
	-rm -f $(build_shlibdir)/libgomp*$(SHLIB_EXT)*
distclean-csl: clean-csl

else
$(eval $(call bb-install,csl,CSL,true))
endif
