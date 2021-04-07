ifeq ($(USE_BINARYBUILDER_CSL),0)

# Interrogate the fortran compiler (which is always GCC based) on where it is keeping its libraries
STD_LIB_PATH := $(shell LANG=C $(FC) -print-search-dirs | grep '^programs: =' | sed -e "s/^programs: =//")
STD_LIB_PATH += :$(shell LANG=C $(FC) -print-search-dirs | grep '^libraries: =' | sed -e "s/^libraries: =//")
ifneq (,$(findstring CYGWIN,$(BUILD_OS))) # the cygwin-mingw32 compiler lies about it search directory paths
STD_LIB_PATH := $(shell echo '$(STD_LIB_PATH)' | sed -e "s!/lib/!/bin/!g")
endif

# Given a colon-separated list of paths in $(2), find the location of the library given in $(1)
define pathsearch
$(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))
endef

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
