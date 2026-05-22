# Interrogate the fortran compiler (which is always GCC based) on where it is keeping its libraries
STD_LIB_PATH := $(shell LANG=C $(FC) -print-search-dirs 2>/dev/null | grep '^programs: =' | sed -e "s/^programs: =//")
STD_LIB_PATH += $(PATHSEP)$(shell LANG=C $(FC) -print-search-dirs 2>/dev/null | grep '^libraries: =' | sed -e "s/^libraries: =//")
ifeq ($(BUILD_OS),WINNT)  # the mingw compiler lies about it search directory paths
STD_LIB_PATH += $(shell echo '$(STD_LIB_PATH)' | sed -e "s!/lib/!/bin/!g")
endif

# Given a $(PATHSEP)-separated list of paths in $(2), find the location of the library given in $(1)
define pathsearch
$(firstword $(wildcard $(addsuffix /$(1),$(subst $(PATHSEP), ,$(2)))))
endef

# CSL bundles lots of system compiler libraries, and while it is quite bleeding-edge
# as compared to what most distros ship, if someone tries to build an older branch,
# the version of CSL that ships with that branch may be relatively old. This is not
# a problem for code that is built in BB, but when we build Julia with the system
# compiler, that compiler uses the version of `libstdc++` that it is bundled with,
# and we can get linker errors when trying to run that 	`julia` executable with the
# `libstdc++` that comes from the (now old) BB-built CSL.
#
# To fix this, we take note when the system `libstdc++.so` is newer than whatever we
# would get from CSL (by searching for a `GLIBCXX_3.4.X` symbol that does not exist
# in our CSL, but would in a newer one), and default to `USE_BINARYBUILDER_CSL=0` in
# this case.

# First, check to see if BB is disabled on a global setting
ifeq ($(USE_BINARYBUILDER),0)
USE_BINARYBUILDER_CSL ?= 0
else
# If it's not, check to see if it's disabled by a USE_SYSTEM_xxx flag
ifeq ($(USE_SYSTEM_CSL),1)
USE_BINARYBUILDER_CSL ?= 0
else
# If it's not, see if we should disable it due to `libstdc++` being newer:
LIBSTDCXX_PATH := $(call pathsearch,$(call versioned_libname,libstdc++,6),$(STD_LIB_PATH))
ifneq (,$(and $(LIBSTDCXX_PATH),$(shell objdump -p '$(LIBSTDCXX_PATH)' | grep '$(CSL_NEXT_GLIBCXX_VERSION)')))
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
	-@SRC_LIB='$$(call pathsearch,$(1),$$(STD_LIB_PATH))'; \
	[ -n "$$$${SRC_LIB}" ] && cp "$$$${SRC_LIB}" '$$(build_shlibdir)'
endef

define copy_csl_static
install-csl: | $$(build_libdir) $$(build_libdir)/$(1)
$$(build_libdir)/$(1): | $$(build_libdir)
	-@SRC='$$(call pathsearch,$(1),$$(STD_LIB_PATH))'; \
	[ -n "$$$${SRC}" ] && cp "$$$${SRC}" '$$(build_libdir)'
endef

# Like copy_csl_static, but stages into $(build_private_libdir) instead of
# $(build_libdir). See JuliaLang/julia#51698.
define copy_csl_static_private
install-csl: | $$(build_private_libdir) $$(build_private_libdir)/$(1)
$$(build_private_libdir)/$(1): | $$(build_private_libdir)
	-@SRC='$$(call pathsearch,$(1),$$(STD_LIB_PATH))'; \
	[ -n "$$$${SRC}" ] && cp "$$$${SRC}" '$$(build_private_libdir)'
endef

# libgfortran has multiple names; we're just going to copy any version we can find
# Since we're only looking in the location given by `$(FC)` this should only succeed for one.
$(eval $(call copy_csl,$(call versioned_libname,libgfortran,3)))
$(eval $(call copy_csl,$(call versioned_libname,libgfortran,4)))
$(eval $(call copy_csl,$(call versioned_libname,libgfortran,5)))

# These are all libraries that we should always have
$(eval $(call copy_csl,$(call versioned_libname,libquadmath,0)))
$(eval $(call copy_csl,$(call versioned_libname,libstdc++,6)))
$(eval $(call copy_csl,$(call versioned_libname,libatomic,1)))
$(eval $(call copy_csl,$(call versioned_libname,libgomp,1)))

# Configurable either a static or dynamic library depending on the system
$(eval $(call copy_csl,$(call versioned_libname,libssp,0)))

ifeq ($(OS),WINNT)
# These are static gcc runtime libraries / objects for linking pkgimages
$(eval $(call copy_csl_static_private,libgcc.a))
$(eval $(call copy_csl_static_private,libgcc_s.a))
$(eval $(call copy_csl_static_private,libmsvcrt.a))
$(eval $(call copy_csl_static_private,libmingwex.a))
$(eval $(call copy_csl_static_private,libkernel32.a))
$(eval $(call copy_csl_static_private,libmingw32.a))
$(eval $(call copy_csl_static_private,libmoldname.a))
$(eval $(call copy_csl_static_private,libadvapi32.a))
$(eval $(call copy_csl_static_private,libshell32.a))
$(eval $(call copy_csl_static_private,libuser32.a))
$(eval $(call copy_csl_static_private,libpthread.dll.a))
$(eval $(call copy_csl_static_private,dllcrt2.o))
$(eval $(call copy_csl_static_private,crtbegin.o))
$(eval $(call copy_csl_static_private,crtend.o))
# Windows has special gcc_s names
ifeq ($(ARCH),i686)
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s_sjlj,1)))
else
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s_seh,1)))
endif
else
ifeq ($(OS),Darwin)
# On macOS, libgcc_s has soversion 1.1 always on aarch64 and only for GCC 12+
# (-> libgfortran 5) on x86_64
ifeq ($(ARCH),aarch64)
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s,1.1)))
else
ifeq ($(LIBGFORTRAN_VERSION),5)
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s,1.1)))
else
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s,1)))
endif
endif
else
# Other targets just use libgcc_s.1
$(eval $(call copy_csl,$(call versioned_libname,libgcc_s,1)))
install-csl: $(build_shlibdir)/libgcc_s.$(SHLIB_EXT)
$(build_shlibdir)/libgcc_s.$(SHLIB_EXT): $(build_shlibdir)/$(call versioned_libname,libgcc_s,1)
	ln -sf $(call versioned_libname,libgcc_s,1) $@
# These are static gcc runtime libraries / objects for linking pkgimages
$(eval $(call copy_csl_static,libgcc.a))
$(eval $(call copy_csl_static,crti.o))
$(eval $(call copy_csl_static,crtn.o))
$(eval $(call copy_csl_static,crtbeginS.o))
$(eval $(call copy_csl_static,crtendS.o))
ifeq ($(OS),Linux) # glibc-specific
$(eval $(call copy_csl_static,libc_nonshared.a))
endif
endif
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
GCC_VERSION = 15
ifeq ($(OS),WINNT)
install-csl:
	mkdir -p $(build_private_libdir)/
	cp -a $(build_shlibdir)/$(call versioned_libname,libstdc++,6) $(build_shlibdir)/libstdc++.$(SHLIB_EXT)
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libgcc_s.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libgcc.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libmsvcrt.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libmingwex.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libkernel32.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libmingw32.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libmoldname.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libadvapi32.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libshell32.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libuser32.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libpthread.dll.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libssp.dll.a $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/dllcrt2.o $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/crtbegin.o $(build_private_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/crtend.o $(build_private_libdir)/
else ifneq ($(OS),Darwin)
install-csl:
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libgcc.a $(build_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/crti.o $(build_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/crtn.o $(build_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/crtbeginS.o $(build_libdir)/
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/crtendS.o $(build_libdir)/
	ln -sf $(call versioned_libname,libgcc_s,1) $(build_shlibdir)/libgcc_s.$(SHLIB_EXT)
ifeq ($(OS),Linux)
	cp -a $(build_libdir)/gcc/$(BB_TRIPLET)/$(GCC_VERSION)/libc_nonshared.a $(build_libdir)/
endif
endif
endif

ifeq ($(OS),WINNT)
uninstall-csl: uninstall-gcc-libraries
uninstall-gcc-libraries:
	-rm -f $(build_shlibdir)/libstdc++.$(SHLIB_EXT)
	-rm -f $(build_private_libdir)/libgcc_s.a
	-rm -f $(build_private_libdir)/libgcc.a
	-rm -f $(build_private_libdir)/libmsvcrt.a
	-rm -f $(build_private_libdir)/libmingwex.a
	-rm -f $(build_private_libdir)/libkernel32.a
	-rm -f $(build_private_libdir)/libmingw32.a
	-rm -f $(build_private_libdir)/libmoldname.a
	-rm -f $(build_private_libdir)/libadvapi32.a
	-rm -f $(build_private_libdir)/libshell32.a
	-rm -f $(build_private_libdir)/libuser32.a
	-rm -f $(build_private_libdir)/libpthread.dll.a
	-rm -f $(build_private_libdir)/libssp.dll.a
	-rm -f $(build_private_libdir)/dllcrt2.o
	-rm -f $(build_private_libdir)/crtbegin.o
	-rm -f $(build_private_libdir)/crtend.o
.PHONY: uninstall-gcc-libraries
else ifneq ($(OS),Darwin)
uninstall-csl: uninstall-gcc-libraries
uninstall-gcc-libraries:
	-rm -f $(build_libdir)/libgcc.a
	-rm -f $(build_libdir)/libc_nonshared.a
	-rm -f $(build_shlibdir)/libgcc_s.$(SHLIB_EXT)
	-rm -f $(build_libdir)/crti.o
	-rm -f $(build_libdir)/crtn.o
	-rm -f $(build_libdir)/crtbeginS.o
	-rm -f $(build_libdir)/crtendS.o
.PHONY: uninstall-gcc-libraries
endif
