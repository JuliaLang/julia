ifneq ($(USE_BINARYBUILDER_CSL),1)

# If we're not using BB-vendored CompilerSupportLibraries, then we must
# build our own by stealing the libraries from the currently-running system
CSL_FORTRAN_LIBDIR := $(dir $(shell $(FC) --print-file-name libgfortran.$(SHLIB_EXT)))
CSL_CXX_LIBDIR := $(dir $(shell $(CXX) --print-file-name libstdc++.$(SHLIB_EXT)))

CXX_LIBS := libgcc_s libstdc++ libc++ libgomp
FORTRAN_LIBS := libgfortran libquadmath

define cxx_src
$(CSL_CXX_LIBDIR)/$(1)*.$(SHLIB_EXT)*
endef
define fortran_src
$(CSL_FORTRAN_LIBDIR)/$(1)*.$(SHLIB_EXT)*
endef

get-compilersupportlibraries:
extract-compilersupportlibraries:
configure-compilersupportlibraries:
compile-compilersupportlibraries:
install-compilersupportlibraries: | $(build_libdir)
	cp -va $(foreach lib,$(CXX_LIBS),$(call cxx_src $(lib))) $(build_libdir)/
	cp -va $(foreach lib,$(FORTRAN_LIBS),$(call fortran_src $(lib))) $(build_libdir)/

$(eval $(call jll-generate,CompilerSupportLibraries_jll, \
                           libgcc_s=\"libgcc_s\" \
						   libgomp=\"libgomp\" \
						   libgfortran=\"libgfortran\" \
						   libstdcxx=\"libstdc++\" \
                           ,,e66e0078-7015-5450-92f7-15fbd957f2ae,))
else # USE_BINARYBUILDER_CSL

# Install CompilerSupportLibraries_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,CompilerSupportLibraries_jll))

endif