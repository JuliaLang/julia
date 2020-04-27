ifneq ($(USE_BINARYBUILDER_CSL),1)

# If we're not using BB-vendored CompilerSupportLibraries, then we just generate
# a stub JLL that loads whatever the system is running.  The downside of this,
# of course, is that this is much less backwards-compatible.  The one exception
# is FreeBSD, where we cannot rely upon the `libgcc_s` in `/lib`, but must
# explicitly link against the `libgcc_s` that comes with GCC, which we find here:

ifeq ($(OS),FreeBSD)
UNINSTALL_compilersupportlibraries := delete-uninstaller $(build_shlibdir)/libgcc_s.so.1
$(build_prefix)/manifest/compilersupportlibraries: | $(build_shlibdir) $(build_prefix)/manifest
	@cp -v $$($(FC) --print-file-name=libgcc_s.so.1) $(build_shlibdir)
	@ln -s libgcc_s.so.1 $(build_shlibdir)/libgcc_s.so
	@echo "delete-uninstaller $(build_shlibdir)/libgcc_s.so.1" > "$@"
else
$(build_prefix)/manifest/compilersupportlibraries:
	touch "$@"
endif

LIBGFORTRAN_SOVER := $(subst libgfortran,,$(word 4, $(subst -, ,$(BB_TRIPLET_LIBGFORTRAN_CXXABI))))

ifeq ($(OS),Linux)
LIBGCC_S_SONAME := libgcc_s.so.1
LIBGFORTRAN_SONAME := libgfortran.so.$(LIBGFORTRAN_SOVER)
LIBSTDCXX_SONAME := libstdc++.so.6
CSL_LIBNAMES := libgcc_s libgfortran libstdcxx
else ifeq ($(OS),Darwin)
LIBGCC_S_SONAME := libgcc_s.1.dylib
LIBGFORTRAN_SONAME := libgfortran.$(LIBGFORTRAN_SOVER).dylib
# We're not going to load this by default, since it's built with clang
#LIBSTDCXX_SONAME := libstdc++.6.dylib
CSL_LIBNAMES := libgcc_s libgfortran
else ifeq ($(OS),WINNT)
LIBGCC_S_SONAME := libgcc_s_seh-1.dll
LIBGFORTRAN_SONAME := libgfortran-$(LIBGFORTRAN_SOVER).dll
LIBSTDCXX_SONAME := libstdc++-6.dll
CSL_LIBNAMES := libgcc_s libgfortran libstdcxx
else ifeq ($(OS),FreeBSD)
LIBGCC_S_SONAME := libgcc_s.so.1
LIBGFORTRAN_SONAME := libgfortran.so.$(LIBGFORTRAN_SOVER)
# We're not going to load this by default, since it's built with clang
#LIBSTDCXX_SONAME := libstdc++.so.6
CSL_LIBNAMES := libgcc_s libgfortran
endif

get-compilersupportlibraries:
extract-compilersupportlibraries:
configure-compilersupportlibraries:
compile-compilersupportlibraries:
install-compilersupportlibraries: $(build_prefix)/manifest/compilersupportlibraries

CSL_LIB_MAPPINGS := $(foreach lib,$(CSL_LIBNAMES),$(lib)=\"$($(call uppercase,$(lib))_SONAME)\")
$(eval $(call jll-generate,CompilerSupportLibraries_jll,$(CSL_LIB_MAPPINGS),,e66e0078-7015-5450-92f7-15fbd957f2ae,))
else # USE_BINARYBUILDER_CSL

# Install CompilerSupportLibraries_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,CompilerSupportLibraries_jll))

endif