## LIBSUITESPARSE ##
include $(SRCDIR)/libsuitesparse.version

ifneq ($(USE_BINARYBUILDER_LIBSUITESPARSE), 1)

LIBSUITESPARSE_PROJECTS := "suitesparse_config;amd;btf;camd;ccolamd;colamd;cholmod;klu;ldl;umfpack;rbio;spqr"
LIBSUITESPARSE_LIBS := $(addsuffix .*$(SHLIB_EXT)*,suitesparseconfig $(subst ;, ,$(LIBSUITESPARSE_PROJECTS)))

ifeq ($(OS),WINNT)
BLAS_LIB_NAME_NO_EXT:=blastrampoline-5
else
BLAS_LIB_NAME_NO_EXT:=blastrampoline
endif

LIBSUITESPARSE_CMAKE_FLAGS := $(CMAKE_COMMON) \
	  -DCMAKE_BUILD_TYPE=Release \
	  -DBUILD_STATIC_LIBS=OFF \
	  -DBUILD_TESTING=OFF \
	  -DSUITESPARSE_ENABLE_PROJECTS=$(LIBSUITESPARSE_PROJECTS) \
	  -DSUITESPARSE_DEMOS=OFF \
	  -DSUITESPARSE_USE_STRICT=ON \
	  -DSUITESPARSE_USE_CUDA=OFF \
	  -DSUITESPARSE_USE_FORTRAN=OFF \
	  -DSUITESPARSE_USE_OPENMP=OFF \
	  -DCHOLMOD_PARTITION=ON \
	  -DBLAS_FOUND=1 \
	  -DBLAS_LIBRARIES="$(build_shlibdir)/lib$(BLAS_LIB_NAME_NO_EXT).$(SHLIB_EXT)" \
	  -DBLAS_LINKER_FLAGS="$(BLAS_LIB_NAME_NO_EXT)" \
	  -DBLA_VENDOR="$(BLAS_LIB_NAME_NO_EXT)" \
	  -DLAPACK_LIBRARIES="$(build_shlibdir)/lib$(BLAS_LIB_NAME_NO_EXT).$(SHLIB_EXT)" \
	  -DLAPACK_LINKER_FLAGS="${BLAS_LIB_NAME_NO_EXT}"

ifeq ($(BINARY),64)
LIBSUITESPARSE_CMAKE_FLAGS += -DBLAS64_SUFFIX="_64" -DSUITESPARSE_USE_64BIT_BLAS=YES
else
LIBSUITESPARSE_CMAKE_FLAGS += -DSUITESPARSE_USE_64BIT_BLAS=NO
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
LIBSUITESPARSE_CMAKE_FLAGS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/DrTimothyAldenDavis/SuiteSparse/archive/v$(LIBSUITESPARSE_VER).tar.gz

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted: $(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -zxf $<
	echo 1 > $@

checksum-libsuitesparse: $(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-patched: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted
	echo 1 > $@

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/blastrampoline

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-patched
	cd $(dir $<) && $(CMAKE) . $(LIBSUITESPARSE_CMAKE_FLAGS)
	make -C $(dir $<)
	make -C $(dir $<) install
	echo 1 > $@

ifeq ($(OS),WINNT)
LIBSUITESPARSE_SHLIB_ENV:=PATH="$(abspath $(dir $<))lib:$(build_bindir):$(PATH)"
else
LIBSUITESPARSE_SHLIB_ENV:=LD_LIBRARY_PATH="$(build_shlibdir)"
endif
$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-checked: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled
	for PROJ in $(shell echo $(subst ;, ,$(LIBSUITESPARSE_PROJECTS))); do \
		$(LIBSUITESPARSE_SHLIB_ENV) $(MAKE) -C $(dir $<)$${PROJ} default $(LIBSUITESPARSE_MFLAGS) || exit 1; \
	done
	echo 1 > $@

UNINSTALL_libsuitesparse := $(LIBSUITESPARSE_VER) manual_libsuitesparse $(LIBSUITESPARSE_LIBS)

$(build_prefix)/manifest/libsuitesparse: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled | $(build_prefix)/manifest $(build_shlibdir)
	echo $(UNINSTALL_libsuitesparse) > $@

clean-libsuitesparse: uninstall-libsuitesparse
	-rm -f $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled
	-rm -fr $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/lib
	-rm -fr $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/include
	-$(MAKE) -C $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER) clean

distclean-libsuitesparse:
	rm -rf $(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz \
		$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)

get-libsuitesparse: $(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz
extract-libsuitesparse: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted
configure-libsuitesparse: extract-libsuitesparse
compile-libsuitesparse: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled
fastcheck-libsuitesparse: #none
check-libsuitesparse: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-checked
install-libsuitesparse: $(build_prefix)/manifest/libsuitesparse

else # USE_BINARYBUILDER_LIBSUITESPARSE

$(eval $(call bb-install,libsuitesparse,LIBSUITESPARSE,false))

# libsuitesparse depends on blastrampoline
compile-libsuitesparse: | $(build_prefix)/manifest/blastrampoline
endif

define manual_libsuitesparse
uninstall-libsuitesparse:
	-rm -f $(build_prefix)/manifest/libsuitesparse
	-rm -f $(addprefix $(build_shlibdir)/lib,$3)
endef
