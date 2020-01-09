## SUITESPARSE ##

ifeq ($(USE_BLAS64), 1)
UMFPACK_CONFIG := -DLONGBLAS='long long'
CHOLMOD_CONFIG := -DLONGBLAS='long long'
SPQR_CONFIG := -DLONGBLAS='long long'
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
UMFPACK_CONFIG += -DSUN64
CHOLMOD_CONFIG += -DSUN64
SPQR_CONFIG += -DSUN64
endif
endif

# Disable trying to link against libmetis
CHOLMOD_CONFIG += -DNPARTITION

ifneq ($(USE_BINARYBUILDER_SUITESPARSE), 1)

SUITESPARSE_PROJECTS := AMD BTF CAMD CCOLAMD COLAMD CHOLMOD LDL KLU UMFPACK RBio SPQR
SUITESPARSE_LIBS := $(addsuffix .*$(SHLIB_EXT)*,suitesparseconfig amd btf camd ccolamd colamd cholmod klu umfpack rbio spqr)

SUITE_SPARSE_LIB := $(LDFLAGS) -L"$(abspath $(BUILDDIR))/SuiteSparse-$(SUITESPARSE_VER)/lib"
ifeq ($(OS), Darwin)
SUITE_SPARSE_LIB += $(RPATH_ESCAPED_ORIGIN)
endif
SUITESPARSE_MFLAGS := CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  LDFLAGS="$(SUITE_SPARSE_LIB)" CFOPENMP="" CUDA=no CUDA_PATH="" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)" SPQR_CONFIG="$(SPQR_CONFIG)"
ifeq ($(OS),WINNT)
SUITESPARSE_MFLAGS += UNAME=Windows
else
SUITESPARSE_MFLAGS += UNAME=$(OS)
endif

$(SRCCACHE)/SuiteSparse-$(SUITESPARSE_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/DrTimothyAldenDavis/SuiteSparse/archive/v$(SUITESPARSE_VER).tar.gz

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted: $(SRCCACHE)/SuiteSparse-$(SUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -zxf $<
	echo 1 > $@

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-winclang.patch-applied: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted
	cd $(dir $@) && patch -p0 < $(SRCDIR)/patches/SuiteSparse-winclang.patch
	echo 1 > $@
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-shlib.patch-applied: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted
	cd $(dir $@) && patch -p1 < $(SRCDIR)/patches/SuiteSparse-shlib.patch
	echo 1 > $@
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-winclang.patch-applied
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-shlib.patch-applied

ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/openblas
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/lapack
endif

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted
	$(MAKE) -C $(dir $<)SuiteSparse_config library config $(SUITESPARSE_MFLAGS)
	$(INSTALL_NAME_CMD)libsuitesparseconfig.$(SHLIB_EXT) $(dir $<)lib/libsuitesparseconfig.$(SHLIB_EXT)
	for PROJ in $(SUITESPARSE_PROJECTS); do \
		$(MAKE) -C $(dir $<)$${PROJ} library $(SUITESPARSE_MFLAGS) || exit 1; \
		$(INSTALL_NAME_CMD)lib`echo $${PROJ} | tr A-Z a-z`.$(SHLIB_EXT) $(dir $<)lib/lib`echo $${PROJ} | tr A-Z a-z`.$(SHLIB_EXT) || exit 1; \
	done
	echo 1 > $@

ifeq ($(OS),WINNT)
SUITESPARSE_SHLIB_ENV:=PATH="$(abspath $(dir $<))lib:$(build_bindir):$(PATH)"
else
SUITESPARSE_SHLIB_ENV:=LD_LIBRARY_PATH="$(build_shlibdir)"
endif
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-checked: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled
	for PROJ in $(SUITESPARSE_PROJECTS); do \
		$(SUITESPARSE_SHLIB_ENV) $(MAKE) -C $(dir $<)$${PROJ} default $(SUITESPARSE_MFLAGS) || exit 1; \
	done
	echo 1 > $@

$(build_prefix)/manifest/suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled | $(build_prefix)/manifest $(build_shlibdir)
	for lib in $(SUITESPARSE_LIBS); do \
		cp -a $(dir $<)lib/lib$${lib} $(build_shlibdir) || exit 1; \
	done
	#cp -a $(dir $<)lib/* $(build_shlibdir)
	#cp -a $(dir $<)include/* $(build_includedir)
	echo $(SUITESPARSE_VER) > $@

uninstall-suitesparse:
	-rm $(build_prefix)/manifest/suitesparse
	-rm $(addprefix $(build_shlibdir)/lib, $(SUITESPARSE_LIBS))

clean-suitesparse: clean-suitesparse-wrapper uninstall-suitesparse
	-rm $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled
	-rm -fr $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib
	-rm -fr $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/include
	-$(MAKE) -C $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER) clean

distclean-suitesparse: clean-suitesparse-wrapper
	-rm -rf $(SRCCACHE)/SuiteSparse-$(SUITESPARSE_VER).tar.gz \
		$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)

get-suitesparse: $(SRCCACHE)/SuiteSparse-$(SUITESPARSE_VER).tar.gz
extract-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted
configure-suitesparse: extract-suitesparse
compile-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled
fastcheck-suitesparse: #none
check-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-checked
install-suitesparse: $(build_prefix)/manifest/suitesparse install-suitesparse-wrapper

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC := -I $(LOCALBASE)/include/suitesparse
SUITESPARSE_LIB := -lumfpack -lcholmod -lamd -lcamd -lcolamd -lspqr
else
SUITESPARSE_INC := -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse_config -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SPQR/Include
SUITESPARSE_LIB := -L$(build_shlibdir) -lcholmod -lumfpack -lspqr $(RPATH_ORIGIN)
$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT): $(build_prefix)/manifest/suitesparse
endif

$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT): $(SRCDIR)/SuiteSparse_wrapper.c
	mkdir -p $(build_shlibdir)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(SUITESPARSE_INC) $< -o $@ $(SUITESPARSE_LIB)
	$(INSTALL_NAME_CMD)libsuitesparse_wrapper.$(SHLIB_EXT) $@
	touch -c $@

clean-suitesparse-wrapper:
	-rm -f $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)

distclean-suitesparse-wrapper: clean-suitesparse-wrapper

get-suitesparse-wrapper:
extract-suitesparse-wrapper:
configure-suitesparse-wrapper:
compile-suitesparse-wrapper:
fastcheck-suitesparse-wrapper: #none
check-suitesparse-wrapper:
install-suitesparse-wrapper: $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)

else # USE_BINARYBUILDER_SUITESPARSE

SUITESPARSE_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/SuiteSparse_jll.jl/releases/download/SuiteSparse-v$(SUITESPARSE_VER)+$(SUITESPARSE_BB_REL)
SUITESPARSE_BB_NAME := SuiteSparse.v$(SUITESPARSE_VER)

$(eval $(call bb-install,suitesparse,SUITESPARSE,false))
get-suitesparse-wrapper: get-suitesparse
extract-suitesparse-wrapper: extract-suitesparse
configure-suitesparse-wrapper: configure-suitesparse
compile-suitesparse-wrapper: compile-suitesparse
fastcheck-suitesparse-wrapper: fastcheck-suitesparse
check-suitesparse-wrapper: check-suitesparse
clean-suitesparse-wrapper: clean-suitesparse
distclean-suitesparse-wrapper: distclean-suitesparse
install-suitesparse-wrapper: install-suitesparse

# suitesparse depends on OpenBLAS
compile-suitesparse: | $(build_prefix)/manifest/openblas
endif
