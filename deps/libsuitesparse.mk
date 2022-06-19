## LIBSUITESPARSE ##

ifeq ($(USE_BLAS64), 1)
UMFPACK_CONFIG := -DLONGBLAS='long long'
CHOLMOD_CONFIG := -DLONGBLAS='long long'
SPQR_CONFIG := -DLONGBLAS='long long'
UMFPACK_CONFIG += -DSUN64
CHOLMOD_CONFIG += -DSUN64
SPQR_CONFIG += -DSUN64
endif

# Disable linking to libmetis
CHOLMOD_CONFIG += -DNPARTITION

ifneq ($(USE_BINARYBUILDER_LIBSUITESPARSE), 1)

LIBSUITESPARSE_PROJECTS := AMD BTF CAMD CCOLAMD COLAMD CHOLMOD LDL KLU UMFPACK RBio SPQR
LIBSUITESPARSE_LIBS := $(addsuffix .*$(SHLIB_EXT)*,suitesparseconfig amd btf camd ccolamd colamd cholmod klu ldl umfpack rbio spqr)

SUITESPARSE_LIB := $(LDFLAGS) -L"$(abspath $(BUILDDIR))/SuiteSparse-$(LIBSUITESPARSE_VER)/lib"
ifeq ($(OS), Darwin)
SUITESPARSE_LIB += $(RPATH_ESCAPED_ORIGIN)
endif
LIBSUITESPARSE_MFLAGS := CC="$(CC)" CXX="$(CXX)" F77="$(FC)" \
	  AR="$(AR)" RANLIB="$(RANLIB)" \
	  BLAS="-L$(build_shlibdir) -lblastrampoline" \
	  LAPACK="-L$(build_shlibdir) -lblastrampoline" \
	  LDFLAGS="$(SUITESPARSE_LIB)" CFOPENMP="" CUDA=no CUDA_PATH="" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" \
	  CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)" \
	  SPQR_CONFIG="$(SPQR_CONFIG)"
ifeq ($(OS),WINNT)
LIBSUITESPARSE_MFLAGS += UNAME=Windows
else
LIBSUITESPARSE_MFLAGS += UNAME=$(OS)
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

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/SuiteSparse-shlib.patch-applied: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted
	cd $(dir $@) && patch -p1 < $(SRCDIR)/patches/SuiteSparse-shlib.patch
	echo 1 > $@
$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/SuiteSparse-shlib.patch-applied

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/blastrampoline

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted
	$(MAKE) -C $(dir $<)SuiteSparse_config library config $(LIBSUITESPARSE_MFLAGS)
	$(INSTALL_NAME_CMD)libsuitesparseconfig.$(SHLIB_EXT) $(dir $<)lib/libsuitesparseconfig.$(SHLIB_EXT)
	for PROJ in $(LIBSUITESPARSE_PROJECTS); do \
		$(MAKE) -C $(dir $<)$${PROJ} library $(LIBSUITESPARSE_MFLAGS) || exit 1; \
		$(INSTALL_NAME_CMD)lib`echo $${PROJ} | tr A-Z a-z`.$(SHLIB_EXT) $(dir $<)lib/lib`echo $${PROJ} | tr A-Z a-z`.$(SHLIB_EXT) || exit 1; \
	done
	echo 1 > $@

ifeq ($(OS),WINNT)
LIBSUITESPARSE_SHLIB_ENV:=PATH="$(abspath $(dir $<))lib:$(build_bindir):$(PATH)"
else
LIBSUITESPARSE_SHLIB_ENV:=LD_LIBRARY_PATH="$(build_shlibdir)"
endif
$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-checked: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled
	for PROJ in $(LIBSUITESPARSE_PROJECTS); do \
		$(LIBSUITESPARSE_SHLIB_ENV) $(MAKE) -C $(dir $<)$${PROJ} default $(LIBSUITESPARSE_MFLAGS) || exit 1; \
	done
	echo 1 > $@

UNINSTALL_suitesparse := $(LIBSUITESPARSE_VER) manual_suitesparse $(LIBSUITESPARSE_LIBS)

$(build_prefix)/manifest/libsuitesparse: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled | $(build_prefix)/manifest $(build_shlibdir)
	for lib in $(LIBSUITESPARSE_LIBS); do \
		cp -a $(dir $<)lib/lib$${lib} $(build_shlibdir) || exit 1; \
	done
	#cp -a $(dir $<)lib/* $(build_shlibdir)
	#cp -a $(dir $<)include/* $(build_includedir)
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
