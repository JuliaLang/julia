## SUITESPARSE ##

ifeq ($(USE_BLAS64), 1)
UMFPACK_CONFIG := -DLONGBLAS='long long'
CHOLMOD_CONFIG := -DLONGBLAS='long long'
SPQR_CONFIG := -DLONGBLAS='long long'
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
UMFPACK_CONFIG += -DSUN64
CHOLMOD_CONFIG += -DSUN64
SPQR_CONFIG += -DSUN64
endif
endif
endif

SUITE_SPARSE_LIB := -lm
ifneq ($(OS), Darwin)
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -lrt
endif
endif
SUITE_SPARSE_LIB += $(RPATH_ESCAPED_ORIGIN)
SUITESPARSE_MFLAGS := CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  INSTALL_LIB="$(build_libdir)" INSTALL_INCLUDE="$(build_includedir)" LIB="$(SUITE_SPARSE_LIB)" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)" SPQR_CONFIG="$(SPQR_CONFIG)"

$(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://faculty.cse.tamu.edu/davis/SuiteSparse/$(notdir $@)

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted: $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -zxf $<
	echo 1 > $@

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-winclang.patch-applied: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted
	cd $(dir $@) && patch -p0 < $(SRCDIR)/patches/SuiteSparse-winclang.patch
	echo 1 > $@

ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/openblas
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/lapack
endif
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-winclang.patch-applied
	$(MAKE) -C $(dir $<) library $(SUITESPARSE_MFLAGS)
	echo 1 > $@

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-checked: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled
	$(MAKE) -C $(dir $@) default $(SUITESPARSE_MFLAGS)
	echo 1 > $@

$(build_prefix)/manifest/suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled | $(build_prefix)/manifest
	mkdir -p $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib && \
	cd $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find .. -name libamd.a -o -name libcolamd.a -o -name libcamd.a -o -name libccolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libsuitesparseconfig.a -o -name libspqr.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libsuitesparseconfig.$(SHLIB_EXT) $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(build_shlibdir)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcolamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(build_shlibdir)/libcolamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcamd.$(SHLIB_EXT) $(build_shlibdir)/libcamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libccolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libccolamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libccolamd.$(SHLIB_EXT) $(build_shlibdir)/libccolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcholmod.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcolamd -lamd -lcamd -lccolamd -lsuitesparseconfig $(LIBLAPACK) $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(build_shlibdir)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libumfpack.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcholmod -lcolamd -lamd -lsuitesparseconfig $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(build_shlibdir)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libspqr.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libspqr.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcholmod -lcolamd -lamd -lsuitesparseconfig $(LIBLAPACK) $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(build_shlibdir)/libspqr.$(SHLIB_EXT)
	echo $(SUITESPARSE_VER) > $@

clean-suitesparse: clean-suitesparse-wrapper
	-rm $(build_prefix)/manifest/suitesparse $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled
	-rm $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) \
		$(build_shlibdir)/libamd.$(SHLIB_EXT) \
		$(build_shlibdir)/libcolamd.$(SHLIB_EXT) \
		$(build_shlibdir)/libcamd.$(SHLIB_EXT) \
		$(build_shlibdir)/libccolamd.$(SHLIB_EXT) \
		$(build_shlibdir)/libcholmod.$(SHLIB_EXT) \
		$(build_shlibdir)/libumfpack.$(SHLIB_EXT) \
		$(build_shlibdir)/libspqr.$(SHLIB_EXT)
	-rm -fr $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib
	-$(MAKE) -C $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER) clean

distclean-suitesparse: clean-suitesparse-wrapper
	-rm -rf $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz \
		$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)

get-suitesparse: $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz
extract-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/source-extracted
configure-suitesparse: extract-suitesparse
compile-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-compiled
fastcheck-suitesparse: #none
check-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/build-checked
install-suitesparse: $(build_prefix)/manifest/suitesparse install-suitesparse-wrapper

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC := -I /usr/include/suitesparse
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
