## SUITESPARSE ##

SUITESPARSE_OBJ_SOURCE := $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
SUITESPARSE_OBJ_TARGET := $(build_shlibdir)/libspqr.$(SHLIB_EXT)

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
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile: $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -zxf $<
	touch -c $@

$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-winclang.patch-applied: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd $(dir $@) && patch -p0 < $(SRCDIR)/patches/SuiteSparse-winclang.patch
	echo 1 > $@

ifeq ($(USE_ATLAS), 1)
$(SUITESPARSE_OBJ_SOURCE): | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(LAPACK_OBJ_TARGET)
endif
$(SUITESPARSE_OBJ_SOURCE): $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse-winclang.patch-applied
	$(MAKE) -C $(dir $<) library $(SUITESPARSE_MFLAGS)
	touch -c $@
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/checked: $(SUITESPARSE_OBJ_SOURCE)
	$(MAKE) -C $(dir $@) default $(SUITESPARSE_MFLAGS)
	touch $@
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
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

clean-suitesparse:
	-$(MAKE) -C $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER) clean
	-rm -fr $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib
	-rm -f $(SUITESPARSE_OBJ_TARGET)
distclean-suitesparse:
	-rm -rf $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz \
		$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)

get-suitesparse: $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz
configure-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile
compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
check-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/checked
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) install-suitesparse-wrapper

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC := -I /usr/include/suitesparse
SUITESPARSE_LIB := -lumfpack -lcholmod -lamd -lcamd -lcolamd -lspqr
else
SUITESPARSE_INC := -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse_config -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SPQR/Include
SUITESPARSE_LIB := -L$(build_shlibdir) -lcholmod -lumfpack -lspqr $(RPATH_ORIGIN)
$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT):  $(SUITESPARSE_OBJ_TARGET)
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
configure-suitesparse-wrapper:
compile-suitesparse-wrapper:
check-suitesparse-wrapper:
install-suitesparse-wrapper: $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)
