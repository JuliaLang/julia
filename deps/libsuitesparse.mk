## LIBSUITESPARSE ##
include $(SRCDIR)/libsuitesparse.version

ifneq ($(USE_BINARYBUILDER_LIBSUITESPARSE), 1)

LIBSUITESPARSE_PROJECTS := AMD BTF CAMD CCOLAMD COLAMD CHOLMOD LDL KLU UMFPACK RBio SPQR
LIBSUITESPARSE_LIBS := $(addsuffix .*$(SHLIB_EXT)*,suitesparseconfig amd btf camd ccolamd colamd cholmod klu ldl umfpack rbio spqr)

LIBSUITESPARSE_CMAKE_FLAGS := $(CMAKE_COMMON) \
	  -DCMAKE_BUILD_TYPE=Release \
	  -DENABLE_CUDA=0 \
	  -DNFORTRAN=1 \
	  -DNOPENMP=1 \
	  -DNPARTITION=0 \
	  -DNSTATIC=1 \
	  -DBLAS_FOUND=1 \
	  -DBLAS_LIBRARIES="$(build_shlibdir)/libblastrampoline.$(SHLIB_EXT)" \
	  -DBLAS_LINKER_FLAGS="blastrampoline" \
	  -DBLAS_UNDERSCORE=ON \
	  -DBLA_VENDOR="blastrampoline" \
	  -DBLAS64_SUFFIX="_64" \
	  -DALLOW_64BIT_BLAS=ON \
	  -DLAPACK_FOUND=1 \
	  -DLAPACK_LIBRARIES="$(build_shlibdir)/libblastrampoline.$(SHLIB_EXT)" \
	  -DLAPACK_LINKER_FLAGS="blastrampoline"

$(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/DrTimothyAldenDavis/SuiteSparse/archive/v$(LIBSUITESPARSE_VER).tar.gz

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted: $(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -zxf $<
	echo 1 > $@

checksum-libsuitesparse: $(SRCCACHE)/SuiteSparse-$(LIBSUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: | $(build_prefix)/manifest/blastrampoline

$(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/build-compiled: $(BUILDDIR)/SuiteSparse-$(LIBSUITESPARSE_VER)/source-extracted
	cd $(dir $<); \
	for PROJ in SuiteSparse_config $(LIBSUITESPARSE_PROJECTS); do \
		cd $${PROJ}/build || exit 1; \
		$(CMAKE) .. $(LIBSUITESPARSE_CMAKE_FLAGS) || exit 1; \
		make || exit 1; \
		make install || exit 1; \
		cd ../..; \
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
