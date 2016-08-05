## ARPACK ##

ARPACK_FFLAGS := $(USE_BLAS_FFLAGS)
ARPACK_CFLAGS :=

ifeq ($(USE_BLAS64), 1)
ARPACK_CFLAGS += -DBLASINT=int64_t
ifneq ($(USEIFC),1)
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
ARPACK_FFLAGS += -cpp -ffixed-line-length-none
ARPACK_OPENBLASFCNS1 := axpy copy gemv geqr2 lacpy lahqr lanhs larnv lartg lascl laset scal trevc trmm trsen gbmv gbtrf gbtrs gttrf gttrs pttrf pttrs
ARPACK_OPENBLASFCNS2 := dot ger labad laev2 lamch lanst lanv2 lapy2 larf larfg lasr nrm2 orm2r rot steqr swap
ARPACK_OPENBLASFCNS3 := dotc geru unm2r
ARPACK_OPENBLASFCNS4 := COPY LABAD LAMCH LANHS LANV2 LARFG ROT GEMV
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS1) $(ARPACK_OPENBLASFCNS2), -Ds$(fcn)=s$(fcn)_64 -Dd$(fcn)=d$(fcn)_64)
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS1) $(ARPACK_OPENBLASFCNS3), -Dc$(fcn)=c$(fcn)_64 -Dz$(fcn)=z$(fcn)_64)
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS4), -DS$(fcn)=S$(fcn)_64 -DD$(fcn)=D$(fcn)_64)
ARPACK_FFLAGS += -Dscnrm2=scnrm2_64 -Ddznrm2=dznrm2_64 -Dcsscal=csscal_64 -Dzdscal=zdscal_64
# CFLAGS are for the configure checks
ARPACK_CFLAGS += -Dsgemm_=sgemm_64_ -Dcheev_=cheev_64_
endif
endif
endif
endif

ARPACK_MFLAGS := F77="$(FC)" MPIF77="$(FC)"
ARPACK_FFLAGS += $(FFLAGS) $(JFFLAGS)
ARPACK_FLAGS := --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" \
    --disable-mpi --enable-shared FFLAGS="$(ARPACK_FFLAGS)" \
    CFLAGS="$(CFLAGS) $(ARPACK_CFLAGS)" LDFLAGS="$(LDFLAGS) $(RPATH_ESCAPED_ORIGIN)"

# ARPACK-NG upstream keeps changing their download filenames
$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://s3.amazonaws.com/julialang/src/arpack-ng-$(ARPACK_VER).tar.gz
	touch -c $@
$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://raw.githubusercontent.com/opencollab/arpack-ng/$(ARPACK_VER)/TESTS/testA.mtx
	touch -c $@

$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/source-extracted: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	echo 1 > $@

ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-configured: | $(build_prefix)/manifest/openblas
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-configured: | $(build_prefix)/manifest/lapack
endif

$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/arpack-tests-blasint.patch-applied: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/source-extracted
	cd $(dir $@) && patch -p1 < $(SRCDIR)/patches/arpack-tests-blasint.patch
	echo 1 > $@

$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-configured: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/source-extracted $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/arpack-tests-blasint.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) $(ARPACK_FLAGS)
	echo 1 > $@

$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-compiled: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-configured
	$(MAKE) -C $(dir $<) $(ARPACK_MFLAGS)
	echo 1 > $@

$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-checked: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-compiled
	$(JLCHECKSUM) $<
	cp $< $(dir $@)/TESTS/testA.mtx
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check $(ARPACK_MFLAGS)
endif
	echo 1 > $@

$(build_prefix)/manifest/arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-compiled | $(build_shlibdir)
	$(call make-install,arpack-ng-$(ARPACK_VER),$(ARPACK_MFLAGS))
ifeq ($(OS), WINNT)
	mv $(build_shlibdir)/libarpack-2.dll $(build_shlibdir)/libarpack.$(SHLIB_EXT)
endif
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(build_shlibdir)/libarpack.$(SHLIB_EXT)
ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libarpack.so* ; do \
		[ -L $$filename ] || $(PATCHELF_BIN) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	echo $(ARPACK_VER) > $@

ifneq ($(PATCHELF),patchelf)
$(build_prefix)/manifest/arpack: $(PATCHELF)
endif

clean-arpack:
	-rm -f $(build_prefix)/manifest/arpack $(build_shlibdir)/libarpack.$(SHLIB_EXT)
	-$(MAKE) -C $(BUILDDIR)/arpack-ng-$(ARPACK_VER) clean

distclean-arpack:
	-rm -rf $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz \
		$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER) \
		$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx \
		$(BUILDDIR)/arpack-ng-$(ARPACK_VER)

get-arpack: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx
extract-arpack: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/source-extracted
configure-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-configured
compile-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-compiled
check-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/build-checked
install-arpack: $(build_prefix)/manifest/arpack
