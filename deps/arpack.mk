## ARPACK ##
ARPACK_FFLAGS := $(GFORTBLAS_FFLAGS)
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

ifeq ($(OS),WINNT)
ARPACK_OBJ_SOURCE := $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/.libs/libarpack-2.$(SHLIB_EXT)
else
ARPACK_OBJ_SOURCE := $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/.libs/libarpack.$(SHLIB_EXT)
endif
ARPACK_OBJ_TARGET := $(build_shlibdir)/libarpack.$(SHLIB_EXT)

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
$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/configure: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $@

ifeq ($(USE_ATLAS), 1)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: | $(LAPACK_OBJ_TARGET)
endif

$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/arpack-tests-blasint.patch-applied: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/configure
	cd $(dir $@) && patch -p1 < $(SRCDIR)/patches/arpack-tests-blasint.patch
	echo 1 > $@
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/configure $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/arpack-tests-blasint.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) $(ARPACK_FLAGS)
	touch -c $@
$(ARPACK_OBJ_SOURCE): $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status
	$(MAKE) -C $(dir $<) $(ARPACK_MFLAGS)
	touch -c $@
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/checked: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx $(ARPACK_OBJ_SOURCE)
	$(JLCHECKSUM) $<
	cp $< $(dir $@)/TESTS/testA.mtx
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check $(ARPACK_MFLAGS)
endif
	echo 1 > $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE) | $(build_shlibdir)
	$(call make-install,arpack-ng-$(ARPACK_VER),$(ARPACK_MFLAGS))
ifeq ($(OS), WINNT)
	mv $(build_shlibdir)/libarpack-2.dll $@
endif
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(build_shlibdir)/libarpack.$(SHLIB_EXT)
ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libarpack.so* ; do \
		[ -L $$filename ] || $(PATCHELF_BIN) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@
ifneq ($(PATCHELF),patchelf)
$(ARPACK_OBJ_TARGET): $(PATCHELF)
endif

clean-arpack:
	-$(MAKE) -C $(BUILDDIR)/arpack-ng-$(ARPACK_VER) clean
	-rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	-rm -rf $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz \
		$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER) \
		$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx \
		$(BUILDDIR)/arpack-ng-$(ARPACK_VER)

get-arpack: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx
configure-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status
compile-arpack: $(ARPACK_OBJ_SOURCE)
check-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/checked
install-arpack: $(ARPACK_OBJ_TARGET)
