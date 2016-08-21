## OpenBLAS ##
# LAPACK is built into OpenBLAS by default
OPENBLAS_GIT_URL := git://github.com/xianyi/OpenBLAS.git
OPENBLAS_TAR_URL = https://api.github.com/repos/xianyi/OpenBLAS/tarball/$1
$(eval $(call git-external,openblas,OPENBLAS,Makefile,$(LIBBLASNAME).$(SHLIB_EXT),$(BUILDDIR)))

OPENBLAS_OBJ_SOURCE := $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/$(LIBBLASNAME).$(SHLIB_EXT)
OPENBLAS_OBJ_TARGET := $(build_shlibdir)/$(LIBBLASNAME).$(SHLIB_EXT)
OPENBLAS_BUILD_OPTS := CC="$(CC)" FC="$(FC)" RANLIB="$(RANLIB)" FFLAGS="$(FFLAGS) $(JFFLAGS)" TARGET=$(OPENBLAS_TARGET_ARCH) BINARY=$(BINARY)

# Thread support
ifeq ($(OPENBLAS_USE_THREAD), 1)
OPENBLAS_BUILD_OPTS += USE_THREAD=1
OPENBLAS_BUILD_OPTS += GEMM_MULTITHREADING_THRESHOLD=50
# Maximum number of threads for parallelism
ifneq ($(ARCH),x86_64)
# Assume we can't address much memory to spawn many threads
# It is also unlikely that 32-bit architectures have too many cores
OPENBLAS_BUILD_OPTS += NUM_THREADS=8
else ifeq ($(OS),WINNT)
# Windows seems unable to handle very many
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
else ifeq ($(OS),Darwin)
# This should suffice for the largest macs
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
else
# On linux, try to provision for the largest possible machine currently
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
endif
else
OPENBLAS_BUILD_OPTS += USE_THREAD=0
endif

# don't touch scheduler affinity since we manage this ourselves
OPENBLAS_BUILD_OPTS += NO_AFFINITY=1

# Build for all architectures - required for distribution
ifeq ($(OPENBLAS_DYNAMIC_ARCH), 1)
OPENBLAS_BUILD_OPTS += DYNAMIC_ARCH=1
endif

# 64-bit BLAS interface
ifeq ($(USE_BLAS64), 1)
OPENBLAS_BUILD_OPTS += INTERFACE64=1 SYMBOLSUFFIX="$(OPENBLAS_SYMBOLSUFFIX)" LIBPREFIX="$(LIBBLASNAME)"
ifeq ($(OS), Darwin)
OPENBLAS_BUILD_OPTS += OBJCONV=$(abspath $(BUILDDIR)/objconv/objconv)
$(OPENBLAS_OBJ_SOURCE): $(OBJCONV_SOURCE)
endif
endif

# Decide whether to build for 32-bit or 64-bit arch
ifneq ($(BUILD_OS),$(OS))
OPENBLAS_BUILD_OPTS += OSNAME=$(OS) CROSS=1 HOSTCC=$(HOSTCC)
endif
ifeq ($(OS),WINNT)
ifneq ($(ARCH),x86_64)
ifneq ($(USECLANG),1)
OPENBLAS_BUILD_OPTS += CFLAGS="$(CFLAGS) -mincoming-stack-boundary=2"
endif
OPENBLAS_BUILD_OPTS += FFLAGS="$(FFLAGS) -mincoming-stack-boundary=2"
endif
endif

# Debug OpenBLAS
ifeq ($(OPENBLAS_DEBUG), 1)
OPENBLAS_BUILD_OPTS += DEBUG=1
endif

# Allow disabling AVX for older binutils
ifeq ($(OPENBLAS_NO_AVX), 1)
OPENBLAS_BUILD_OPTS += NO_AVX=1 NO_AVX2=1
else ifeq ($(OPENBLAS_NO_AVX2), 1)
OPENBLAS_BUILD_OPTS += NO_AVX2=1
endif

# Do not overwrite the "-j" flag
OPENBLAS_BUILD_OPTS += MAKE_NB_JOBS=0

$(BUILDDIR)/$(OPENBLAS_SRC_DIR)/config.status: $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/Makefile
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $<.system
	touch $@
$(OPENBLAS_OBJ_SOURCE): $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/config.status
	echo $(MAKE) -C $(dir $<) $(OPENBLAS_BUILD_OPTS) # echo first, so we only print the error message below in a failure case
	@$(MAKE) -C $(dir $<) $(OPENBLAS_BUILD_OPTS) || (echo $(WARNCOLOR)"*** Clean the OpenBLAS build with 'make -C deps clean-openblas'. Rebuild with 'make OPENBLAS_USE_THREAD=0' if OpenBLAS had trouble linking libpthread.so, and with 'make OPENBLAS_TARGET_ARCH=NEHALEM' if there were errors building SandyBridge support. Both these options can also be used simultaneously. ***"$(ENDCOLOR) && false)
	touch -c $@
ifneq ($(USE_SYSTEM_BLAS),1)
$(OPENBLAS_OBJ_TARGET): $(OPENBLAS_OBJ_SOURCE) | $(build_shlibdir)
	cp -f $< $@
ifeq ($(OS), Linux)
	cd $(dir $@) && \
	ln -sf $(LIBBLASNAME).$(SHLIB_EXT) $(LIBBLASNAME).$(SHLIB_EXT).0
endif
	$(INSTALL_NAME_CMD)$(LIBBLASNAME).$(SHLIB_EXT) $@
endif

clean-openblas:
	-$(MAKE) -C $(BUILDDIR)/$(OPENBLAS_SRC_DIR) clean

get-openblas: $(OPENBLAS_SRC_FILE)
configure-openblas: $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/config.status
compile-openblas: $(OPENBLAS_OBJ_SOURCE)
check-openblas: compile-openblas
install-openblas: $(OPENBLAS_OBJ_TARGET)


## ATLAS (currently 3.10.0) ##

# no threading, with full lapack, shared library
# should always be compiled with (a real) gcc, it's
# configure script will search for the best match
# (gcc 4.7, gcc, clang,ICC/microsoft/others)
ATLAS_OBJ_SOURCE := $(BUILDDIR)/atlas/build/lib/libsatlas.$(SHLIB_EXT)
ATLAS_OBJ_TARGET := $(build_shlibdir)/libsatlas.$(SHLIB_EXT)
ATLAS_FLAGS := --shared --prefix=$(build_prefix) --cc=gcc -t 0 \
	--with-netlib-lapack-tarfile=$(JULIAHOME)/deps/lapack-$(LAPACK_VER).tgz
ifeq ($(OS), WINNT)
ATLAS_FLAGS += -b 32
endif

#force backwards compatibility (pick any 1)
#ATLAS_FLAGS += -V 192 -A 13  # requires SSE2 (P4 & later)
#ATLAS_FLAGS += -V 128 -A 12 # requires SSE1 (P3 & later)
#ATLAS_FLAGS += -V -1 -A 11 # any x87 (PentiumPro or Athlon & later)
#ATLAS_FLAGS += -A 25  # requires Corei132 (Corei232 doesn't have definition yet)

$(SRCDIR)/srccache/atlas/configure:
	git clone git://github.com/vtjnash/atlas-3.10.0.git $(SRCDIR)/srccache/atlas
ifeq "$(MAKECMDGOALS)" "compile-atlas"
# only allow building atlas as the sole target (without -jN)
# since it internally handles parallelism, for tuning timing accuracy
$(BUILDDIR)/atlas/Make.top: $(SRCDIR)/srccache/atlas/configure $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(ATLAS_FLAGS)
	touch -c $@
$(ATLAS_OBJ_SOURCE): $(BUILDDIR)/atlas/Make.top
	$(MAKE) -C $(dir $<) -j1
	touch -c $@
else
$(ATLAS_OBJ_SOURCE):
	$(error cannot build atlas in parallel with anything else)
endif

$(ATLAS_OBJ_TARGET): $(ATLAS_OBJ_SOURCE)
	cp -f $(ATLAS_OBJ_SOURCE) $@
	$(INSTALL_NAME_CMD)libsatlas.$(SHLIB_EXT) $@
	touch -c $@

clean-atlas:
	rm -rf $(BUILDDIR)/atlas/build
distclean-atlas:
	rm -rf $(BUILDDIR)/atlas

get-atlas: $(SRCDIR)/srccache/atlas/configure $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
configure-atlas: $(BUILDDIR)/atlas/Make.top
compile-atlas: $(ATLAS_OBJ_SOURCE)
check-atlas: compile-atlas
install-atlas: $(ATLAS_OBJ_TARGET)

## Mac gfortran BLAS wrapper ##
GFORTBLAS_FFLAGS :=
ifeq ($(USE_BLAS64), 1)
ifeq ($(USEIFC),1)
GFORTBLAS_FFLAGS += -i8
else
GFORTBLAS_FFLAGS += -fdefault-integer-8
endif
endif
ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
GFORTBLAS_FFLAGS += -cpp -ffree-line-length-0 -ffixed-line-length-0 \
			    -Dsasum=sasum_gfort -Dscasum=scasum_gfort \
				-Dscnrm2=scnrm2_gfort -Dsdot=sdot_gfort \
				-Dsdsdot=sdsdot_gfort -Dsnrm2=snrm2_gfort \
				-Dcdotc=cdotc_gfort -Dcdotu=cdotu_gfort \
				-Dzdotc=zdotc_gfort -Dzdotu=zdotu_gfort \
				\
			    -DSASUM=SASUM_GFORT -DSCASUM=SCASUM_GFORT \
				-DSCNRM2=SCNRM2_GFORT -DSDOT=SDOT_GFORT \
				-DSDSDOT=SDSDOT_GFORT -DSNRM2=SNRM2_GFORT \
				-DCDOTC=CDOTC_GFORT -DCDOTU=CDOTU_GFORT \
				-DZDOTC=ZDOTC_GFORT -DZDOTU=ZDOTU_GFORT
endif
endif

$(BUILDDIR)/libgfortblas.$(SHLIB_EXT): $(SRCDIR)/gfortblas.c $(SRCDIR)/gfortblas.alias
	$(CC) -Wall -O3 $(CPPFLAGS) $(CFLAGS) $(fPIC) -shared $< -o $@ -pipe \
				-Wl,-reexport_framework,Accelerate -Wl,-alias_list,$(SRCDIR)/gfortblas.alias
$(build_shlibdir)/libgfortblas.$(SHLIB_EXT): $(BUILDDIR)/libgfortblas.$(SHLIB_EXT)
	cp -f $< $@
	$(INSTALL_NAME_CMD)libgfortblas.$(SHLIB_EXT) $@
endif

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET := $(build_shlibdir)/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE := $(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET :=
LAPACK_OBJ_SOURCE :=
endif

LAPACK_MFLAGS := NOOPT="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS) -O0" \
    OPTS="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS)" FORTRAN="$(FC)" \
    LOADER="$(FC)" BLASLIB="$(RPATH_ESCAPED_ORIGIN) $(LIBBLAS)"

$(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.netlib.org/lapack/$(notdir $@)
$(BUILDDIR)/lapack-$(LAPACK_VER)/make.inc: $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)
	cd $(BUILDDIR) && $(TAR) zxf $<
	cp $(dir $@)INSTALL/make.inc.gfortran $(dir $@)make.inc
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(OS),Darwin)
$(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a: | $(build_shlibdir)/libgfortblas.$(SHLIB_EXT)
endif
$(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a: $(BUILDDIR)/lapack-$(LAPACK_VER)/make.inc
	$(MAKE) -C $(dir $@) lapacklib $(LAPACK_MFLAGS)
	touch -c $@
$(BUILDDIR)/lapack-$(LAPACK_VER)/checked: $(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a
ifeq ($(BUILD_OS),$(OS))
	$(MAKE) -C $(dir $@) lapack_testing $(LAPACK_MFLAGS) -k
endif
	touch $@
$(LAPACK_OBJ_SOURCE): $(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a
	$(FC) -shared $(FFLAGS) $(JFFLAGS) $(dir $<)/SRC/*.o $(dir $<)/INSTALL/dlamch.o $(dir $<)/INSTALL/dsecnd_INT_ETIME.o $(dir $<)/INSTALL/ilaver.o $(dir $<)/INSTALL/slamch.o $(LIBBLAS) -o $@
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	cp $< $@
	$(INSTALL_NAME_CMD)liblapack.$(SHLIB_EXT) $@

clean-lapack:
	-$(MAKE) -C $(BUILDDIR)/lapack-$(LAPACK_VER) clean
	-rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	-rm -rf $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz $(BUILDDIR)/lapack-$(LAPACK_VER)

get-lapack: $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
configure-lapack: $(BUILDDIR)/lapack-$(LAPACK_VER)/make.inc
compile-lapack: $(LAPACK_OBJ_SOURCE)
check-lapack: $(BUILDDIR)/lapack-$(LAPACK_VER)/checked
install-lapack: $(LAPACK_OBJ_TARGET)
