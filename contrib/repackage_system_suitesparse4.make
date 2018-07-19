#!/usr/bin/make -f

JULIAHOME := $(abspath ..)
include $(JULIAHOME)/Make.inc

all: default

SS_LIB = $(shell dirname $(shell find $(shell eval $(JULIAHOME)/contrib/filterArgs.sh $(LDFLAGS)) /lib /usr/lib /usr/local/lib -name libsuitesparseconfig.a 2>/dev/null | head -n 1) 2>/dev/null)

ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)

$(build_private_libdir)/libgfortblas.dylib:
	mkdir -p $(build_private_libdir)
	make -C ../deps/ $@

default: $(build_private_libdir)/libgfortblas.dylib
endif
endif
endif

default:
	mkdir -p $(build_private_libdir)
	mkdir -p $(JULIAHOME)/deps/build/SuiteSparse-SYSTEM/lib
	cd $(JULIAHOME)/deps/build/SuiteSparse-SYSTEM/lib && \
	rm -f $(build_private_libdir)/lib{amd,camd,cholmod,ccolamd,colamd,spqr,umfpack}.$(SHLIB_EXT)
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libsuitesparseconfig.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libsuitesparseconfig.$(SHLIB_EXT) $(build_private_libdir)/libsuitesparseconfig.$(SHLIB_EXT)
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libamd.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_private_libdir) -lsuitesparseconfig $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(build_private_libdir)/libamd.$(SHLIB_EXT)
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libcamd.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libcamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_private_libdir) -lsuitesparseconfig $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libcamd.$(SHLIB_EXT) $(build_private_libdir)/libcamd.$(SHLIB_EXT)
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libccolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libccolamd.$(SHLIB_EXT) $(LDFLAGS) $(LIBBLAS) -L$(build_private_libdir) -lsuitesparseconfig $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libccolamd.$(SHLIB_EXT) $(build_private_libdir)/libccolamd.$(SHLIB_EXT)
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libcolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libcolamd.$(SHLIB_EXT) $(LDFLAGS) $(LIBBLAS) -L$(build_private_libdir) -lsuitesparseconfig $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(build_private_libdir)/libcolamd.$(SHLIB_EXT)
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libcholmod.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libcholmod.$(SHLIB_EXT) $(LDFLAGS) $(LIBBLAS) -L$(build_private_libdir) -lsuitesparseconfig -lcolamd -lccolamd -lcamd -lamd $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(build_private_libdir)/libcholmod.$(SHLIB_EXT)
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libumfpack.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libumfpack.$(SHLIB_EXT) $(LDFLAGS) $(LIBBLAS) -L$(build_private_libdir) -lsuitesparseconfig -lcolamd -lccolamd -lcamd -lamd -lcholmod $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(build_private_libdir)/libumfpack.$(SHLIB_EXT)
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libspqr.a $(NO_WHOLE_ARCHIVE) -o $(build_private_libdir)/libspqr.$(SHLIB_EXT) $(LDFLAGS) $(LIBBLAS) -L$(build_private_libdir) -lsuitesparseconfig -lcolamd -lccolamd -lcamd -lamd -lcholmod $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(build_private_libdir)/libspqr.$(SHLIB_EXT)

