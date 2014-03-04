#!/usr/bin/make -f 

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: default

SS_LIB = $(shell dirname $(shell find $(shell eval $(JULIAHOME)/contrib/filterArgs.sh $(LDFLAGS)) /lib /usr/lib /usr/local/lib -name libsuitesparseconfig.a 2>/dev/null | head -n 1) 2>/dev/null)

ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)

$(build_libdir)/libgfortblas.dylib:
	make -C ../deps/ $(build_libdir)/libgfortblas.dylib

default: $(build_libdir)/libgfortblas.dylib
endif
endif
endif

default:
	mkdir -p $(build_libdir)
	mkdir -p $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib
	cd $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib && \
	rm -f $(build_libdir)/lib{amd,cholmod,colamd,spqr,umfpack}.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libamd.a $(NO_WHOLE_ARCHIVE) -o $(build_libdir)/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(build_libdir)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(build_libdir)/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(build_libdir)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(SS_LIB)/libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(build_libdir)/libcholmod.$(SHLIB_EXT) -L$(build_libdir) $(LDFLAGS) -lcolamd -lccolamd -lcamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(build_libdir)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(SS_LIB)/libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(build_libdir)/libumfpack.$(SHLIB_EXT) -L$(build_libdir) $(LDFLAGS) -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(build_libdir)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(SS_LIB)/libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(build_libdir)/libspqr.$(SHLIB_EXT) -L$(build_libdir) $(LDFLAGS) -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(build_libdir)/libspqr.$(SHLIB_EXT)
	
