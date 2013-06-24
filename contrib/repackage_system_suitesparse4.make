#!/usr/bin/make -f 

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: default

SS_LIB = $(shell dirname $(shell find $(shell eval $(JULIAHOME)/contrib/filterArgs.sh $(LDFLAGS)) /lib /usr/lib /usr/local/lib -name libsuitesparseconfig.a 2>/dev/null | head -n 1))

default:
	mkdir -p $(BUILD)/$(JL_LIBDIR)
	mkdir -p $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib
	cd $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib && \
	rm -f $(BUILD)/$(JL_LIBDIR)/lib{amd,cholmod,colamd,spqr,umfpack}.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(SS_LIB)/libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libcholmod.$(SHLIB_EXT) -L$(BUILD)/$(JL_LIBDIR) $(LDFLAGS) -lcolamd -lccolamd -lcamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(SS_LIB)/libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libumfpack.$(SHLIB_EXT) -L$(BUILD)/$(JL_LIBDIR) $(LDFLAGS) -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) $(SS_LIB)/libsuitesparseconfig.a $(SS_LIB)/libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libspqr.$(SHLIB_EXT) -L$(BUILD)/$(JL_LIBDIR) $(LDFLAGS) -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libspqr.$(SHLIB_EXT)
	
