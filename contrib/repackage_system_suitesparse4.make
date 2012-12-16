#!/usr/bin/make -f 

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: default

default:
	mkdir -p $(BUILD)/$(JL_LIBDIR)
	mkdir -p $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib
	cd $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib && \
	rm -f *.a && \
	cp -f $(shell find /lib /usr/lib /usr/local/lib $(shell eval $(JULIAHOME)/contrib/filterArgs.sh $(LDFLAGS)) -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libspqr.a -o -name libsuitesparseconfig.a 2>/dev/null) . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libcholmod.$(SHLIB_EXT) $(LDFLAGS) -L$(BUILD)/$(JL_LIBDIR) -L. -lcolamd -lccolamd -lcamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libumfpack.$(SHLIB_EXT) $(LDFLAGS) -L$(BUILD)/$(JL_LIBDIR) -L. -lcholmod -lcolamd -lcamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/$(JL_LIBDIR)/libspqr.$(SHLIB_EXT) $(LDFLAGS) -L$(BUILD)/$(JL_LIBDIR) -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(BUILD)/$(JL_LIBDIR)/libspqr.$(SHLIB_EXT)
	