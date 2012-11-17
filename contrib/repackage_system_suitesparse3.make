#!/usr/bin/make -f 

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: default

default:
	mkdir -p $(USRLIB)
	mkdir -p $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib
	cd $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib && \
	rm -f *.a && \
	cp -f $(shell find /lib /usr/lib /usr/local/lib $(shell eval $(JULIAHOME)/contrib/filterArgs.sh $(LDFLAGS)) -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libspqr.a 2>/dev/null) . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcholmod.$(SHLIB_EXT) $(LDFLAGS) -L$(USRLIB) -lcolamd -lamd $(LIBBLAS) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(USRLIB)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libumfpack.$(SHLIB_EXT) $(LDFLAGS) -L$(USRLIB) -lcholmod -lcolamd -lamd $(LIBBLAS) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(USRLIB)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libspqr.$(SHLIB_EXT) $(LDFLAGS) -L$(USRLIB) -lcholmod -lcolamd -lamd $(LIBBLAS) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(USRLIB)/libspqr.$(SHLIB_EXT)
