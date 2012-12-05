#!/usr/bin/make -f 

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: default

default:
	mkdir -p $(BUILD)/lib
	mkdir -p $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib
	cd $(JULIAHOME)/deps/SuiteSparse-SYSTEM/lib && \
	rm -f *.a && \
	cp -f $(shell find /lib /usr/lib /usr/local/lib $(shell eval $(JULIAHOME)/contrib/filterArgs.sh $(LDFLAGS)) -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libspqr.a 2>/dev/null) . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(BUILD)/lib/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(BUILD)/lib/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcholmod.$(SHLIB_EXT) $(LDFLAGS) -L$(BUILD)/lib -lcolamd -lamd $(LIBBLAS) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(BUILD)/lib/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libumfpack.$(SHLIB_EXT) $(LDFLAGS) -L$(BUILD)/lib -lcholmod -lcolamd -lamd $(LIBBLAS) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(BUILD)/lib/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libspqr.$(SHLIB_EXT) $(LDFLAGS) -L$(BUILD)/lib -lcholmod -lcolamd -lamd $(LIBBLAS) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(BUILD)/lib/libspqr.$(SHLIB_EXT)
