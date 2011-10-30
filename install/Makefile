JULIAHOME = $(pwd)/..
include ../Make.inc

LIBS = ../lib/libarpack.$(SHLIB_EXT) ../lib/libfdm.$(SHLIB_EXT) ../lib/libfftw3.$(SHLIB_EXT) ../lib/libfftw3f.$(SHLIB_EXT) ../lib/libpcre.$(SHLIB_EXT) ../lib/libpcrecpp.$(SHLIB_EXT) ../lib/libpcreposix.$(SHLIB_EXT) ../lib/librandom.$(SHLIB_EXT) ../lib/libLAPACK.$(SHLIB_EXT)

JL_DIR = julia

all: clean
	mkdir -p $(JL_DIR)/lib
	cp ../julia-release-readline $(JL_DIR)/julia
	cp ../julia-release-basic $(JL_DIR)/julia-no-readline
	cp $(LIBS) $(JL_DIR)/lib/
	cp -r ../j $(JL_DIR)/
	cp -r ../contrib $(JL_DIR)/
	cp -r ../examples $(JL_DIR)/
	cp -r ../sys.ji $(JL_DIR)/
	tar zcvf julia-$(JULIA_COMMIT)-$(OS)-$(ARCH).tar.gz $(JL_DIR)

clean:
	rm -rf julia*
