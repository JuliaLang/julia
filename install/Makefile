include ../Make.inc

LIBS = ../lib/libarpack.$(SHLIB_EXT) ../lib/libfdm.$(SHLIB_EXT) ../lib/libfftw3.$(SHLIB_EXT) ../lib/libfftw3f.$(SHLIB_EXT) ../lib/libpcre.$(SHLIB_EXT) ../lib/libpcrecpp.$(SHLIB_EXT) ../lib/libpcreposix.$(SHLIB_EXT) ../lib/librandom.$(SHLIB_EXT) ../lib/libLAPACK.$(SHLIB_EXT)

all: package-$(OS)

package-$(OS): clean
	mkdir -p package/julia-$(OS) && \
	mkdir -p package/julia-$(OS)/lib && \
	cp ../julia-release-readline ../julia-release-basic package/julia-$(OS) && \
	cp $(LIBS) package/julia-$(OS)/lib && \
	cp -r ../j package/julia-$(OS) && \
	cp -r ../contrib package/julia-$(OS) && \
	cp -r ../examples package/julia-$(OS) && \
	cp -r ../sys.ji package/julia-$(OS) && \
	cd package && tar zcvf julia-$(OS).tar.gz julia-$(OS) && mv *.tar.gz ..

pkg:
	/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker -d julia-Darwin.pmdoc -o julia-Darwin.pkg

clean:
	rm -fr package *.pkg *.tar.gz
