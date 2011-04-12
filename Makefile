JULIAHOME = $(shell pwd)
include ./Make.inc

default: debug

julia-debug-build:
	cd src && make debug

julia-release-build:
	cd src && make release

sys.ji: sysimg.j start_image.j src/boot.j src/dump.c *.j
	./julia -b sysimg.j

custom.j:
	if [ ! -f custom.j ]; then touch custom.j; fi

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

pcre_h.j:
	cpp -dM $(EXTROOT)/include/pcre.h | perl -nle '/^\s*#define\s+(PCRE\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "$$1 = $$2"' | sort > $@

debug release: %: julia-%-build pcre_h.j sys.ji custom.j

test: debug
	./julia tests.j

testall: test
	./julia test_utf8.j

clean:
	$(MAKE) -C src clean
	rm -f *.ji
	rm -f pcre_h.j
	rm -f *~ *#

cleanall: clean
	rm -f nbits
	$(MAKE) -C src cleanother

.PHONY: debug release test testall clean cleanall
