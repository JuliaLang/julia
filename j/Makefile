JULIAHOME = $(shell pwd)/..
include ../Make.inc

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

all: pcre_h.j

pcre_h.j:
	cpp -dM $(EXTROOT)/include/pcre.h | perl -nle '/^\s*#define\s+(PCRE\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "$$1 = $$2"' | sort > $@

clean:
	rm -f *# *~
	rm -f pcre_h.j
