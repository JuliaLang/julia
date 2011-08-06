JULIAHOME = $(shell pwd)/..
include ../Make.inc

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+
COMMITVER=$(shell git log -1 | head -n1 | cut -b8-47)
COMMITDATE=$(shell git log -1 | head -n3 | tail -n1 | cut -b 9-50)

all: pcre_h.j version.j

pcre_h.j:
	cpp -dM $(EXTROOT)/include/pcre.h | perl -nle '/^\s*#define\s+(PCRE\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "$$1 = $$2"' | sort > $@

version.j:
	echo "version() = (0.5, \"$(COMMITVER)\", \"$(COMMITDATE)\")" > version.j

clean:
	rm -f pcre_h.j version.j
