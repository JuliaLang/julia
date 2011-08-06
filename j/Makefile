JULIAHOME = $(shell pwd)/..
include ../Make.inc

PCRE_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

JULIA_VERSION=$(shell cat ../VERSION)
COMMIT_HASH=$(shell git log -1 --pretty=format:%H)
COMMIT_DATE=$(shell git log -1 --pretty=format:%ct | xargs perl -MPOSIX=strftime -le 'print strftime "%F %T", gmtime $$ARGV[0]')

all: pcre_h.j version.j

pcre_h.j:
	cpp -dM $(EXTROOT)/include/pcre.h | perl -nle '/^\s*#define\s+(PCRE\w*)\s*\(?($(PCRE_CONST))\)?\s*$$/ and print "$$1 = $$2"' | sort > $@

version.j:
	echo "VERSION = (\"$(JULIA_VERSION)\", \"$(COMMIT_HASH)\", \"$(COMMIT_DATE)\")" > version.j

clean:
	rm -f pcre_h.j version.j
