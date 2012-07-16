JULIAHOME = ../..
include ../../Make.inc

PRIMARY = UTF-32LE
ENCODINGS = UTF-32LE UTF-16BE UTF-16LE UTF-8

DERIVED = $(filter-out $(PRIMARY),$(ENCODINGS))
DERIVED_FILES = $(addsuffix .txt,$(DERIVED))
CHECKS = $(addprefix check-,$(DERIVED_FILES))

all: $(DERIVED_FILES)

UTF-32LE.txt:
	$(QUIET_PERL) perl -e 'print pack "V*", 0xfeff, 0..0xd7ff, 0xe000..0x10ffff' >$@

$(DERIVED_FILES): %.txt: $(PRIMARY).txt
	$(QUIET_PERL) iconv -f $(PRIMARY) -t $* <$< >$@

check: $(CHECKS)

$(CHECKS): check-%.txt: %.txt
	iconv -f $* -t $(PRIMARY) <$< | cmp $(PRIMARY).txt

clean:
	@rm -rf *.txt

.PHONY: all check $(CHECKS) clean
