JULIAHOME = ../..
include ../../Make.inc

PRIMARY = UTF-32BE
ENCODINGS = UTF-32BE UTF-32LE UTF-16BE UTF-16LE UTF-8

EXT = .unicode
DERIVED = $(filter-out $(PRIMARY),$(ENCODINGS))
DERIVED_FILES = $(addsuffix $(EXT),$(DERIVED))
CHECKS = $(addprefix check-,$(DERIVED_FILES))

all: $(DERIVED_FILES)

UTF-32BE$(EXT):
	$(QUIET_PERL) perl -e 'print pack "N*", 0xfeff, 0..0xd7ff, 0xe000..0x10ffff' >$@

$(DERIVED_FILES): %$(EXT): $(PRIMARY)$(EXT)
	$(QUIET_PERL) iconv -f $(PRIMARY) -t $* <$< >$@

check: $(CHECKS)

$(CHECKS): check-%$(EXT): %$(EXT)
	iconv -f $* -t $(PRIMARY) <$< | cmp $(PRIMARY)$(EXT)

clean:
	@rm -rf *$(EXT)

.PHONY: all check $(CHECKS) clean
