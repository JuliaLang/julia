ENCODINGS = UTF-32BE UTF-32LE UTF-16BE UTF-16LE UTF-8

PRIMARY = UTF-32BE
DERIVED = $(filter-out $(PRIMARY),$(ENCODINGS))
DERIVED_FILES = $(addsuffix .txt,$(DERIVED))

all: $(DERIVED_FILES)

UTF-32BE.txt:
	perl -e 'print pack "N", 0xfeff; \
	         print pack "N", $$_ for 0..0xd7ff, 0xe000..0x10ffff' >$@

$(DERIVED_FILES): %.txt: $(PRIMARY).txt
	iconv -f $(PRIMARY) -t $* <$< >$@

clean:
	rm -rf *.txt
