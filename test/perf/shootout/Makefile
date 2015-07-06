JULIAHOME = $(abspath ../../..)
include ../../../Make.inc

SHOOTOUTFILES = knucleotide-input.txt regexdna-input.txt revcomp-input.txt fasta-output.txt mandelbrot-output.txt nbody-output.txt fannkuchredux-output.txt pidigits-output.txt knucleotide-output.txt revcomp-output.txt spectralnorm-output.txt regexdna-output.txt meteor-output.txt

getall: $(SHOOTOUTFILES)

%.txt:
	$(JLDOWNLOAD) http://benchmarksgame.alioth.debian.org/download/$@

clean:
	rm -f *.txt *~

.PHONY: getall
