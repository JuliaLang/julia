JULIAHOME = ../..
include $(JULIAHOME)/Make.inc

default: benchmarks.html

bin/perf%: perf.cpp
	$(CXX) -O$* $< -o $@ $(JULIAHOME)/deps/openblas-v0.1.0/libopenblas.a

benchmarks/c.csv: \
	benchmarks/c0.csv \
	benchmarks/c1.csv \
	benchmarks/c2.csv \
	benchmarks/c3.csv
	cat $^ > $@

benchmarks/c%.csv: bin/perf%
	for t in 1 2 3 4 5; do $<; done >$@

benchmarks/julia.csv: perf.jl
	for t in 1 2 3 4 5; do ../../julia $<; done >$@

benchmarks/python.csv: perf.py
	for t in 1 2 3 4 5; do python $<; done >$@

benchmarks/matlab.csv: perf.m
	for t in 1 2 3 4 5; do matlab -nosplash -nodesktop -nojvm -r 'perf;exit' 2>/dev/null | grep '^matlab,'; done >$@

benchmarks/octave.csv: perf.m
	for t in 1 2 3 4 5; do octave -q --eval perf 2>/dev/null; done >$@

benchmarks/r.csv: perf.R
	for t in 1 2 3 4 5; do cat $< | R --vanilla --slave 2>/dev/null; done >$@

benchmarks/javascript.csv: perf.js
	for t in 1 2 3 4 5; do node $<; done >$@

BENCHMARKS = \
	benchmarks/c.csv \
	benchmarks/julia.csv \
	benchmarks/python.csv \
	benchmarks/matlab.csv \
	benchmarks/octave.csv \
	benchmarks/r.csv \
	benchmarks/javascript.csv

benchmarks.csv: bin/collect.pl $(BENCHMARKS)
	$(QUIET_PERL) $^ >$@

benchmarks.html: bin/table.pl benchmarks.csv
	$(QUIET_PERL) $^ >$@

clean:
	@rm -rf bin/perf* benchmarks/*.csv benchmarks.csv

.PHONY: all perf clean
