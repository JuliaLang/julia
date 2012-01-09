JULIAHOME = ../..
include $(JULIAHOME)/Make.inc

benchmark: benchmarks.txt

bin/perf%: perf.cpp
	$(CXX) -O$* $(JULIAHOME)/external/openblas-v0.1alpha2.4/libopenblas.a $< -o $@

benchmarks/c.csv: bin/perf0 bin/perf1 bin/perf2 bin/perf3
	rm -f $@
	mkdir -p benchmarks
	for t in 1 2 3 4 5; do bin/perf0; done >>$@
	for t in 1 2 3 4 5; do bin/perf1; done >>$@
	for t in 1 2 3 4 5; do bin/perf2; done >>$@
	for t in 1 2 3 4 5; do bin/perf3; done >>$@

benchmarks/julia.csv: perf.j
	../../julia $< >$@

benchmarks/python.csv: perf.py
	python $< >$@

benchmarks/matlab.csv: perf.m
	matlab -nosplash -nodesktop -nojvm -r 'perf;exit' 2>/dev/null | grep '^matlab,' >$@

benchmarks/octave.csv: perf.m
	octave -q --eval perf 2>/dev/null >$@

benchmarks/r.csv: perf.R
	cat $< | R --vanilla --slave 2>/dev/null >$@

benchmarks/javascript.csv: perf.js
	node $< >$@

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

benchmarks.txt: bin/table.pl benchmarks.csv
	$(QUIET_PERL) $^ >$@

clean:
	@rm -rf bin/perf* benchmarks benchmarks.csv

.PHONY: all perf clean
