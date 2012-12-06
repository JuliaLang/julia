JULIAHOME = ../..
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/Versions.make

default: benchmarks.html

export OMP_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

perf.h:
	echo '#include "$(JULIAHOME)/deps/openblas-$(OPENBLAS_VER)/cblas.h"' > $@
	echo '#include "$(JULIAHOME)/deps/random/dsfmt-$(DSFMT_VER)/dSFMT.c"' >> $@

CC99 = $(CC) -std=c99
CXX11 = $(CXX) -std=c++11

bin/pperf%: perf.cpp
	$(CXX11) -O$* $< -o $@

bin/perf%: perf.c perf.h
	$(CC99) -O$* $< -o $@ $(JULIAHOME)/deps/openblas-$(OPENBLAS_VER)/libopenblas.a -lpthread -lm

bin/fperf%: perf.f90
	$(FC) -static-libgfortran -O$* -fexternal-blas $< -o $@ $(JULIAHOME)/deps/openblas-$(OPENBLAS_VER)/libopenblas.a -lpthread

benchmarks/c.csv: \
	benchmarks/c0.csv \
	benchmarks/c1.csv \
	benchmarks/c2.csv \
	benchmarks/c3.csv
	cat $^ > $@

benchmarks/cpp.csv: \
	benchmarks/p0.csv \
	benchmarks/p1.csv \
	benchmarks/p2.csv \
	benchmarks/p3.csv
	cat $^ > $@

benchmarks/fortran.csv: \
	benchmarks/fortran0.csv \
	benchmarks/fortran1.csv \
	benchmarks/fortran2.csv \
	benchmarks/fortran3.csv
	cat $^ > $@

benchmarks/c%.csv: bin/perf%
	for t in 1 2 3 4 5; do $<; done >$@

benchmarks/p%.csv: bin/pperf%
	for t in 1 2 3 4 5; do $<; done >$@

benchmarks/fortran%.csv: bin/fperf%
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
	benchmarks/cpp.csv \
	benchmarks/fortran.csv \
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
	@rm -rf bin/perf* bin/pperf* bin/fperf* benchmarks/*.csv benchmarks.csv *.mod

.PHONY: all perf clean
