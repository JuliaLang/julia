JULIAHOME = $(abspath ../../..)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/Versions.make

ifeq ($(OS), WINNT)
MATHEMATICABIN = MathKernel
else ifeq ($(OS), Darwin)
MATHEMATICABIN = MathKernel
else
MATHEMATICABIN = math
endif

#Which BLAS library am I using?
ifeq ($(USE_SYSTEM_BLAS), 0)
BLASDIR=$(JULIAHOME)/deps/openblas-$(OPENBLAS_VER)/
LIBBLAS=$(BLASDIR)libopenblas.a
endif

FFLAGS=-fexternal-blas
ifeq ($(findstring gfortran, $(FC)), gfortran)
ifeq ($(USE_BLAS64), 1)
echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
echo WARNING: gfortran cannot multiply matrices using 64-bit external BLAS.
echo          External BLAS usage is turned OFF.
echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FFLAGS=
endif
FFLAGS+= -static-libgfortran
endif

#Which libm library am I using?
LIBMDIR = $(JULIAHOME)/deps/openlibm/
ifeq ($(USE_SYSTEM_LIBM), 0)
ifeq ($(USE_SYSTEM_OPENLIBM), 0)
LIBM = $(LIBMDIR)libopenlibm.a
endif
endif

default: benchmarks.html

export OMP_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

perf.h: $(JULIAHOME)/deps/Versions.make
	echo '#include "$(BLASDIR)cblas.h"' > $@
	echo '#include "$(JULIAHOME)/deps/random/dsfmt-$(DSFMT_VER)/dSFMT.c"' >> $@

bin/perf%: perf.c perf.h
	$(CC) -std=c99 -O$* $< -o $@ -L$(BLASDIR) $(LIBBLAS) -L$(LIBMDIR) $(LIBM) -lpthread

bin/fperf%: perf.f90
	mkdir -p mods/$@ #Modules for each binary go in separate directories 
	$(FC) $(FFLAGS) -Jmods/$@ -O$* $< -o $@ -L$(BLASDIR) $(LIBBLAS) -L$(LIBMDIR) $(LIBM) -lpthread

benchmarks/c.csv: \
	benchmarks/c0.csv \
	benchmarks/c1.csv \
	benchmarks/c2.csv \
	benchmarks/c3.csv
	cat $^ > $@

benchmarks/fortran.csv: \
	benchmarks/fortran0.csv \
	benchmarks/fortran1.csv \
	benchmarks/fortran2.csv \
	benchmarks/fortran3.csv
	cat $^ > $@

benchmarks/c%.csv: bin/perf%
	for t in 1 2 3 4 5; do $<; done >$@

benchmarks/fortran%.csv: bin/fperf%
	for t in 1 2 3 4 5; do $<; done >$@

benchmarks/go.csv: perf.go
	for t in 1 2 3 4 5; do go run $<; done >$@

benchmarks/julia.csv: perf.jl
	for t in 1 2 3 4 5; do ../../../julia $<; done >$@

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

benchmarks/mathematica.csv: perf.nb
	for t in 1 2 3 4 5; do $(MATHEMATICABIN) -noprompt -run "<<$<; Exit[]"; done >$@

BENCHMARKS = \
	benchmarks/c.csv \
	benchmarks/fortran.csv \
	benchmarks/go.csv \
	benchmarks/julia.csv \
	benchmarks/python.csv \
	benchmarks/matlab.csv \
	benchmarks/octave.csv \
	benchmarks/r.csv \
	benchmarks/javascript.csv \
	benchmarks/mathematica.csv

benchmarks.csv: bin/collect.pl $(BENCHMARKS)
	@$(call PRINT_PERL, $^ >$@)

benchmarks.html: bin/table.pl benchmarks.csv
	@$(call PRINT_PERL, $^ >$@)

clean:
	@rm -rf perf.h bin/perf* bin/fperf* benchmarks/*.csv benchmarks.csv mods *~ octave-core

.PHONY: all perf clean
