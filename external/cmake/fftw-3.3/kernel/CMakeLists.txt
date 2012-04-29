########### next target ###############

set(kernel_STAT_SRCS
    align.c
    alloc.c
    assert.c
    awake.c
    buffered.c
    cpy1d.c
    cpy2d-pair.c
    cpy2d.c
    ct.c
    debug.c
    extract-reim.c
    hash.c
    iabs.c
    kalloc.c
    md5-1.c
    md5.c
    minmax.c
    ops.c
    pickdim.c
    plan.c
    planner.c
    primes.c
    print.c
    problem.c
    rader.c
    scan.c
    solver.c
    solvtab.c
    stride.c
    tensor.c
    tensor1.c
    tensor2.c
    tensor3.c
    tensor4.c
    tensor5.c
    tensor7.c
    tensor8.c
    tensor9.c
    tile2d.c
    timer.c
    transpose.c
    trig.c
    twiddle.c
    cycle.h
    ifftw.h)

include_directories("${top_srcdir}/simd")
addObjects(kernel ${kernel_STAT_SRCS})



########### install files ###############




#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/simd
#noinst_LTLIBRARIES = libkernel.la
#
## pkgincludedir = $(includedir)/fftw3@PREC_SUFFIX@
## pkginclude_HEADERS = ifftw.h cycle.h
#
#libkernel_la_SOURCES = align.c alloc.c assert.c awake.c buffered.c	\
#cpy1d.c cpy2d-pair.c cpy2d.c ct.c debug.c extract-reim.c hash.c iabs.c	\
#kalloc.c md5-1.c md5.c minmax.c ops.c pickdim.c plan.c planner.c	\
#primes.c print.c problem.c rader.c scan.c solver.c solvtab.c stride.c	\
#tensor.c tensor1.c tensor2.c tensor3.c tensor4.c tensor5.c tensor7.c	\
#tensor8.c tensor9.c tile2d.c timer.c transpose.c trig.c twiddle.c	\
#cycle.h ifftw.h
