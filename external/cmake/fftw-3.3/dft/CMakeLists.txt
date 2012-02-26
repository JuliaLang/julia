add_subdirectory(scalar)
add_subdirectory(simd)


########### next target ###############

set(dft_STAT_SRCS
    bluestein.c
    buffered.c
    conf.c
    ct.c
    dftw-direct.c
    dftw-directsq.c
    dftw-generic.c
    dftw-genericbuf.c
    direct.c
    generic.c
    indirect.c
    indirect-transpose.c
    kdft-dif.c
    kdft-difsq.c
    kdft-dit.c
    kdft.c
    nop.c
    plan.c
    problem.c
    rader.c
    rank-geq2.c
    solve.c
    vrank-geq1.c
    zero.c
    codelet-dft.h
    ct.h
    dft.h)

include_directories("${TOP_SRCDIR}/kernel")
addObjects(dft ${dft_STAT_SRCS})
#target_link_libraries(dft dft_scalar_codelets dft_scalar)
#IF(HAVE_SSE2)
#target_link_libraries(dft dft_sse2_codelets)
#ENDIF(HAVE_SSE2)

########### install files ###############




#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/kernel
#SUBDIRS = scalar simd
#
#noinst_LTLIBRARIES = libdft.la
#
## pkgincludedir = $(includedir)/fftw3@PREC_SUFFIX@
## pkginclude_HEADERS = codelet-dft.h dft.h
#
#libdft_la_SOURCES = bluestein.c buffered.c conf.c ct.c dftw-direct.c	\
#dftw-directsq.c dftw-generic.c dftw-genericbuf.c direct.c generic.c	\
#indirect.c indirect-transpose.c kdft-dif.c kdft-difsq.c kdft-dit.c	\
#kdft.c nop.c plan.c problem.c rader.c rank-geq2.c solve.c vrank-geq1.c	\
#zero.c codelet-dft.h ct.h dft.h
