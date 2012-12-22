
include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/rdft")


########### next target ###############

set(reodft_STAT_SRCS
    conf.c
    reodft.h
    reodft010e-r2hc.c
    reodft11e-radix2.c
    reodft11e-r2hc-odd.c
    redft00e-r2hc-pad.c
    rodft00e-r2hc-pad.c
    reodft00e-splitradix.c)

addObjects(reodft ${reodft_STAT_SRCS})


########### install files ###############




#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/rdft
#SUBDIRS = 
#
#noinst_LTLIBRARIES = libreodft.la
#
## pkgincludedir = $(includedir)/fftw3@PREC_SUFFIX@
## pkginclude_HEADERS = reodft.h
#
## no longer used due to numerical problems
#EXTRA_DIST = reodft11e-r2hc.c redft00e-r2hc.c rodft00e-r2hc.c
#
#libreodft_la_SOURCES = conf.c reodft.h reodft010e-r2hc.c	\
#reodft11e-radix2.c reodft11e-r2hc-odd.c redft00e-r2hc-pad.c	\
#rodft00e-r2hc-pad.c reodft00e-splitradix.c
## redft00e-r2hc.c rodft00e-r2hc.c reodft11e-r2hc.c
