set(CMAKE_C_FLAGS ${STACK_ALIGN_CFLAGS})
include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/api")
########### next target ###############
set(fftw3${PREC_SUFFIX}_threads_STAT_SRCS
    api.c
    conf.c
    threads.c
    threads.h
    dft-vrank-geq1.c
    ct.c
    rdft-vrank-geq1.c
    hc2hc.c
    vrank-geq1-rdft2.c
    f77api.c
    f77funcs.h)
	
set(libfftw3${PREC_SUFFIX}_omp_la_SOURCES 
api.c
conf.c 
openmp.c
threads.h 
dft-vrank-geq1.c 
ct.c
rdft-vrank-geq1.c
hc2hc.c
vrank-geq1-rdft2.c
f77api.c
f77funcs.h)

if (HAVE_THREADS)
        addObjects(fftw3${PREC_SUFFIX}_threads ${fftw3${PREC_SUFFIX}_threads_STAT_SRCS})
	if(NOT COMBINED_THREADS)
		if(OPENMP)
                        addObjects(fftw3${PREC_SUFFIX}_omp ${libfftw3${PREC_SUFFIX}_omp_la_SOURCES})
                  #      install(TARGETS fftw3${PREC_SUFFIX}_omp DESTINATION lib)
		endif()
             #   install(TARGETS fftw3${PREC_SUFFIX}_threads DESTINATION lib)
	endif()
        #set_target_properties(fftw3${PREC_SUFFIX}_threads PROPERTIES VERSION 1.0.0 SOVERSION 1)
elseif(HAVE_OPENMP)
        addObjects(fftw3${PREC_SUFFIX}_omp ${libfftw3${PREC_SUFFIX}_omp_la_SOURCES})
       # install(TARGETS fftw3${PREC_SUFFIX}_omp DESTINATION lib)
endif()

########### next target ###############

#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/dft	\
#-I$(top_srcdir)/rdft -I$(top_srcdir)/api
#AM_CFLAGS = $(STACK_ALIGN_CFLAGS)
#
#if OPENMP
#FFTWOMPLIB = libfftw3@PREC_SUFFIX@_omp.la
#else
#FFTWOMPLIB = 
#endif
#
#if THREADS
#if COMBINED_THREADS
#noinst_LTLIBRARIES = libfftw3@PREC_SUFFIX@_threads.la
#else
#lib_LTLIBRARIES = libfftw3@PREC_SUFFIX@_threads.la $(FFTWOMPLIB)
#endif
#else
#lib_LTLIBRARIES = $(FFTWOMPLIB)
#endif
#
## pkgincludedir = $(includedir)/fftw3@PREC_SUFFIX@
## pkginclude_HEADERS = threads.h
#
#libfftw3@PREC_SUFFIX@_threads_la_SOURCES = api.c conf.c threads.c	\
#threads.h dft-vrank-geq1.c ct.c rdft-vrank-geq1.c hc2hc.c		\
#vrank-geq1-rdft2.c f77api.c f77funcs.h
#libfftw3@PREC_SUFFIX@_threads_la_CFLAGS = $(PTHREAD_CFLAGS)
#libfftw3@PREC_SUFFIX@_threads_la_LDFLAGS = -version-info @SHARED_VERSION_INFO@
#
#libfftw3@PREC_SUFFIX@_omp_la_SOURCES = api.c conf.c openmp.c	\
#threads.h dft-vrank-geq1.c ct.c rdft-vrank-geq1.c hc2hc.c	\
#vrank-geq1-rdft2.c f77api.c f77funcs.h
#libfftw3@PREC_SUFFIX@_omp_la_CFLAGS = $(OPENMP_CFLAGS)
#libfftw3@PREC_SUFFIX@_omp_la_LDFLAGS = -version-info @SHARED_VERSION_INFO@
