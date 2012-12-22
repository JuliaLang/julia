########### next target ###############

if(THREADS)
        set(CMAKE_C_COMPILER_CFLAGS ${PTHREAD_CFLAGS})
        if(NOT COMBINED_THREADS)
                set(LIBFFTWTHREADS = libfftw3${PREC_SUFFIX}_threads)
        endif()
elseif(OPENMP)
        set(CMAKE_C_COMPILER_CFLAGS ${OPENMP_CFLAGS})
        set(LIBFFTWTHREADS libfftw3${PREC_SUFFIX}_omp)
endif()

set(fftw${PREC_SUFFIX}-wisdom_SRCS fftw-wisdom.c)

set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})
string(REPLACE "${TOP_CMAKE_SRCDIR}" "" SRCDIR "${SRCDIR}")
prefixlist(fftw${PREC_SUFFIX}-wisdom_SRCS "${TOP_SRCDIR}/${SRCDIR}/")

include_directories("${TOP_SRCDIR}/libbench2" "${TOP_SRCDIR}/tests" "${TOP_SRCDIR}/api")

add_executable(fftw${PREC_SUFFIX}-wisdom ${fftw${PREC_SUFFIX}-wisdom_SRCS})

target_link_libraries(fftw${PREC_SUFFIX}-wisdom bench_lib ${LIBFFTWTHREADS} fftw3${PREC_PREFIX} bench2)

#target_link_libraries(fftw${PREC_SUFFIX}-wisdom ${KDE4_KDECORE_LIBS})

install(TARGETS fftw${PREC_SUFFIX}-wisdom ${INSTALL_TARGETS_DEFAULT_ARGS} DESTINATION bin)


########### install files ###############

install(PROGRAMS fftw-wisdom-to-conf DESTINATION bin)



#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/libbench2 -I$(top_srcdir)/api 
#
#bin_SCRIPTS = fftw-wisdom-to-conf
#bin_PROGRAMS = fftw@PREC_SUFFIX@-wisdom
#
#BUILT_SOURCES = fftw-wisdom-to-conf fftw@PREC_SUFFIX@-wisdom.1
#EXTRA_DIST = fftw-wisdom-to-conf.in
#
#dist_man_MANS = fftw-wisdom-to-conf.1 fftw@PREC_SUFFIX@-wisdom.1
#EXTRA_MANS = fftw_wisdom.1.in
#fftw@PREC_SUFFIX@-wisdom.1: fftw_wisdom.1
#	rm -f $@
#	cp fftw_wisdom.1 $@
#
#if THREADS
#fftw@PREC_SUFFIX@_wisdom_CFLAGS = $(PTHREAD_CFLAGS)
#if !COMBINED_THREADS
#LIBFFTWTHREADS = $(top_builddir)/threads/libfftw3@PREC_SUFFIX@_threads.la
#endif
#else
#if OPENMP
#fftw@PREC_SUFFIX@_wisdom_CFLAGS = $(OPENMP_CFLAGS)
#LIBFFTWTHREADS = $(top_builddir)/threads/libfftw3@PREC_SUFFIX@_omp.la
#endif
#endif
#
#fftw@PREC_SUFFIX@_wisdom_SOURCES = fftw-wisdom.c
#fftw@PREC_SUFFIX@_wisdom_LDADD = $(top_builddir)/tests/bench-bench.o	\
#$(top_builddir)/tests/bench-fftw-bench.o $(LIBFFTWTHREADS)	\
#$(top_builddir)/libfftw3@PREC_SUFFIX@.la			\
#$(top_builddir)/libbench2/libbench2.a $(THREADLIBS)
