include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/libbench2"
"${TOP_SRCDIR}/dft" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/reodft"
"${TOP_SRCDIR}/threads" "${TOP_SRCDIR}/api")


########### next target ###############

set(bench_SRCS
    bench.c
    hook.c
    fftw-bench.c
    fftw-bench.h)

set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})
string(REPLACE "${TOP_CMAKE_SRCDIR}" "" SRCDIR "${SRCDIR}")
prefixlist(bench_SRCS "${TOP_SRCDIR}/${SRCDIR}/")
	
add_executable(bench ${bench_SRCS} )
add_library(bench_lib STATIC "${TOP_SRCDIR}/${SRCDIR}/fftw-bench.c" "${TOP_SRCDIR}/${SRCDIR}/bench.c")

## Dertermine libraries to link ##

target_link_libraries(bench fftw3${PREC_PREFIX} bench2)

set_target_properties(bench
PROPERTIES
ENABLE_EXPORTS ON
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

########### install files ###############


#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/libbench2	\
#-I$(top_srcdir)/dft -I$(top_srcdir)/rdft -I$(top_srcdir)/reodft	\
#-I$(top_srcdir)/threads -I$(top_srcdir)/api 
#
#noinst_PROGRAMS = bench
#EXTRA_DIST = check.pl README
#
#if THREADS
#bench_CFLAGS = $(PTHREAD_CFLAGS)
#if !COMBINED_THREADS
#LIBFFTWTHREADS = $(top_builddir)/threads/libfftw3@PREC_SUFFIX@_threads.la
#endif
#else
#if OPENMP
#bench_CFLAGS = $(OPENMP_CFLAGS)
#LIBFFTWTHREADS = $(top_builddir)/threads/libfftw3@PREC_SUFFIX@_omp.la
#endif
#endif
#
#bench_SOURCES = bench.c hook.c fftw-bench.c fftw-bench.h
#bench_LDADD = $(LIBFFTWTHREADS)				\
#$(top_builddir)/libfftw3@PREC_SUFFIX@.la		\
#$(top_builddir)/libbench2/libbench2.a $(THREADLIBS)
#
#check-local: bench$(EXEEXT)
#	perl -w $(srcdir)/check.pl $(CHECK_PL_OPTS) -r -c=30 -v `pwd`/bench
#	@echo "--------------------------------------------------------------"
#	@echo "         FFTW transforms passed basic tests!"
#	@echo "--------------------------------------------------------------"
#if SMP
#	perl -w $(srcdir)/check.pl $(CHECK_PL_OPTS) -r -c=30 -v --nthreads=2 `pwd`/bench
#	@echo "--------------------------------------------------------------"
#	@echo "         FFTW threaded transforms passed basic tests!"
#	@echo "--------------------------------------------------------------"
#endif
#
#bigcheck: bench$(EXEEXT)
#	perl -w $(srcdir)/check.pl $(CHECK_PL_OPTS) -a -v `pwd`/bench
#	@echo "--------------------------------------------------------------"
#	@echo "         FFTW transforms passed big tests!"
#	@echo "--------------------------------------------------------------"
#if SMP
#	perl -w $(srcdir)/check.pl $(CHECK_PL_OPTS) -a -v --nthreads=2 `pwd`/bench
#	perl -w $(srcdir)/check.pl $(CHECK_PL_OPTS) -a -v --nthreads=3 `pwd`/bench
#	perl -w $(srcdir)/check.pl $(CHECK_PL_OPTS) -a -v --nthreads=10 `pwd`/bench
#	@echo "--------------------------------------------------------------"
#	@echo "         FFTW threaded transforms passed big tests!"
#	@echo "--------------------------------------------------------------"
#endif
#
#smallcheck: bench$(EXEEXT)
#	perl -w $(srcdir)/check.pl -r -c=1 -v `pwd`/bench
#	perl -w $(srcdir)/check.pl -r --estimate -c=5 -v `pwd`/bench
#	@echo "--------------------------------------------------------------"
#	@echo "         FFTW transforms passed a few tests!"
#	@echo "--------------------------------------------------------------"
#if SMP
#	perl -w $(srcdir)/check.pl -r --estimate -c=2 -v --nthreads=2 `pwd`/bench
#	@echo "--------------------------------------------------------------"
#	@echo "         FFTW threaded transforms passed a few tests!"
#	@echo "--------------------------------------------------------------"
#endif
#
#paranoid-check: bench$(EXEEXT)
#if SMP
#	perl -w $(srcdir)/check.pl -a --patient --nthreads=10 --paranoid `pwd`/bench
#	perl -w $(srcdir)/check.pl -a --patient --nthreads=7 --paranoid `pwd`/bench
#	perl -w $(srcdir)/check.pl -a --patient --nthreads=3 --paranoid `pwd`/bench
#	perl -w $(srcdir)/check.pl -a --patient --nthreads=2 --paranoid `pwd`/bench
#endif
#	perl -w $(srcdir)/check.pl -a --patient --paranoid `pwd`/bench
#
#exhaustive-check: bench$(EXEEXT)
#if SMP
#	perl -w $(srcdir)/check.pl -a --exhaustive --nthreads=10 --paranoid `pwd`/bench
#	perl -w $(srcdir)/check.pl -a --exhaustive --nthreads=7 --paranoid `pwd`/bench
#	perl -w $(srcdir)/check.pl -a --exhaustive --nthreads=3 --paranoid `pwd`/bench
#	perl -w $(srcdir)/check.pl -a --exhaustive --nthreads=2 --paranoid `pwd`/bench
#endif
#	perl -w $(srcdir)/check.pl -a --exhaustive --paranoid `pwd`/bench
