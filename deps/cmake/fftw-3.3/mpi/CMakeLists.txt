########### next target ###############
include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/api" "${TOP_SRCDIR}/tests" "${TOP_SRCDIR}/libbench2")

set(TRANSPOSE_SRC 
transpose-alltoall.c
transpose-pairwise.c 
transpose-recurse.c 
transpose-problem.c 
transpose-solve.c 
mpi-transpose.h)
set(DFT_SRC 
dft-serial.c 
dft-rank-geq2.c
dft-rank-geq2-transposed.c 
dft-rank1.c 
dft-rank1-bigvec.c 
dft-problem.c 
dft-solve.c 
mpi-dft.h)
set(RDFT_SRC 
rdft-serial.c 
rdft-rank-geq2.c 
rdft-rank-geq2-transposed.c 
rdft-rank1-bigvec.c 
rdft-problem.c 
rdft-solve.c 
mpi-rdft.h)
set(RDFT2_SRC 
rdft2-serial.c 
rdft2-rank-geq2.c 
rdft2-rank-geq2-transposed.c 
rdft2-problem.c 
rdft2-solve.c 
mpi-rdft2.h)
set(SRC 
any-true.c 
api.c 
block.c 
choose-radix.c
conf.c
dtensor.c 
fftw3-mpi.h 
ifftw-mpi.h 
rearrange.c 
wisdom-api.c 
f03-wrap.c)

if(THREADS)
        set(CMAKE_C_COMPILER_CFLAGS ${PTHREAD_CFLAGS})
        if(NOT COMBINED_THREADS)
                set(LIBFFTWTHREADS = libfftw3${PREC_SUFFIX}_threads)
        endif()
elseif(OPENMP)
        set(CMAKE_C_COMPILER_CFLAGS ${OPENMP_CFLAGS})
        set(LIBFFTWTHREADS libfftw3${PREC_SUFFIX}_omp)
endif()

if(MPI)
list(APPEND fftw3${PREC_SUFFIX}_mpi_LIB_SRCS
    ${SRC}
    ${TRANSPOSE_SRC}
    ${DFT_SRC}
    ${RDFT_SRC}
    ${RDFT2_SRC})

set(mpi_bench_SOURCES mpi-bench.c ${TOP_SRCDIR}/tests/fftw-bench.c ${TOP_SRCDIR}/tests/hook.c)

add_executable(mpi-bench ${mpi_bench_SOURCES})

addObjects(fftw3${PREC_SUFFIX}_mpi ${fftw3${PREC_SUFFIX}_mpi_LIB_SRCS})

target_link_libraries(mpi-bench ${LIBFFTWTHREADS} fftw3${PREC_SUFFIX}_mpi fftw3${PREC_SUFFIX} bench2 ${MPILIBS} ${THREADLIBS})

set_target_properties(fftw3${PREC_SUFFIX}_mpi PROPERTIES VERSION 1.0.0 SOVERSION 1)
install(TARGETS fftw3${PREC_SUFFIX}_mpi ${INSTALL_TARGETS_DEFAULT_ARGS} DESTINATION lib)

endif(MPI)

########### install files ###############
configure_file(${TOP_SRCDIR}/mpi/fftw3-mpi.f03.in ${CMAKE_CURRENT_BINARY_DIR}/fftw3-mpi.f03)

install(FILES  ${TOP_SRCDIR}/mpi/fftw3-mpi.h DESTINATION include)
install(FILES  ${CMAKE_CURRENT_BINARY_DIR}/fftw3-mpi.f03 DESTINATION lib/pkgconfig)

############ CHECKS ######################
if(MPI)

set(CHECK ${top_srcdir}/tests/check.pl)
set(NUMCHECK 10)
set(CHECKSIZE 10000)
set(CHECKOPTS --verbose --random --maxsize=${CHECKSIZE} -c=${NUMCHECK} ${CHECK_PL_OPTS})

FindPerl(PERL_EXECUTABLE PERL_FOUND)
if(NOT ${PERL_FOUND})
	message(WARNING "MPI checks will be disable dut to the lack of perl")
else()
add_custom_TARGET(check-local
	${PERL_EXECUTABLE} -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 1 `pwd`/mpi-bench"
	COMMAND echo "--------------------------------------------------------------"
	COMMAND echo "     MPI FFTW transforms passed "$(NUMCHECK)" tests, 1 CPU"
	COMMAND "--------------------------------------------------------------"
	COMMAND ${PERL_EXECUTABLE} -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 2 `pwd`/mpi-bench"
	COMMAND echo "--------------------------------------------------------------"
	COMMAND echo "      MPI FFTW transforms passed "$(NUMCHECK)" tests, 2 CPUs"
	COMMAND echo "--------------------------------------------------------------"
	COMMAND ${PERL_EXECUTABLE} -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 3 `pwd`/mpi-bench"
	COMMAND echo "--------------------------------------------------------------"
	COMMAND echo "      MPI FFTW transforms passed "$(NUMCHECK)" tests, 3 CPUs"
	COMMAND echo "--------------------------------------------------------------"
	COMMAND ${PERL_EXECUTABLE} -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 4 `pwd`/mpi-bench"
	COMMAND echo "--------------------------------------------------------------"
	COMMAND echo "      MPI FFTW transforms passed "$(NUMCHECK)" tests, 4 CPUs"
	COMMAND echo "--------------------------------------------------------------"
	DEPENDS mpi-bench
	WORKING_DIRECOTRY ${CMAKE_CURRENT_BINARY_DIR})
	if(DEFINED SMP)
		add_custom_TARGET(check-local-smp
		COMMAND perl -w $(CHECK) $(CHECKOPTS) --mpi --nthreads=2 "$(MPIRUN) -np 3 `pwd`/mpi-bench"
		COMMAND echo "--------------------------------------------------------------"
		COMMAND echo "      MPI FFTW threaded transforms passed "$(NUMCHECK)" tests!"
		COMMAND echo "--------------------------------------------------------------"
		DEPENDS mpi-bench)
		add_dependencies(check-local check-local-smp)
	endif(DEFINED SMP)
endif()

endif(MPI)

## TODO: bigcheck smallcheck

#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/dft	\
#-I$(top_srcdir)/rdft -I$(top_srcdir)/api -I$(top_srcdir)/tests	\
#-I$(top_srcdir)/libbench2
#
#if MPI
#lib_LTLIBRARIES = libfftw3@PREC_SUFFIX@_mpi.la
#include_HEADERS = fftw3-mpi.h
#nodist_include_HEADERS = fftw3-mpi.f03
#noinst_PROGRAMS = mpi-bench
#endif
#
#CC=@MPICC@
#
#EXTRA_DIST = testsched.c f03api.sh f03-wrap.sh genf03-wrap.pl fftw3-mpi.f03.in
#BUILT_SOURCES = fftw3-mpi.f03.in fftw3-mpi.f03 f03-wrap.c
#
#TRANSPOSE_SRC = transpose-alltoall.c transpose-pairwise.c transpose-recurse.c transpose-problem.c transpose-solve.c mpi-transpose.h
#DFT_SRC = dft-serial.c dft-rank-geq2.c dft-rank-geq2-transposed.c dft-rank1.c dft-rank1-bigvec.c dft-problem.c dft-solve.c mpi-dft.h
#RDFT_SRC = rdft-serial.c rdft-rank-geq2.c rdft-rank-geq2-transposed.c rdft-rank1-bigvec.c rdft-problem.c rdft-solve.c mpi-rdft.h
#RDFT2_SRC = rdft2-serial.c rdft2-rank-geq2.c rdft2-rank-geq2-transposed.c rdft2-problem.c rdft2-solve.c mpi-rdft2.h
#SRC = any-true.c api.c block.c choose-radix.c conf.c dtensor.c fftw3-mpi.h ifftw-mpi.h rearrange.c wisdom-api.c f03-wrap.c
#
#libfftw3@PREC_SUFFIX@_mpi_la_SOURCES = $(SRC) $(TRANSPOSE_SRC) $(DFT_SRC) $(RDFT_SRC) $(RDFT2_SRC)
#
#libfftw3@PREC_SUFFIX@_mpi_la_LDFLAGS = -version-info @SHARED_VERSION_INFO@
#
#if THREADS
#mpi_bench_CFLAGS = $(PTHREAD_CFLAGS)
#if !COMBINED_THREADS
#LIBFFTWTHREADS = $(top_builddir)/threads/libfftw3@PREC_SUFFIX@_threads.la
#endif
#else
#if OPENMP
#mpi_bench_CFLAGS = $(OPENMP_CFLAGS)
#LIBFFTWTHREADS = $(top_builddir)/threads/libfftw3@PREC_SUFFIX@_omp.la
#endif
#endif
#
#mpi_bench_SOURCES = mpi-bench.c $(top_srcdir)/tests/fftw-bench.c $(top_srcdir)/tests/hook.c
#mpi_bench_LDADD = libfftw3@PREC_SUFFIX@_mpi.la $(LIBFFTWTHREADS) $(top_builddir)/libfftw3@PREC_SUFFIX@.la $(top_builddir)/libbench2/libbench2.a $(MPILIBS) $(THREADLIBS)
#
#CHECK = $(top_srcdir)/tests/check.pl
#NUMCHECK=10
#CHECKSIZE=10000
#CHECKOPTS = --verbose --random --maxsize=$(CHECKSIZE) -c=$(NUMCHECK) $(CHECK_PL_OPTS)
#
#if MPI
#
#check-local: mpi-bench$(EXEEXT)
#	perl -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 1 `pwd`/mpi-bench"
#	@echo "--------------------------------------------------------------"
#	@echo "     MPI FFTW transforms passed "$(NUMCHECK)" tests, 1 CPU"
#	@echo "--------------------------------------------------------------"
#	perl -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 2 `pwd`/mpi-bench"
#	@echo "--------------------------------------------------------------"
#	@echo "      MPI FFTW transforms passed "$(NUMCHECK)" tests, 2 CPUs"
#	@echo "--------------------------------------------------------------"
#	perl -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 3 `pwd`/mpi-bench"
#	@echo "--------------------------------------------------------------"
#	@echo "      MPI FFTW transforms passed "$(NUMCHECK)" tests, 3 CPUs"
#	@echo "--------------------------------------------------------------"
#	perl -w $(CHECK) $(CHECKOPTS) --mpi "$(MPIRUN) -np 4 `pwd`/mpi-bench"
#	@echo "--------------------------------------------------------------"
#	@echo "      MPI FFTW transforms passed "$(NUMCHECK)" tests, 4 CPUs"
#	@echo "--------------------------------------------------------------"
#if SMP
#	perl -w $(CHECK) $(CHECKOPTS) --mpi --nthreads=2 "$(MPIRUN) -np 3 `pwd`/mpi-bench"
#	@echo "--------------------------------------------------------------"
#	@echo "      MPI FFTW threaded transforms passed "$(NUMCHECK)" tests!"
#	@echo "--------------------------------------------------------------"
#endif
#
#bigcheck: mpi-bench$(EXEEXT)
#	$(MAKE) $(AM_MAKEFLAGS) NUMCHECK=100 CHECKSIZE=60000 check-local
#
#smallcheck: mpi-bench$(EXEEXT)
#	$(MAKE) $(AM_MAKEFLAGS) NUMCHECK=2 check-local
#
#endif
#
#fftw3-mpi.f03: fftw3-mpi.f03.in
#	sed 's/C_MPI_FINT/@C_MPI_FINT@/' $(srcdir)/fftw3-mpi.f03.in > $@
#
#if MAINTAINER_MODE
#
#fftw3-mpi.f03.in: fftw3-mpi.h f03api.sh $(top_srcdir)/api/genf03.pl
#	sh $(srcdir)/f03api.sh > $@
#
#f03-wrap.c: fftw3-mpi.h f03-wrap.sh genf03-wrap.pl
#	sh $(srcdir)/f03-wrap.sh > $@
#
#endif
