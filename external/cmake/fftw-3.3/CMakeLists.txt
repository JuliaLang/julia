cmake_minimum_required(VERSION 2.8)
enable_language(C)

set(TOP_SRCDIR "${CMAKE_CURRENT_SOURCE_DIR}/../../fftw-3.3")
set(TOP_CMAKE_SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})

function(prefixlist list_name prefix)
    set(${list_name}_TMP)
    foreach(l ${${list_name}})
      list(APPEND ${list_name}_TMP ${prefix}${l} )
    endforeach()
    set(${list_name} "${${list_name}_TMP}" PARENT_SCOPE)
endfunction(prefixlist)

## BUILD MACROS ##
macro(addObjects library)
    list(APPEND FFTW${PREC_PREFIX}_${library}_objects ${ARGV})
    list(REMOVE_ITEM FFTW${PREC_PREFIX}_${library}_objects ${library})
	set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})
	string(REPLACE "${TOP_CMAKE_SRCDIR}" "" SRCDIR "${SRCDIR}")
    prefixlist(FFTW${PREC_PREFIX}_${library}_objects "${TOP_SRCDIR}/${SRCDIR}/")
    set_property(DIRECTORY ${TOP_CMAKE_SRCDIR} PROPERTY FFTW${PREC_PREFIX}_${library}_objects "${FFTW${PREC_PREFIX}_${library}_objects}")
endmacro()

macro(findObjects outputVar)
    foreach(f ${ARGV})
        if(NOT ${f} STREQUAL ${outputVar})
            get_property(FFTW${PREC_PREFIX}_${f}_objects DIRECTORY ${TOP_CMAKE_SRCDIR} PROPERTY FFTW${PREC_PREFIX}_${f}_objects)
            list(APPEND ${outputVar} ${FFTW${PREC_PREFIX}_${f}_objects})
        endif()
    endforeach()
endmacro()

#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(CODE "/** THIS FILE IS GENERATED AUTOMATICALLY! DO NOT EDIT **/")

include(CheckTypeSize)

function(CheckSizeof type CODE_VAR)
string(REPLACE " " "_" name ${type})
string(REPLACE "*" "P" name ${name})
string(TOUPPER "SIZEOF_${name}" name)
CHECK_TYPE_SIZE(${type} ${name})
set(${name} ${${name}} PARENT_SCOPE)
set(${CODE_VAR} "${${CODE_VAR}}\n${${name}_CODE}" PARENT_SCOPE)
endfunction()

function(AddConfigHDefine name value CODE_VAR)
set(${CODE_VAR} "${${CODE_VAR}}\n#define ${name} ${value}" PARENT_SCOPE)
endfunction()

function(AddConfigHVar name CODE_VAR)
if(${name})
	set(${CODE_VAR} "${${CODE_VAR}}\n#define ${name} ${${name}}" PARENT_SCOPE)
else()
	set(${CODE_VAR} "${${CODE_VAR}}\n#undef ${name}" PARENT_SCOPE)
endif()
endfunction()

function(AddConfigHVarB name CODE_VAR)
if(${name})
set(${CODE_VAR} "${${CODE_VAR}}\n#define ${name} 1" PARENT_SCOPE)
else()
set(${CODE_VAR} "${${CODE_VAR}}\n#undef ${name}" PARENT_SCOPE)
endif()
endfunction()


set(TYPES 
"int" 
"unsigned int" 
"long" 
"unsigned long" 
"long long" 
"unsigned long long" 
"size_t" 
"ptrdiff_t" 
"float" 
"double"
"void *")
foreach(t ${TYPES})
CheckSizeof(${t} CODE)
endforeach()

get_target_property(VER fftw3${PREC_SUFFIX} VERSION)

if(MSVC)
set(CMAKE_C_FLAGS "/GL")
set(LINK_FLAGS "/LTCG /OPT:REF /OPT:ICF")
set(CMAKE_SHARED_LINKER_FLAGS ${LINK_FLAGS})
set(CMAKE_EXE_LINKER_FLAGS ${LINK_FLAGS})
message(STATUS MSVC)
else()
set(CMAKE_C_FLAGS "-std=gnu99 -O3 -fomit-frame-pointer -mtune=native -malign-double -fstrict-aliasing -ffast-math")
endif()

addConfigHDefine("PACKAGE" "\"fftw\"" CODE)
addConfigHDefine("PACKAGE_VERSION" "\"${VER}\"" CODE)
addConfigHDefine("VERSION" "\"${VER}\"" CODE)
addConfigHDefine("FFTW_CC" "\"${CMAKE_C_FLAGS}\"" CODE)

set(HeadersToCheck malloc.h sys/types.h stdlib.h strings.h string.h stdint.h 
stddef.h sys/time.h unistd.h sys/stat.h sys/sysctl.h sys/time.h sys/types.h alloca.h c_asm.h
dlfcn.h inttypes.h intrinsics.h libintl.h limits.h mach/mach_time.h memory.h)

foreach(f ${HeadersToCheck})
string(TOUPPER ${f} name)
string(REPLACE "/" "_" name ${name})
string(REPLACE "." "_" name ${name})
check_include_file(${f} HAVE_${name})
addConfigHVarB(HAVE_${name} CODE)
endforeach()

########### DETECT FMA  ###############
set(DEFAULT_FMA "powerpc" "ia64" "hppa" "mips64")
string(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} PROC)
list(FIND DEFAULT_FMA ${PROC} ENABLE_FMA_DEFUALT)
if(ENABLE_FMA_DEFUALT EQUAL -1)
option(HAVE_FMA "Enable optimizations for machines with fused multiply-add" OFF)
else()
option(HAVE_FMA "Enable optimizations for machines with fused multiply-add" ON)
endif()
addConfigHVarB(HAVE_FMA CODE)

########## DEBUG ######################
option(FFTW_DEBUG "Define to enable extra FFTW debugging code." OFF)
option(FFTW_DEBUG_MALLOC "Define to enable debugging malloc." OFF)
option(FFTW_DEBUG_ALIGNMENT "Define to enable alignment debugging hacks." OFF)

if(FFTW_DEBUG)
set(FFTW_DEBUG_MALLOC ON)
endif()

addConfigHVarB(FFTW_DEBUG CODE)
addConfigHVarB(FFTW_DEBUG_MALLOC CODE)
addConfigHVarB(FFTW_DEBUG_ALIGNMENT CODE)

option(FFTW_RANDOM_ESTIMATOR "Define to enable pseudorandom estimate planning for debugging." OFF)
if(FFTW_RANDOM_ESTIMATOR)
set(CHECK_PL_OPTS "--estimate")
endif()
addConfigHVarB(FFTW_RANDOM_ESTIMATOR CODE)

option(FFTW_ENABLE_ALLOCA "Define to enable the use of alloca()." ON)
addConfigHVarB(FFTW_ENABLE_ALLOCA CODE)

## Precision Detection
option(FFTW_SINGLE "compile fftw in single precision" OFF)

if(FFTW_SINGLE)
	set(PRECISION "s")
	set(BENCHFFT_SINGLE ON)
endif()

option(FFTW_LDOUBLE "compile fftw in long-double precision" OFF)
if(FFTW_LDOUBLE)
	if(FFTW_SINGLE)
		message(FATAL_ERROR "Single and long double precisions conflict")
	endif()
	set(PRECISION "l")
	set(BENCHFFT_LDOUBLE ON)
endif()

option(FFTW_QUAD "compile fftw in quad precision" OFF)
if(FFTW_QUAD)
	if(FFTW_SINGLE OR FFTW_LDOUBLE)
		message(FATAL_ERROR "Conflicting Precision Options")
	endif()
	set(PRECISION "q")
	set(BENCHFFT_QUAD ON)
endif()

set(PREC_SUFFIX ${PRECISION})
if(NOT DEFINED PRECISION)
	set(PRECISION "d")
endif()

AddConfigHVarB(FFTW_SINGLE CODE)
AddConfigHVarB(FFTW_LDOUBLE CODE)
AddConfigHVarB(FFTW_QUAD CODE)

AddConfigHVar(PRECISION CODE)
AddConfigHVar(PREC_PREFIX CODE)
AddConfigHVarB(BENCHFFT_SINGLE CODE)
AddConfigHVarB(BENCHFFT_LDOUBLE CODE)
AddConfigHVarB(BENCHFFT_QUAD CODE)

##
option(ENABLE_SSE "enable SSE optimizations" OFF)
if(ENABLE_SSE AND FFTW_SINGLE)
	message(FATAL_ERROR "SSE requires single precision")
endif()

option(HAVE_SSE2 "enable SSE/SSE2 optimizations" ${ENABLE_SSE})
if(HAVE_SSE2 AND (FFTW_LDOUBLE OR FFTW_QUAD))
	message(FATAL_ERROR "SSE2 requires single or double precision")
endif()

option(HAVE_AVX "enable AVX optimizations" OFF)
if(HAVE_AVE AND (FFTW_LDOUBLE OR FFTW_QUAD))
	message(FATAL_ERROR "AVX requires single or double precision")
endif()

option(HAVE_ALTIVEC "enable Altivec optimizations" OFF)
if(HAVE_AVE AND NOT FFTW_SINGLE)
	message(FATAL_ERROR "Altivec requires single precision")
endif()

option(HAVE_MIPS_PS "enable MIPS pair-single optimizations" OFF)
if(HAVE_MIPS_PS AND NOT FFTW_SINGLE)
	message(FATAL_ERROR "MIPS paired-single requires single precision")
endif()

option(WITH_OUR_MALLOC "use our aligned malloc (helpful for Win32)" OFF)
addConfigHVarB(WITH_OUR_MALLOC CODE)

if(HAVE_SSE2)
include(CheckCXXSourceRuns)
if( CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX )
 set(SSE_FLAGS)

if(NOT FFTW_SINGLE)
 set(CMAKE_REQUIRED_FLAGS "-msse2")
 check_cxx_source_runs("
   #include <emmintrin.h>

   int main()
   {
       __m128d a, b;
       double vals[2] = {0};
       a = _mm_loadu_pd(vals);
       b = _mm_add_pd(a,a);
       _mm_storeu_pd(vals,b);
       return 0;
    }"
    HAS_SSE2_EXTENSIONS)
else()
 set(CMAKE_REQUIRED_FLAGS "-msse")
 check_cxx_source_runs("
   #include <xmmintrin.h>
   int main()
   {
       __m128 a, b;
       float vals[4] = {0};
       a = _mm_loadu_ps(vals);
       b = a;
       b = _mm_add_ps(a,b);
       _mm_storeu_ps(vals,b);
       return 0;
   }"
   HAS_SSE_EXTENSIONS)
endif()
 set(CMAKE_REQUIRED_FLAGS)

 if(HAS_SSE2_EXTENSIONS)
   message(STATUS "Using SSE2 extensions")
   set(SSE_FLAGS "-msse2 -mfpmath=sse")
   add_definitions("-DHAVE_SSE2")
 elseif(HAS_SSE_EXTENSIONS)
   message(STATUS "Using SSE extensions")
   set(SSE_FLAGS "-msse -mfpmath=sse")
 else()
   message(FATAL_ERROR "SSE2 selected, but not supported by processor")
 endif()

 add_definitions(${SSE_FLAGS})
message(STATUS ${CMAKE_C_FLAGS})
 set(SSE2_CFLAGS "${CMAKE_C_FLAGS} ${SSE_FLAGS}")
message(SATUS "${SSE2_CFLAGS}")
elseif(MSVC)
 check_cxx_source_runs("
   #include <emmintrin.h>

   int main()
   {
       __m128d a, b;
       double vals[2] = {0};
       a = _mm_loadu_pd(vals);
       b = _mm_add_pd(a,a);
       _mm_storeu_pd(vals,b);
       return 0;
    }"
    HAS_SSE2_EXTENSIONS)
 if( HAS_SSE2_EXTENSIONS )
   message(STATUS "Using SSE2 extensions")
   add_definitions( "/arch:SSE2 /fp:fast -D__SSE__ -D__SSE2__" )
 else()
   message(FATAL_ERROR "SSE2 selected, but not supported by processor")
 endif()
endif()
endif(HAVE_SSE2)

addConfigHVarB(HAVE_SSE2 CODE)
addConfigHVarB(HAVE_AVX CODE)
addConfigHVarB(HAVE_ALTIVEC CODE)
addConfigHVarB(HAVE_MIPS_PS CODE)

######## THREADS ######
option(HAVE_OPENMP "use OpenMP directives for parallelism" OFF)
option(HAVE_THREADS "compile FFTW SMP threads library" OFF)
option(WITH_COMBINED_THREADS "combine threads into main libfftw3" OFF)

if(WITH_COMBINED_THREAD AND (HAVE_OPENMP OR NOT HAVE_THREADS))
    message(FATAL_ERROR "WITH_COMBINED_THREAD requires HAVE_THREADS and not HAVE_OPENMP")
endif()

addConfigHVarB(HAVE_OPENMP CODE)
addConfigHVarB(HAVE_THREADS CODE)

if(HAVE_THREADS)
    set(CAMKE_THREAD_PREFER_PTHREADS 1)
    include(FindThreads)
    if(CMAKE_THREAD_LIBS_INIT)
        set(THREADLIBS ${CMAKE_THREAD_LIBS_INIT})
        if(CMAKE_USE_PTHREADS_INIT)
            set(USE_POSIX_THREADS 1)
        endif()
    else()
        message(FATAL_ERROR "could not find thread library")
    endif(CMAKE_THREAD_LIBS_INIT)
elseif(HAVE_OPENMP)
    include(FindOpenMP)
    if(OPENMP_FOUND)
        set(OPENMP_CFLAGS ${OpenMP_C_FLAGS})
    else()
        message(FATAL_ERROR "don't know how to enable OpenMP")
    endif(OPENMP_FOUND)
endif(HAVE_THREADS)

addConfigHVarB(USE_POSIX_THREADS CODE)

macro(conditionalSet var codition)
if(${condition})
set(${var} 1)
endif()
addConfigHVarB("${var}" CODE)
endmacro()

conditionalSet(OPENMP HAVE_OPENMP)
conditionalSet(THREADS HAVE_THREADS)
conditionalSet(SMP "HAVE_OPENMP OR HAVE_THREADS")
conditionalSet(COMBINED_THREADS HAVE_COMBINED_THREADS)

#################

set(FunctionsToCheck alloca abort BSDgettimeofday gettimeofday gethrtime read_real_time time_base_to_time drand48 sqrt memset posix_memalign memalign _mm_malloc _mm_free clock_gettime mach_absolute_time sysctl abort sinl cosl snprintf)
foreach(f ${FunctionsToCheck})
string(TOUPPER ${f} name)
CHECK_FUNCTION_EXISTS(${f} HAVE_${name})
addConfigHVarB(HAVE_${name} CODE)
endforeach()

include(../Tests.cmake)
message(STATUS "Running test")
COMPILE_TEST(TIME_WITH_SYS_TIME)
addConfigHVarB(TIME_WITH_SYS_TIME CODE)

## MSVC inline fix ##
if(MSVC)
set(CODE "${CODE}\n#ifndef __cplusplus\n#define inline __inline\n#endif")
endif(MSVC)

set(FFTW_DLL 1)
addConfigHVarB(FFTW_DLL CODE)

IF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/config.h)
file(READ ${CMAKE_CURRENT_BINARY_DIR}/config.h OLDCODE)
ENDIF()
if(NOT CODE STREQUAL OLDCODE)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/config.h "${CODE}")
endif()

add_definitions("-DHAVE_CONFIG_H")

add_subdirectory(kernel)
add_subdirectory(simd-support)
add_subdirectory(dft)
add_subdirectory(rdft)
add_subdirectory(reodft)
add_subdirectory(api)
add_subdirectory(threads)
add_subdirectory(libbench2)
add_subdirectory(mpi)

message(STATUS ${FFTW_kernel_objects})
message(STATUS ${TEST})

message(STATUS "${CMAKE_CURRENT_SOURCE_DIR}: skipped subdir $(GENFFT)")

########### next target ###############


set(SIMD_LIBS simd_support simd_sse2_nonportable)

if(HAVE_SSE2)
set(SSE2_LIBS dft_sse2_codelets rdft_sse2_codelets)
endif(HAVE_SSE2)

if(HAVE_AVX)
set(AVX_LIBS dft_avx_codelets rdft_avx_codelets)
endif(HAVE_AVX)

if(HAVE_ALTIVEC)
set(ALTIVEC_LIBS dft_altivec_codelets rdft_altivec_codelets)
endif(HAVE_ALTIVEC)


if(HAVE_THREADS)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${PTHREAD_CFLAGS}")
if(NOT COMBINED_THREADS)
set(FFTWTHREADLIBS fftw3${PREC_SUFFIX}_threads)
endif()
elseif(HAVE_OPENMP)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OPENMP_CFLAGS}")
set(FFTWTHREADLIBS fftw3${PREC_SUFFIX}_omp)
endif()

findObjects(FFTW${PREC_PREFIX}_SOURCES reodft kernel dft_scalar_codelets dft_scalar dft rdft_scalar_r2cf rdft_scalar_r2cb rdft_scalar_r2r rdft_scalar rdft api ${SSE2_LIBS} ${AVX_LIBS} ${ALTIVEC_LIBS} ${SIMD_LIBS} ${FFTWTHREADLIBS})
include_directories("${TOP_SRCDIR}/api" "${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft" "${TOP_SRCDIR}/dft/scalar" "${TOP_SRCDIR}/dft/simd" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/rdft/scalar" "${TOP_SRCDIR}/rdft/simd" "${TOP_SRCDIR}/reodft" "${TOP_SRCDIR}/simd-support")
add_library(fftw3${PREC_SUFFIX} SHARED ${FFTW${PREC_PREFIX}_SOURCES})

set_target_properties(fftw3${PREC_SUFFIX} PROPERTIES VERSION 3.3.0)
install(TARGETS fftw3${PREC_SUFFIX} DESTINATION lib)

#add_subdirectory(tools)
#add_subdirectory(tests)

########### install files ###############

configure_file(${TOP_SRCDIR}/fftw.pc.in ${CMAKE_CURRENT_BINARY_DIR}/fftw3${PREC_SUFFIX}.pc)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/fftw3${PREC_SUFFIX}.pc DESTINATION lib/pkgconfig)

## Type Sizes ##
include(CheckTypeSize)

#original Makefile.am contents follow:

#OPTIONS_AUTOMAKE=gnu
#lib_LTLIBRARIES = libfftw3@PREC_SUFFIX@.la
#
## pkgincludedir = $(includedir)/fftw3@PREC_SUFFIX@
## nodist_pkginclude_HEADERS = config.h
#
## recompile genfft if maintainer mode is true
#if MAINTAINER_MODE
#GENFFT = genfft
#else
#GENFFT =
#endif
#
#ACLOCAL_AMFLAGS=-I m4
#
#SUBDIRS=support $(GENFFT) kernel simd-support dft rdft reodft api	\
#threads libbench2 . tests mpi doc tools m4
#EXTRA_DIST=COPYRIGHT bootstrap.sh CONVENTIONS fftw.pc.in
#
#SIMD_LIBS =						\
#	simd-support/libsimd_support.la			\
#	simd-support/libsimd_sse2_nonportable.la
#
#if HAVE_SSE2
#SSE2_LIBS = dft/simd/sse2/libdft_sse2_codelets.la	\
#rdft/simd/sse2/librdft_sse2_codelets.la
#endif
#
#if HAVE_AVX
#AVX_LIBS = dft/simd/avx/libdft_avx_codelets.la	\
#rdft/simd/avx/librdft_avx_codelets.la
#endif
#
#if HAVE_ALTIVEC
#ALTIVEC_LIBS = dft/simd/altivec/libdft_altivec_codelets.la	\
#rdft/simd/altivec/librdft_altivec_codelets.la
#endif
#
#if THREADS
#if COMBINED_THREADS
#COMBINED_THREADLIBS=threads/libfftw3@PREC_SUFFIX@_threads.la
#endif
#endif
#
#libfftw3@PREC_SUFFIX@_la_SOURCES = 
#
#libfftw3@PREC_SUFFIX@_la_LIBADD =			\
#	kernel/libkernel.la				\
#	dft/libdft.la					\
#	dft/scalar/libdft_scalar.la			\
#	dft/scalar/codelets/libdft_scalar_codelets.la	\
#	rdft/librdft.la					\
#	rdft/scalar/librdft_scalar.la			\
#	rdft/scalar/r2cf/librdft_scalar_r2cf.la		\
#	rdft/scalar/r2cb/librdft_scalar_r2cb.la		\
#	rdft/scalar/r2r/librdft_scalar_r2r.la		\
#	reodft/libreodft.la				\
#	api/libapi.la					\
#        $(SIMD_LIBS) $(SSE2_LIBS) $(AVX_LIBS) $(ALTIVEC_LIBS)    	\
#	$(COMBINED_THREADLIBS)
#
#if QUAD
## cannot use -no-undefined since dependent on libquadmath
#libfftw3@PREC_SUFFIX@_la_LDFLAGS = -version-info @SHARED_VERSION_INFO@
#else
#libfftw3@PREC_SUFFIX@_la_LDFLAGS = -no-undefined -version-info	\
#@SHARED_VERSION_INFO@
#endif
#
#fftw3@PREC_SUFFIX@.pc: fftw.pc
#	cp -f fftw.pc fftw3@PREC_SUFFIX@.pc
#pkgconfigdir = $(libdir)/pkgconfig
#pkgconfig_DATA = fftw3@PREC_SUFFIX@.pc
#
#WISDOM_DIR = /etc/fftw
#WISDOM = wisdom@PREC_SUFFIX@
#
#WISDOM_TIME=12 # default to 12-hour limit, i.e. overnight
#WISDOM_FLAGS=--verbose --canonical --time-limit=$(WISDOM_TIME)
#
#wisdom:
#	tools/fftw@PREC_SUFFIX@-wisdom -o $@ $(WISDOM_FLAGS)
#
#install-wisdom: wisdom
#	$(mkinstalldirs) $(WISDOM_DIR)
#	$(INSTALL_DATA) wisdom $(WISDOM_DIR)/$(WISDOM)
