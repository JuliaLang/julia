cmake_minimum_required(VERSION 2.8)
## library versions ##

set(llvm_VER 3.0)
set(readline_VER 6.2)
set(pcre_VER 8.21)
set(grisu_VER 1.1)
set(dsfmt_VER 2.1)
set(openplas_VER v0.1alpha2.5)
set(lapack_VER 3.4.0)
set(arpack_VER 3.0.2)
set(fftw_VER 3.3)
set(suitesparse_VER 3.7.0)
set(clp_VER 1.14.5)
set(unwind_VER 1.0.1)
set(lightttpd_VER = 1.4.29)

## Download Paths (to be replaced by git sumbodules) ##
set(llvm_URL http://llvm.org/releases/${llvm_VER}/llvm-${llvm_VER}.tar.gz)
set(readline_URL ftp://ftp.gnu.org/gnu/readline/readline-${readline_VER}.tar.gz)
set(pcre_URL ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-${pcre_VER}.tar.bz2)
set(grisu_URL http://double-conversion.googlecode.com/files/double-conversion-${grisu_VER}.tar.gz)
set(dsfmt_URL http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-${DSFMT_VER}.tar.gz)
set(openblas_URL https://github.com/xianyi/OpenBLAS/tarball/${openblas_VER})
set(lapack_URL http://www.netlib.org/lapack/lapack-${lapack_VER}.tgz)
set(arpack_URL http://forge.scilab.org/index.php/p/arpack-ng/downloads/353/get/arpack-ng-${arpack_VER}.tar.gz)
set(fftw_URL http://www.fftw.org/fftw-${fftw_VER}.tar.gz)
set(suitesparse_URL http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-${suitesparse_VER}.tar.gz)
set(clp_URL http://www.coin-or.org/download/source/Clp/Clp-${clp_URL}.tgz)
set(unwind_URL http://savannah.spinellicreations.com/libunwind/libunwind-${unwind_VER}.tar.gz)
set(lighthttp_URL download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-${lighttd_VER}.tar.gz)


## Download Packages ##

set(PACKAGES llvm pcre fdlibm dsfmt openblas lapack arpack fftw suitesparse suitesparse-wrapper double-conversion amos)

foreach(package ${PACKAGES})
IF(NOT EXISTS ${CMAKE_SOURCE_DIR}/${package}-${${package}_VER})
unset(downLoadStatus)
file(DOWNLOAD ${${package}_VER} STATUS downloadStatus)
IF(downloadStatus>210)
message(FATAL_ERROR "Failed to download package" ${package})
endif()
endif()
endforeach(package)
## General Setup ##

set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR}/"root")

if( CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE )
  message(FATAL_ERROR "In-source builds are not allowed.
Please create a directory and run cmake from there, passing the path
to this source directory as the last argument.
This process created the file `CMakeCache.txt' and the directory `CMakeFiles'.
Please delete them.")
endif()

## LLVM Setup

set(LLVM_ENABLE_THREADS OFF)
set(LLVM_TARGETS_TO_BUILD X86)
add_subdirectory(llvm-${LLVM_VER})

## PCRE Setup

set(PCRE_SUPPORT_UNICODE_PROPERTIES ON)
set(PCRE_SUPPORT_UTF8 ON)
set(PCRE_SUPPORT_JIT ON)
add_subdirectory(pcre-${PCRE_VER})


# The Following configure check has been taken from LaPACK
# Check the usage of the user provided BLAS libraries
# If none were found, build the OpenBlas libraries

if(BLAS_LIBRARIES)
  include(CheckFortranFunctionExists)
  set(CMAKE_REQUIRED_LIBRARIES ${BLAS_LIBRARIES})
  unset( BLAS_FOUND )
  CHECK_FORTRAN_FUNCTION_EXISTS2("dgemm" BLAS_FOUND)
  unset( CMAKE_REQUIRED_LIBRARIES )
  if(BLAS_FOUND)
    message(STATUS "--> BLAS supplied by user is WORKING, will use ${BLAS_LIBRARIES}.")
  else(BLAS_FOUND)
    message(ERROR "--> BLAS supplied by user is not WORKING, CANNOT USE ${BLAS_LIBRARIES}.")
    message(ERROR "-->     Will use OpenBlas (by default)")
    message(ERROR "-->     Or Correct your BLAS_LIBRARIES entry ")
    message(ERROR "-->     Or Consider checking USE_OPTIMIZED_BLAS")
  endif(BLAS_FOUND)

# User did not provide a BLAS Library but specified to search for one
elseif( USE_OPTIMIZED_BLAS )
  find_package( BLAS )
endif (BLAS_LIBRARIES)

# Neither user specified or optimized BLAS libraries can be used
if(NOT BLAS_FOUND)
  message(STATUS "Using OpenBlas")
  set( BLAS_LIBRARIES ${CMAKE_SOURCE_DIR}/openblas-${OPENBLAS_VER}/libopenblas.lib)
  set( BUILD_OPENBLAS ON)
else()
  set( CMAKE_EXE_LINKER_FLAGS 
    "${CMAKE_EXE_LINKER_FLAGS} ${BLAS_LINKER_FLAGS}" 
    CACHE STRING "Linker flags for executables" FORCE)
  set( CMAKE_MODULE_LINKER_FLAGS 
    "${CMAKE_MODULE_LINKER_FLAGS} ${BLAS_LINKER_FLAGS}" 
    CACHE STRING "Linker flags for modules" FORCE)
  set( CMAKE_SHARED_LINKER_FLAGS 
    "${CMAKE_SHARED_LINKER_FLAGS} ${BLAS_LINKER_FLAGS}" 
    CACHE STRING "Linker flags for shared libs" FORCE)
endif( NOT BLAS_FOUND )

set(BUILD_SHARED_LIBS ON)
add_subdirectory(lapack-${LAPACK_VER})

