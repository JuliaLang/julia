cmake_minimum_required(VERSION 2.8)
## library versions ##

set(EXTERNAL_TOP_SRCDIR ${CMAKE_CURRENT_SOURCE_DIR})

set(llvm_VER 3.0)
set(readline_VER 6.2)
set(pcre_VER 8.21)
set(grisu_VER 1.1)
set(dsfmt_VER 2.1)
set(openblas_VER v0.1alpha2.5)
set(lapack_VER 3.4.0)
set(arpack_VER 3.0.2)
set(fftw_VER 3.3)
set(suitesparse_VER 3.7.0)
set(clp_VER 1.14.5)
set(unwind_VER 1.0.1)
set(lightttpd_VER 1.4.29)

## Download Paths (to be replaced by git sumbodules) ##
set(validExtensions tar gz bz2 tgz)
set(llvm_URL http://llvm.org/releases/${llvm_VER}/llvm-${llvm_VER}.tar.gz)
set(readline_URL ftp://ftp.gnu.org/gnu/readline/readline-${readline_VER}.tar.gz)
set(pcre_URL ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-${pcre_VER}.tar.bz2)
set(grisu_URL http://double-conversion.googlecode.com/files/double-conversion-${grisu_VER}.tar.gz)
set(dsfmt_URL http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-${dsfmt_VER}.tar.gz)
set(openblas_URL https://github.com/xianyi/OpenBLAS/tarball/${openblas_VER})
set(lapack_URL http://www.netlib.org/lapack/lapack-${lapack_VER}.tgz)
set(arpack_URL http://forge.scilab.org/index.php/p/arpack-ng/downloads/353/get/)
set(fftw_URL http://www.fftw.org/fftw-${fftw_VER}.tar.gz)
set(suitesparse_URL http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-${suitesparse_VER}.tar.gz)
set(clp_URL http://www.coin-or.org/download/source/Clp/Clp-${clp_URL}.tgz)
set(unwind_URL http://savannah.spinellicreations.com/libunwind/libunwind-${unwind_VER}.tar.gz)
set(lighthttp_URL download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-${lighttd_VER}.tar.gz)

## Needs tar command for unzipping - Possibly use cmake tar once strip-components is available ##
find_program(TarPath "tar" DOC "tar command")
if(${TarPath} MATCHES "TarPath-NOTFOUND")
	message(FATAL_ERROR "tar command was not found")
endif()

option(CHECK_CERTIFICATES "Enables certificate checks" OFF)
if(NOT DEFINED BACKUP_DOWNLOADER)
	find_program(WgetPath "wget" DOC "wget command")
	if(${WgetPath} MATCHES "WgetPath-NOTFOUND")
		message(STATUS "wget command was not found")
		find_program(CUrlPath "curl" DOC "wget command")
		if(${CUrlPath} MATCHES "CUrlPath-NOTFOUND")
			message(STATUS "curl command was not found")
		else(${CUrlPath} MATCHES "CUrlPath-NOTFOUND")
			set(BACKUP_DOWNLOADER "${CUrlPath} -kLo")
		endif()
	else()
		set(BACKUP_DOWNLOADER ${WgetPath} "--no-check-certificate" "-O")
	endif()
endif(NOT DEFINED BACKUP_DOWNLOADER)
if(NOT DEFINED BACKUP_DOWNLOADER)
	message(WARNING "No alternate downloader found. Some file downloads may not be supported")
	message(WARNING "You can use -DBACKUP_DOWNLOADER:PATH=<path> to specify an alternative downloader!")
endif(NOT DEFINED BACKUP_DOWNLOADER)
	
## Download Packages ##

set(PACKAGES llvm pcre dsfmt openblas lapack arpack fftw suitesparse)

## Linux Specific Packages ##

IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	set(PACKAGES ${PACKAGES} unwind)
endif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

## Other Packages ##

option(USE_SYSTEM_READLINE "Use the system-provided readline library" OFF)
IF(NOT USE_SYSTEM_READLINE)
	set(PACKAGES ${PACKAGES} readline)
ENDIF(NOT USE_SYSTEM_READLINE)

foreach(package ${PACKAGES})
	message(STATUS "Checking package " ${package})
	## Get Version and URL to download ##
	set(versionvar "${package}_VER")
	set(version ${${versionvar}})
	set(URLvar "${package}_URL")
	set(URL ${${URLvar}})
	## Get proper extension ##
	GET_FILENAME_COMPONENT(ext ${URL} EXT)
	## The extension may contain leftover junk from the version informmation. Clean that up! ##

	string(REPLACE "." ";" ext ${ext} "")        #Create list of extensions
	set(ext2 "${ext}") 					      #copy list
	list(REMOVE_ITEM ext2 ${validExtensions}) 
	list(REMOVE_ITEM ext ${ext2} "")             #remove all but valid extensions
	## Account for missing file extension and assume tar.gz
	list(LENGTH ext count)
	IF(${count} EQUAL 0)
		set(ext "tar;gz")
	endif()

	string(REPLACE ";" "." ext "${ext}")      #turn list back into string

	IF(NOT EXISTS ${CMAKE_SOURCE_DIR}/${package}-${version}.${ext})
		## Download the File ##
		message(STATUS "Downloading package " ${package})
		file(DOWNLOAD ${URL} ${CMAKE_SOURCE_DIR}/${package}-${version}.${ext} STATUS downloadStatus SHOW_PROGRESS)
		list(GET downloadStatus 1 downloadError)
		list(GET downloadStatus 0 downloadStatus)
		IF(downloadStatus EQUAL 1 AND downloadError MATCHES "unsupported protocol")
			message(STATUS "Using alternate downloader from (${BACKUP_DOWNLOADER})")
			execute_process(COMMAND ${BACKUP_DOWNLOADER} ${CMAKE_SOURCE_DIR}/${package}-${version}.${ext} ${URL} RESULT_VARIABLE downloadStatus)
		endif()
		IF(NOT downloadStatus EQUAL 0)
			file(REMOVE ${CMAKE_SOURCE_DIR}/${package}-${version}.${ext})
			message(STATUS "File:" ${URL})
			message(STATUS "Error was: ${downloadError} (${downloadStatus})")
			message(FATAL_ERROR "Failed to download package " ${package})
		else()
			message(STATUS "Download of package " ${package} " succesful")
		endif()
	endif()
	
	IF(NOT EXISTS ${CMAKE_SOURCE_DIR}/${package}-${version})
		message(STATUS "Decompressing package")
		file(MAKE_DIRECTORY ${CMAKE_SOURCE_DIR}/${package}-${version})
		execute_process(COMMAND ${TarPath} -C ${package}-${version} --strip-components 1 -xf "${package}-${version}.${ext}"  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR} RESULT_VARIABLE result)
		IF(NOT EXISTS ${CMAKE_SOURCE_DIR}/${package}-${version} OR result)
			file(REMOVE_RECURSE ${CMAKE_SOURCE_DIR}/${package}-${version})
			message(FATAL_ERROR "Decompression of package " ${package} " failed")
		else()
			message(STATUS "Decompression of package " ${package} " succesful")
		endif()
	ENDIF()
	
endforeach(package)

## Duplicate FFTW ##
IF(NOT EXISTS ${CMAKE_SOURCE_DIR}/fftw-double-${fftw_VER})
message(STATUS "creating fftw-double")
file(COPY fftw-${fftw_VER} DESTINATION ${CMAKE_BINARY_DIR})
file(RENAME ${CMAKE_BINARY_DIR}/fftw-${fftw_VER} ${CMAKE_SOURCE_DIR}/fftw-double-${fftw_VER})
message(STATUS "creation of fftw-double completed (hopefully)")
ENDIF()
## General Setup ##

if(NOT DEFINED CMAKE_INSTALL_PREFIX)
	set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR}/"root")
endif(NOT DEFINED CMAKE_INSTALL_PREFIX)

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
add_subdirectory(llvm-${llvm_VER})

## PCRE Setup

set(PCRE_SUPPORT_UNICODE_PROPERTIES ON)
set(PCRE_SUPPORT_UTF8 ON)
set(PCRE_SUPPORT_JIT ON)
add_subdirectory(pcre-${pcre_VER})


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

set(OpenBlasLIB ${CMAKE_SOURCE_DIR}/openblas-${openblas_VER}/libopenblas.${CMAKE_STATIC_LIBRARY_SUFFIX})
# Neither user specified or optimized BLAS libraries can be used
if(NOT BLAS_FOUND)
  message(STATUS "Using OpenBlas")
  set( BLAS_LIBRARIES ${OpenBlasLIB})
  set( BUILD_OPENBLAS ON)
  set( BLAS_TARGET openblas)
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
set(BLAS_FOUND 1) #Prevent LaPack from using it's own blas version
add_subdirectory(lapack-${lapack_VER})
set( LAPACK_TARGET lapack-static )

add_subdirectory(amos)
##Set ASM Env var -- should not be necessary technically
include(CMakeDetermineASMCompiler)
set(CMAKE_ASM_CREATE_SHARED_LIBRARY ${CMAKE_C_CREATE_SHARED_LIBRARY})

add_subdirectory(fdlibm)
set(LapackLIB lapack)

add_subdirectory(cmake)

set(FFTW_CONFIG --enable-shared --disable-fortran --disable-mpi --disable-openmp --disable-threads --enable-sse2 --enable-fma)
IF(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
set(FFTW_CONFIG ${FFTW_CONFIG} --with-our-malloc)
ENDIF()

macro(autoconfTarget name options)
message(STATUS ${name})
set(${name}_SRC_DIR ${CMAKE_SOURCE_DIR}/${name}-${${name}_VER})
set(${name}_BINARY_DIR ${CMAKE_BINARY_DIR}/${name}-${${name}_VER})
set(${name}_CONFIGURE ${${name}_SRC_DIR}/configure)
message(STATUS ${name}_SRC_DIR)
message(STATUS ${${name}_SRC_DIR})
message(STATUS ${${name}_BINARY_DIR})
file(MAKE_DIRECTORY ${${name}_BINARY_DIR})
add_custom_command(OUTPUT ${${name}_BINARY_DIR}/Makefile  COMMAND ${${name}_CONFIGURE} --prefix=${CMAKE_INSTALL_PREFIX} ${options} WORKING_DIRECTORY ${${name}_BINARY_DIR})
add_custom_command(OUTPUT ${${name}_BINARY_DIR}/BuildDone COMMAND $(MAKE) -C ${${name}_BINARY_DIR}
COMMAND touch ${${name}_CONFIGURE}
COMMAND $(MAKE) -C ${${name}_BINARY_DIR} install
COMMAND touch ${${name}_BINARY_DIR}/BuildDone
DEPENDS ${${name}_BINARY_DIR}/Makefile
WORKING_DIRECTORY ${${name}_BINARY_DIR})
endmacro()

#set(fftw-double_VER ${fftw_VER})
#set(fftwDoubleFeatures --enable-sse --enable-float ${FFTW_CONFIG})
#autoconfTarget(fftw-double "${fftwDoubleFeatures}")
#
#autoconfTarget(fftw "${FFTW_CONFIG}")

IF(OFF)
autoconfTarget(clp)
ENDIF()


IF(BUILD_OPENBLAS)
set(OPENBLAS_SRC_DIR ${CMAKE_SOURCE_DIR}/openblas-${openblas_VER})
set(OPENBLAS_BINARY_DIR ${CMAKE_BINARY_DIR}/openblas-${openblas_VER})
set(OPENBLAS_CONFIGURE ${OPENBLAS_SRC_DIR}/configure)
file(MAKE_DIRECTORY ${OPENBLAS_BINARY_DIR})
add_custom_target(openblas COMMAND perl -i -ple "'s/^\\s*(EXTRALIB\\s*\\+=\\s*-lSystemStubs)\\s*$$/# $$1/g'" ${CMAKE_CURRENT_SOURCE_DIR}/openblas-${openblas_VER}/Makefile.system
COMMAND $(MAKE) DYNAMIC_ARCH=1 USE_THREAD=0 NO_LAPACK=1 CC=$(CC) FC=$(FC)  FFLAGS="$(FFLAGS)" TARGET=$(TARGET_OPENBLAS_ARCH)
COMMAND $(MAKE) install PREFIX=${CMAKE_INSTALL_PREFIX} DYNAMIC_ARCH=1 USE_THREAD=0 NO_LAPACK=1 CC=$(CC) FC=$(FC)  FFLAGS="$(FFLAGS)" TARGET=$(TARGET_OPENBLAS_ARCH)
WORKING_DIRECTORY ${OPENBLAS_SRC_DIR})
ENDIF()

IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
set(UNWIND_SRC_DIR ${CMAKE_SOURCE_DIR}/unwind-${unwind_VER})
set(UNWIND_BINARY_DIR ${CMAKE_BINARY_DIR}/unwind-${unwind_VER})
set(UNWIND_CONFIGURE ${UNWIND_SRC_DIR}/configure)
add_custom_target(unwind ALL cd ${UNWIND_DIR} COMMAND ${UNWIND_CONFIGURE} CFLAGS="-U_FORTIFY_SOURCE -fPIC" --prefix=${CMAKE_INSTALL_PREFIX} 
COMMAND $(MAKE) -C ${UNWIND_BINARY_DIR}
COMMAND touch ${UNWIND_CONFIGURE}
COMMAND $(MAKE) -C ${UNWIND_BINARY_DIR} install
WORKING_DIRECTORY ${UNWIND_BINARY_DIR})
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")

#add_custom_target(dependencies ALL DEPENDS ${fftw_BINARY_DIR}/BuildDone ${fftw-double_BINARY_DIR}/BuildDone)

add_dependencies(lapack openblas)
add_dependencies(arpack openblas lapack)
