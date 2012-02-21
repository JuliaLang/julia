cmake_minimum_required(VERSION 2.8)
## library versions ##

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

## General Setup ##



if(NOT DEFINED CMAKE_INSTALL_PREFIX)
	set(CMAKE_INSTALL_PREFIX "root")
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
set(CMAKE_BUILD_TYPE "RELEASE")
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

# Neither user specified or optimized BLAS libraries can be used
if(NOT BLAS_FOUND)
  message(STATUS "Using OpenBlas")
  set( BLAS_LIBRARIES ${CMAKE_SOURCE_DIR}/openblas-${openblas_VER}/libopenblas.lib)
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
set(BLAS_FOUND 1) #Prevent LaPack from using it's own blas version
add_subdirectory(lapack-${lapack_VER})

add_subdirectory(amos)
add_subdirectory(fdlibm)
