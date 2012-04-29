cmake_minimum_required(VERSION 2.8)
project(amos)

enable_language(Fortran)

# default to release build
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME STREQUAL "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -fPIC")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0")
else()
  message(WARNING "Using unknown Fortran compiler")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -fPIC")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif()

# Don't use AUX_SOURCE_FILES for the reasons listed in the manual
set(SOURCES d1mach.f
	dgamln.f
	i1mach.f
	xerror.f
	zabs.f
	zacai.f
	zacon.f
	zairy.f
	zasyi.f	
	zbesh.f
	zbesi.f
	zbesj.f
	zbesk.f
	zbesy.f
	zbinu.f
	zbiry.f
	zbknu.f
	zbuni.f
	zbunk.f
	zdiv.f
	zexp.f
	zkscl.f
	zlog.f
	zmlri.f
	zmlt.f	
	zrati.f
	zs1s2.f
	zseri.f
	zshch.f
	zsqrt.f
	zuchk.f
	zunhj.f
	zuni1.f
	zuni2.f
	zunik.f
	zunk1.f
	zunk2.f
	zuoik.f	
	zwrsk.f)
	
add_library(amos SHARED ${SOURCES})