#

jl_set_option(USEIFC Off)
jl_set_option(USEGFORTRAN Off)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" OR
    CMAKE_Fortran_COMPILER_ID STREQUAL "G95")
  jl_set_option(USEGFORTRAN On)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  jl_set_option(USEIFC On)
else()
  # TODO
  message(FATAL_ERROR
    "Unsupported Fortran compiler ${CMAKE_Fortran_COMPILER_ID}")
endif()
