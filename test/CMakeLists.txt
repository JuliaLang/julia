#

set(libccalltest_SRCS
  ccalltest.c)

add_definitions("-DCC=${CMAKE_C_COMPILER}")

# TODO make ccall test work with this
add_library(libccalltest MODULE ${libccalltest_SRCS})
