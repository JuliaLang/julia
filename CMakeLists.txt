cmake_minimum_required (VERSION 2.8)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include (cmake/utils.cmake)

disallow_intree_builds()

project (julia C CXX)

# convenient variables
set (TOP_DIR     "${CMAKE_SOURCE_DIR}")
set (SRC_DIR     "${TOP_DIR}/src")
set (SUPPORT_DIR "${SRC_DIR}/support")
set (FLISP_DIR   "${SRC_DIR}/flisp")
set (UI_DIR      "${TOP_DIR}/ui")

# dependencies
find_package (LibUV       REQUIRED)
find_package (LibMojiBake REQUIRED)
find_package (LLVM        REQUIRED)

# subdirectories
add_subdirectory (${SUPPORT_DIR})
add_subdirectory (${FLISP_DIR})
add_subdirectory (${SRC_DIR})
add_subdirectory (${UI_DIR})
