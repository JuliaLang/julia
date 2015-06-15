find_package(Git QUIET)

if(GIT_FOUND)
  set(NO_GIT_DEF Off)
else()
  set(NO_GIT_DEF On)
endif()

jl_option(NO_GIT "Do not use git during the build" "${NO_GIT_DEF}")
if(NOT NO_GIT AND NOT IS_DIRECTORY "${CMAKE_SOURCE_DIR}/.git")
  message(WARNING "git information unavailable; versioning information limited")
  set(NO_GIT Off CACHE INTERNAL "" FORCE)
  jl_set_option(NO_GIT Off)
endif()

# TODO? Maybe make VERSION a configure file rather than reading it

file(READ "${CMAKE_SOURCE_DIR}/VERSION" JULIA_VERSION)
# Trick to trigger re-configure when VERSION is edited.
configure_file(VERSION VERSION.cpy COPYONLY)
string(STRIP "${JULIA_VERSION}" JULIA_VERSION)

jl_set_make_flag(JULIA_VERSION "${JULIA_VERSION}")

if(NO_GIT)
  set(JULIA_COMMIT "${JULIA_VERSION}")
else()
  execute_process(COMMAND "${GIT_EXECUTABLE}" rev-parse --short=10 HEAD
    OUTPUT_VARIABLE JULIA_COMMIT
    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

jl_set_make_flag(JULIA_COMMIT "${JULIA_COMMIT}")

if(NOT "${JULIA_VERSION}" MATCHES
    "^([0-9]+)\\.([0-9]+)\\.([0-9]+)(|-.*)$")
  message(FATAL_ERROR "Invalid version string")
endif()

set(JULIA_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(JULIA_VERSION_MINOR "${CMAKE_MATCH_2}")
set(JULIA_VERSION_PATCH "${CMAKE_MATCH_2}")
if(CMAKE_MATCH_3 STREQUAL "")
  set(JULIA_VERSION_IS_RELEASE 1)
else()
  set(JULIA_VERSION_IS_RELEASE 0)
endif()

set(VERSDIR "v${JULIA_VERSION_MAJOR}.${JULIA_VERSION_MINOR}")

# TODO: Code bundled with Julia should be installed into a versioned directory,
# prefix/share/julia/VERSDIR, so that in the future one can have multiple
# major versions of Julia installed concurrently. Third-party code that
# is not controlled by Pkg should be installed into
# prefix/share/julia/site/VERSDIR (not prefix/share/julia/VERSDIR/site ...
# so that prefix/share/julia/VERSDIR can be overwritten without touching
# third-party code).

jl_set_make_flag(VERSDIR "${VERSDIR}")
