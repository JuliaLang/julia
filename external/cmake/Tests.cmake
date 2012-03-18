## MACRO adapted from http://www.opensource.apple.com/source/curl/curl-57.2/curl/CMakeLists.txt licensed under the MIT license
macro(COMPILE_TEST TEST_NAME)
  if(NOT DEFINED(${TESE_NAME}))
    set(MACRO_CHECK_FUNCTION_DEFINITIONS
      "-D${TEST_NAME} ${TEST_DEFINES} ${CMAKE_REQUIRED_FLAGS}")
    if(CMAKE_REQUIRED_LIBRARIES)
      set(TEST_NAME_ADD_LIBRARIES
        "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    endif(CMAKE_REQUIRED_LIBRARIES)

    message(STATUS "Performing Test ${TEST_NAME}")
    try_compile(${TEST_NAME}
      ${CMAKE_BINARY_DIR}
      ${CMAKE_SOURCE_DIR}/cmake/tests.c
      CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
      "${TEST_NAME_ADD_LIBRARIES}"
      OUTPUT_VARIABLE OUTPUT)
    if(${TEST_NAME})
      set(${TEST_NAME} 1 CACHE INTERNAL "Test ${FUNCTION}" PARENT_SCOPE)
      message(STATUS "Performing Test ${TEST_NAME} - Success")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Performing Curl Test ${TEST_NAME} passed with the following output:\n"
        "${OUTPUT}\n")
    else(${TEST_NAME})
      message(STATUS "Performing Test ${TEST_NAME} - Failed" PARENT_SCOPE)
      set(${TEST_NAME} "" CACHE INTERNAL "Test ${FUNCTION}")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Performing Curl Test ${TEST_NAME} failed with the following output:\n"
        "${OUTPUT}\n")
    endif()
  else()
    message(STATUS "Skipped test ${TEST_NAME}")
    message(STATUS "${${TEST_NAME}}")
  endif()
endmacro()
