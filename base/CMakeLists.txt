#

# TODO JULIA_CPU_TARGET
set(JULIA_CPU_TARGET native)

jl_custom_target(julia-inference0
  inference0.o
  coreimg.jl julia-ui
  inference0.fnames
  COMMAND "${build_bindir}/julia" -C ${JULIA_CPU_TARGET}
  --output-o "${CMAKE_CURRENT_BINARY_DIR}/inference0.o"
  --load-log "${CMAKE_CURRENT_BINARY_DIR}/inference0.fnames" -f coreimg.jl
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/dummy.c")
  execute_process(COMMAND "${CMAKE_COMMAND}" -E
    touch "${CMAKE_CURRENT_BINARY_DIR}/dummy.c")
endif()

add_library(julia-inference0-so MODULE "${CMAKE_CURRENT_BINARY_DIR}/dummy.c")

target_link_libraries(julia-inference0-so
  "${CMAKE_CURRENT_BINARY_DIR}/inference0.o"
  libjulia)

set_target_properties(julia-inference0-so PROPERTIES
  OUTPUT_NAME inference0
  PREFIX "")

add_dependencies(julia-inference0-so julia-inference0)

# TODO? use inference.so if it exists
jl_custom_target(julia-inference
  "${build_private_libdir}/inference.o"
  "coreimg.jl;${CMAKE_CURRENT_BINARY_DIR}/inference0.o"
  "julia-ui;julia-inference0-so"
  inference.fnames
  COMMAND "${build_bindir}/julia" -C ${JULIA_CPU_TARGET}
  --output-o "${build_private_libdir}/inference.o" -f
  --load-log "${CMAKE_CURRENT_BINARY_DIR}/inference.fnames"
  -J "${CMAKE_CURRENT_BINARY_DIR}/inference0.so" coreimg.jl
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

add_library(julia-inference-so MODULE "${CMAKE_CURRENT_BINARY_DIR}/dummy.c")

target_link_libraries(julia-inference-so
  "${build_private_libdir}/inference.o"
  libjulia)

set_target_properties(julia-inference-so PROPERTIES
  OUTPUT_NAME inference
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY "${build_private_libdir}")

add_dependencies(julia-inference-so julia-inference)

set(errno_h_jl "${CMAKE_CURRENT_BINARY_DIR}/errno_h.jl")

add_custom_command(OUTPUT errno_h.jl
  COMMAND "${CMAKE_COMMAND}" "-DOUTPUT_FILE=${errno_h_jl}"
  "-DCPP_COMMAND=${CPP_COMMAND}" "-DCUR_BIN_DIR=${CMAKE_CURRENT_BINARY_DIR}"
  "-DCUR_SRC_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
  -P errno_h.jl.cmake
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  DEPENDS errno_h.jl.cmake VERBATIM)

add_custom_target(julia-errno_h.jl
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/errno_h.jl")
