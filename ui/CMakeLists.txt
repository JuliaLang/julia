#

set(julia_ui_SRCS
  repl.c)

if(USEMSVC)
  set(julia_ui_SRCS
    ${julia_ui_SRCS}
    getopt.c)
endif()

include_directories(../src
  ../src/support
  ${build_includedir})

if(NOT USEMSVC)
  add_definitions(-Wall -Wno-strict-aliasing -fno-omit-frame-pointer)
endif()

# TODO llvm_shlib...
# push_c_flags(JL_LD_FLAGS ${NO_WHOLE_ARCHIVE} ${OSLIBS})

if(NOT USE_SYSTEM_LIBM AND UNTRUSTED_SYSTEM_LIBM)
  push_c_flags(JL_LD_FLAGS ${WHOLE_ARCHIVE} ${build_libdir}/libopenlibm.a
    ${NO_WHOLE_ARCHIVE})
endif()

if(WIN32)
  push_c_flags(JL_LD_FLAGS -municode)
endif()

add_executable(julia-ui ${julia_ui_SRCS})
set_target_properties(julia-ui PROPERTIES
  OUTPUT_NAME julia
  RUNTIME_OUTPUT_DIRECTORY "${build_bindir}")

string(REPLACE " " ";" ldflags "${JL_LD_FLAGS}")

target_link_libraries(julia-ui ${ldflags} libjulia)

if(UNIX)
  add_custom_command(OUTPUT "${CMAKE_BINARY_DIR}/julia"
    COMMAND "${CMAKE_COMMAND}" -E create_symlink "${build_bindir}/julia"
    "${CMAKE_BINARY_DIR}/julia")
  add_custom_target(julia_symlink DEPENDS "${CMAKE_BINARY_DIR}/julia")
else()
  add_custom_target(julia_symlink)
endif()

if(WIN32 AND NOT USEMSVC)
  set(windows_julia_rc "${CMAKE_SOURCE_DIR}/contrib/windows/julia.rc")
  set(julia_res_o "${CMAKE_CURRENT_BINARY_DIR}/julia_res.o")
  set(jlver
    "${JULIA_VERSION_MAJOR},${JULIA_VERSION_MINOR},0,${JULIA_VERSION_PATCH}")
  add_custom_command(OUTPUT "${julia_res_o}"
    COMMAND windres "${windows_julia_rc}" -O coff -o "${julia_res_o}"
    -DJLVER=${jlver} "-DJLVER_STR=\"${JULIA_VERSION}\""
    DEPENDS "${windows_julia_rc}")
  add_custom_target(julia_res_o DEPENDS "${julia_res_o}")
  add_dependencies(julia-ui julia_res_o)
  target_link_libraries("${julia_res_o}")
endif()
