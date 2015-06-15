#

add_subdirectory(support)
add_subdirectory(flisp)

set(FLISP_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/flisp/flisp")

set(julia_flisp_boot_SRCS
  julia-parser.scm
  julia-syntax.scm
  match.scm
  utils.scm
  jlfrontend.scm
  mk_julia_flisp_boot.scm)

set(julia_flisp_boot "${CMAKE_CURRENT_BINARY_DIR}/julia_flisp.boot")

add_custom_command(OUTPUT "${julia_flisp_boot}"
  COMMAND "${CMAKE_COMMAND}" -E env "julia_flisp.boot=${julia_flisp_boot}"
  "${FLISP_EXECUTABLE}" ./mk_julia_flisp_boot.scm
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  DEPENDS ${julia_flisp_boot_SRCS} flmain)

add_custom_target(julia_flisp_boot
  DEPENDS "${julia_flisp_boot}" flmain)

set(julia_flisp_boot_inc "${CMAKE_CURRENT_BINARY_DIR}/julia_flisp.boot.inc")

#TODO make the input and output arguments or envs
add_custom_command(OUTPUT "${julia_flisp_boot_inc}"
  COMMAND "${FLISP_EXECUTABLE}" ./bin2hex.scm < "${julia_flisp_boot}"
  > "${julia_flisp_boot_inc}"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  DEPENDS "${julia_flisp_boot}" flmain)

add_custom_target(julia_flisp_boot_inc
  DEPENDS "${julia_flisp_boot_inc}" julia_flisp_boot)

set(libjulia_SRCS
  jltypes.c
  gf.c
  ast.c
  builtins.c
  module.c
  codegen.cpp
  disasm.cpp
  debuginfo.cpp
  interpreter.c
  alloc.c
  dlload.c
  sys.c
  init.c
  task.c
  array.c
  dump.c
  toplevel.c
  jl_uv.c
  jlapi.c
  profile.c
  llvm-simdloop.cpp
  simplevector.c
  gc.c)

push_c_flags(CMAKE_CXX_FLAGS ${LLVM_CXXFLAGS})

if(USE_LLVM_SHLIB)
  set(LLVM_LINK ${LLVM_LDFLAGS} -lLLVM-${LLVM_VERSION})
  add_definitions(-DLLVM_SHLIB)
else()
  set(LLVM_LINK
    ${LLVM_LDFLAGS} ${LLVM_LIBS} ${LLVM_LDFLAGS} ${LLVM_SYSTEMLIBS})
endif()

include_directories(. "${CMAKE_CURRENT_BINARY_DIR}"
  flisp support
  ../deps/valgrind
  "${LIBUV_INC}"
  "${LLVM_INC}")

add_definitions(-D_GNU_SOURCE -DLIBRARY_EXPORTS)
if(NOT USEMSVC)
  add_definitions(-Wall -Wno-strict-aliasing -fno-omit-frame-pointer
    -fvisibility=hidden -fno-common)
endif()

if(USE_COPY_STACKS)
  add_definitions(-DCOPY_STACKS)
endif()

add_library(libjulia SHARED ${libjulia_SRCS})

set_target_properties(libjulia PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${build_shlibdir}"
  OUTPUT_NAME "julia")

add_dependencies(libjulia julia-deps julia_flisp_boot_inc)

string(REPLACE " " ";" OS_LIBS "${OSLIBS}")

target_link_libraries(libjulia ${WHOLE_ARCHIVE} flisp support
  -L${build_shlibdir} -L${build_libdir} ${LIBUV} ${LIBUTF8PROC}
  ${NO_WHOLE_ARCHIVE} ${LLVM_LINK} ${OS_LIBS}
  -Wl,--no-undefined)
