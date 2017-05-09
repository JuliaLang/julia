include_directories(
  ${SUPPORT_DIR}
  ${FLISP_DIR}
  ${LIBUV_INCLUDE_DIR}
)

add_definitions (
  -std=gnu99
  -D_GNU_SOURCE
  -D__STDC_CONSTANT_MACROS
  -D__STDC_LIMIT_MACROS
  -DJL_SYSTEM_IMAGE_PATH="./sys.ji"
  ${LLVM_CFLAGS}
  ${LLVM_CXXFLAGS}
)

add_library (libjulia 

  jltypes.c gf.c ast.c builtins.c module.c interpreter.c
  alloc.c dlload.c sys.c init.c task.c array.c dump.c toplevel.c 
  jl_uv.c jlapi.c profile.c 
  codegen.cpp disasm.cpp debuginfo.cpp llvm-simdloop.cpp

  julia.h julia_internal.h options.h

)

# extra "includes" for codegen
set_property (
  SOURCE codegen.cpp
  APPEND PROPERTY OBJECT_DEPENDS
  ${SRC_DIR}/intrinsics.cpp
  ${SRC_DIR}/cgutils.cpp
  ${SRC_DIR}/ccall.cpp
)

set_property (TARGET libjulia PROPERTY OUTPUT_NAME julia)

target_link_libraries (libjulia
  ${LIBMOJIBAKE_LIBRARY} support
)
