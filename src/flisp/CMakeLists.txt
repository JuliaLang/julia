include_directories(
  ${SUPPORT_DIR}
  ${FLISP_DIR}
  ${LIBUV_INCLUDE_DIR}
  ${LIBMOJIBAKE_INCLUDE_DIR}
)

add_library (libflisp STATIC

  flisp.c builtins.c string.c equalhash.c table.c iostream.c
  julia_extensions.c 

  equalhash.h flisp.h opcodes.h
)

# these prereqs aren't built directly
set_property (
  SOURCE flisp.c
  APPEND PROPERTY OBJECT_DEPENDS 
  ${FLISP_DIR}/cvalues.c 
  ${FLISP_DIR}/types.c 
  ${FLISP_DIR}/print.c 
  ${FLISP_DIR}/read.c 
  ${FLISP_DIR}/equal.c
)

# since we can't use the same target name twice...
set_property (TARGET libflisp	PROPERTY OUTPUT_NAME flisp)

target_link_libraries (libflisp 
  ${LIBMOJIBAKE_LIBRARY} support dl pthread
)

add_executable        (flisp flmain.c flisp.h)
target_link_libraries (flisp libflisp)

# make boot image
add_custom_command (
  TARGET flisp
  POST_BUILD
  COMMAND cp flisp.boot flisp.boot.bak
  COMMAND ./flisp mkboot0.lsp system.lsp compiler.lsp > flisp.boot.new
  COMMAND mv flisp.boot.new flisp.boot
  COMMAND ./flisp mkboot1.lsp
  WORKING_DIRECTORY ${FLISP_DIR}
)
