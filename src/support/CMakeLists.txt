#

set(libsupport_SRCS
  hashing.c
  timefuncs.c
  ptrhash.c
  operators.c
  utf8.c
  ios.c
  htable.c
  bitvector.c
  int2str.c
  libsupportinit.c
  arraylist.c
  strtod.c)

if(WIN32)
  set(libsupport_SRCS ${libsupport_SRCS} asprintf.c)
  enable_language(ASM_MASM)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(libsupport_SRCS ${libsupport_SRCS} _setjmp.win64.S _longjmp.win64.S)
    set_source_files_properties(_setjmp.win64.S _longjmp.win64.S PROPERTIES
      LANGUAGE ASM_MASM)
  else()
    set(libsupport_SRCS ${libsupport_SRCS} _setjmp.win32.S _longjmp.win32.S)
    set_source_files_properties(_setjmp.win32.S _longjmp.win32.S PROPERTIES
      LANGUAGE ASM_MASM)
  endif()
endif()

add_definitions(-DLIBRARY_EXPORTS -DUTF8PROC_EXPORTS)
include_directories(${LIBUV_INC} ${UTF8PROC_INC})
if(NOT USEMSVC)
  add_definitions(-Wall -Wno-strict-aliasing -fvisibility=hidden)
endif()

add_library(support STATIC
  ${libsupport_SRCS})

add_dependencies(support julia-deps)

target_link_libraries(support ${LIBUV} ${LIBUTF8PROC})
