include_directories (${LIBUV_INCLUDE_DIR})

add_definitions (
  -std=gnu99
)

add_library (support STATIC

  hashing.c timefuncs.c ptrhash.c operators.c	utf8.c ios.c htable.c 
  bitvector.c	int2str.c libsupportinit.c arraylist.c strtod.c

  arraylist.h dtypes.h ios.h platform.h  timefuncs.h bitvector.h 
  hashing.h libsupport.h ptrhash.h utf8.h dirpath.h htable.h
  MurmurHash3.h strtod.h utils.h

)

target_link_libraries (support ${LIBUV_LIBRARY})


