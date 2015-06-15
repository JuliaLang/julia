#

# libc++ is standard on OS X 10.9, but not for earlier releases
jl_option(USE_LIBCPP "Use libc++" Off)
# assume we don't have LIBSSP support in our compiler,
# will enable later if likely true
set(HAVE_SSP Off)

jl_set_option(USEICC Off)
jl_set_option(USEGCC Off)
jl_set_option(USECLANG Off)
jl_set_option(USEMSVC Off)

push_c_flags(CMAKE_CXX_FLAGS -std=c++11)

if(CMAKE_C_COMPILER_ID STREQUAL "AppleClang" OR
    CMAKE_C_COMPILER_ID STREQUAL "Clang")
  jl_set_option(USECLANG On)
  add_definitions(-pipe -fPIC)
  push_c_flags(CMAKE_C_FLAGS -fno-strict-aliasing -D_FILE_OFFSET_BITS=64)
  push_c_flags(CMAKE_CXX_FLAGS -fno-rtti)
  push_c_flags(CMAKE_C_FLAGS_DEBUG -O0 -g -DJL_DEBUG_BUILD
    -fstack-protector-all)
  push_c_flags(CMAKE_CXX_FLAGS_DEBUG -O0 -g -DJL_DEBUG_BUILD
    -fstack-protector-all)
  push_c_flags(CMAKE_C_FLAGS_RELEASE -O3 -g)
  push_c_flags(CMAKE_CXX_FLAGS_RELEASE -O3 -g)
  if(APPLE)
    if(USE_LIBCPP)
      add_definitions(-stdlib=libc++ -mmacosx-version-min=10.7)
    else()
      add_definitions(-mmacosx-version-min=10.6)
    endif()
    add_definitions(-D_LARGEFILE_SOURCE -D_DARWIN_USE_64_BIT_INODE=1)
  endif()
elseif(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  jl_set_option(USEGCC On)
  add_definitions(-pipe -fPIC)
  push_c_flags(CMAKE_C_FLAGS -std=gnu99 -fno-strict-aliasing
    -D_FILE_OFFSET_BITS=64)
  push_c_flags(CMAKE_CXX_FLAGS -fno-rtti)
  push_c_flags(CMAKE_C_FLAGS_DEBUG -O0 -ggdb3 -DJL_DEBUG_BUILD
    -fstack-protector-all)
  push_c_flags(CMAKE_CXX_FLAGS_DEBUG -O0 -ggdb3 -DJL_DEBUG_BUILD
    -fstack-protector-all)
  push_c_flags(CMAKE_C_FLAGS_RELEASE -O3 -ggdb3 -falign-functions)
  push_c_flags(CMAKE_CXX_FLAGS_RELEASE -O3 -ggdb3 -falign-functions)
elseif(CMAKE_C_COMPILER_ID STREQUAL "Intel")
  jl_set_option(USEICC On)
  add_definitions(-pipe -fPIC)
  push_c_flags(CMAKE_C_FLAGS -std=gnu99 -fno-strict-aliasing
    -D_FILE_OFFSET_BITS=64 -fp-model precise -fp-model except -no-ftz)
  push_c_flags(CMAKE_CXX_FLAGS -fno-rtti)
  push_c_flags(CMAKE_C_FLAGS_DEBUG -O0 -g -DJL_DEBUG_BUILD
    -fstack-protector-all)
  push_c_flags(CMAKE_CXX_FLAGS_DEBUG -O0 -g -DJL_DEBUG_BUILD
    -fstack-protector-all)
  push_c_flags(CMAKE_C_FLAGS_RELEASE -O3 -g -falign-functions)
  push_c_flags(CMAKE_CXX_FLAGS_RELEASE -O3 -g -falign-functions)
elseif(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
  jl_set_option(USEMSVC On)
else()
  # TODO
  message(FATAL_ERROR "Unsupported compiler ${CMAKE_C_COMPILER_ID}")
endif()

include_directories("${CMAKE_BINARY_DIR}/include")

if(NOT USECLANG)
  if(USE_LIBCPP)
    message(FATAL_ERROR
      "USE_LIBCPP only supported with clang. Try setting USE_LIBCPP=0")
  endif()
  if(SANITIZE)
    message(FATAL_ERROR
      "Address Sanitizer only supported with clang. Try setting SANITIZE=0")
  endif()
endif()

if(NOT USEMSVC)
  set(CPP_COMMAND ${CMAKE_C_COMPILER} -E -P)
else()
  set(CPP_COMMAND ${CMAKE_C_COMPILER} -EP -E)
endif()
