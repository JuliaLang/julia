find_path (LIBUV_INCLUDE_DIR uv.h
  PATHS
  "${CMAKE_CURRENT_LIST_DIR}/../usr/include"
)

find_library (LIBUV_LIBRARY uv
  "${CMAKE_CURRENT_LIST_DIR}/../usr/lib"
)