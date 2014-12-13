find_path (LIBMOJIBAKE_INCLUDE_DIR mojibake.h
  PATHS
  "${CMAKE_CURRENT_LIST_DIR}/../usr/include"
)

find_library (LIBMOJIBAKE_LIBRARY mojibake
  "${CMAKE_CURRENT_LIST_DIR}/../usr/lib"
)