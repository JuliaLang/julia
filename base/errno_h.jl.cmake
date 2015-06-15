#!/usr/bin/cmake -P

# OUTPUT_FILE
# CPP_COMMAND
# CUR_BIN_DIR
# CUR_SRC_DIR

file(REMOVE "${CUR_BIN_DIR}/errno_h.jl.tmp1")

file(WRITE "${CUR_BIN_DIR}/errno_h.jl.tmp1" "#include \"errno.h\"")

execute_process(COMMAND ${CPP_COMMAND} -dM -
  RESULT_VARIABLE ret
  OUTPUT_VARIABLE output_str
  INPUT_FILE "${CUR_BIN_DIR}/errno_h.jl.tmp1")

if(NOT ${ret} EQUAL 0)
  message(FATAL_ERROR "")
endif()

file(REMOVE "${CUR_BIN_DIR}/errno_h.jl.tmp1")

string(REPLACE "\n" ";" output_str "${output_str}")

set(names)

foreach(line ${output_str})
  # TODO? handle `#define E... E...`?
  if(${line} MATCHES "^#define[ \t]+(E[a-zA-Z0-9]+)[ \t]+([0-9]+)[ \t]*\$")
    set(names ${names} ${CMAKE_MATCH_1})
    set("errno_${CMAKE_MATCH_1}" ${CMAKE_MATCH_2})
  endif()
endforeach()

list(SORT names)

file(REMOVE "${OUTPUT_FILE}")

foreach(name ${names})
  file(APPEND "${OUTPUT_FILE}"
    "const ${name} = Int32(${errno_${name}})\n")
endforeach()
