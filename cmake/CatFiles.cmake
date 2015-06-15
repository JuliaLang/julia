#

set(content)

foreach(i RANGE 1 ${NSRC})
  file(READ "${SRC${i}}" c)
  set(content "${content}${c}")
endforeach()

file(WRITE "${DST}" "${content}")
