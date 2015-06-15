#

add_custom_target(julia-deps
  COMMAND make ${jl_make_flags}
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  DEPENDS juliarc-jl julia-man1)
