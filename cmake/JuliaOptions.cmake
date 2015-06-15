#

## Temporary functions to generate make flags from cmake options
set(jl_make_option_names CACHE INTERNAL "" FORCE)

function(jl_set_make_flag name val)
  set(jl_make_option_names ${jl_make_option_names}
    ${name} CACHE INTERNAL "" FORCE)
  set("_jl_make_flag_${name}" "${val}" CACHE INTERNAL "" FORCE)
endfunction()

function(jl_get_make_flags var)
  set(option_names "${jl_make_option_names}")
  list(REMOVE_DUPLICATES option_names)
  set(flags)
  foreach(opt ${option_names})
    set(flags ${flags} "${opt}=${_jl_make_flag_${opt}}")
  endforeach()
  set(${var} ${flags} PARENT_SCOPE)
endfunction()

function(jl_set_option name val)
  if(${val})
    jl_set_make_flag(${name} 1)
  else()
    jl_set_make_flag(${name} 0)
  endif()
endfunction()

function(jl_option opt doc val)
  set(jl_make_options ${jl_make_options} ${opt})
  option("${opt}" "${doc}" "${val}")
  jl_set_option(${opt} ${${opt}})
endfunction()

function(jl_str_option name doc def_val)
  if(DEFINED "${name}")
    set("${name}_DEFINED" On CACHE INTERNAL "" FORCE)
  else()
    set("${name}_DEFINED" Off CACHE INTERNAL "" FORCE)
    set("${name}" "${def_val}" CACHE INTERNAL "" FORCE)
  endif()
  jl_set_make_flag("${name}" "${${name}}")
endfunction()

function(use_system_opt lib)
  jl_option("USE_SYSTEM_${lib}"
    "Use ${lib} available on the system instead of building it" Off)
endfunction()

function(push_c_flags var)
  set(res "${${var}}")
  foreach(i RANGE 1 ${ARGC})
    set(res "${res} ${ARGV${i}}")
  endforeach()
  set(${var} "${res}" PARENT_SCOPE)
endfunction()
