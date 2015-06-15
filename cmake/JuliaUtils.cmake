#

get_filename_component(jl_cmake_utils_dir
  "${CMAKE_CURRENT_LIST_FILE}" PATH)

set(jl_cmake_utils_dir "${jl_cmake_utils_dir}" CACHE "" INTERNAL FORCE)

function(jl_rewrite_dep_list var deps)
  set(new_deps)
  foreach(dep ${deps})
    if(TARGET "${dep}")
      set(new_deps ${new_deps} "$<TARGET_FILE:${dep}>")
    elseif(IS_ABSOLUTE "${dep}")
      set(new_deps ${new_deps} ${dep})
    else()
      get_filename_component(dep "${dep}" ABSOLUTE)
      set(new_deps ${new_deps} ${dep})
    endif()
  endforeach()
  set(${var} "${new_deps}" PARENT_SCOPE)
endfunction()

function(jl_rewrite_output_list var outputs)
  set(new_outputs)
  foreach(output ${outputs})
    if(IS_ABSOLUTE "${output}")
      set(new_outputs ${new_outputs} ${output})
    else()
      set(new_outputs ${new_outputs} "${CMAKE_CURRENT_BINARY_DIR}/${output}")
    endif()
  endforeach()
  set(${var} "${new_outputs}" PARENT_SCOPE)
endfunction()

function(jl_custom_target target outputs output_deps target_deps autodep_file)
  jl_rewrite_dep_list(output_deps "${output_deps}")
  jl_rewrite_output_list(outputs "${outputs}")
  if((NOT "${autodep_file}" STREQUAL "") AND
      (NOT IS_ABSOLUTE "${autodep_file}"))
    set(autodep_file "${CMAKE_CURRENT_BINARY_DIR}/${autodep_file}")
  endif()
  add_custom_target("${target}" ALL
    COMMAND "${CMAKE_COMMAND}"
    "-DOUTPUTS=${outputs}" "-DOUTPUT_DEPS=${output_deps}"
    "-DAUTODEP_FILE=${autodep_file}" "-DCOMMAND_SPEC=${ARGN}"
    "-DCUR_BIN_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "-DCUR_SRC_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    -P "${jl_cmake_utils_dir}/jl_custom_target.cmake"
    DEPENDS ${target_deps} VERBATIM)
endfunction()
