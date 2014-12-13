include (./utils)

find_program (LLVM_CONFIG_EXECUTABLE llvm-config
  PATHS
  "${CMAKE_CURRENT_LIST_DIR}/usr/bin"
)

function (llvm_config outvar option)
  execute_process (
    COMMAND ${LLVM_CONFIG_EXECUTABLE} ${option}
    OUTPUT_VARIABLE ${outvar}
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
endfunction()

llvm_config (LLVM_INCLUDE_DIR     --includedir)
llvm_config (LLVM_LIBRARY_DIR     --libdir)
llvm_config (LLVM_LIBRARY_NAMES   --libnames)
llvm_config (LLVM_CPPFLAGS        --cppflags)
llvm_config (LLVM_CFLAGS          --cflags)
llvm_config (LLVM_CXXFLAGS        --cxxflags)

prepend_string (LLVM_LIBRARIES ${LLVM_LIBRARY_DIR}/ ${LLVM_LIBRARY_NAMES})
