project(Julia C CXX)

## Setup cmake paths
cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(JuliaUtils)

# Utilities for defining options
include(JuliaOptions)

# Set versions
include(JuliaVersion)

# Architecture and compiler/flags
jl_str_option(ARCH "" "")
jl_set_option(JL_IS_MINGW "${MINGW}")
jl_set_option(JL_IS_MSYS "${MSYS}")

include(JuliaDirs)
include(JuliaCCompiler)

# TODO only enable if necessary
# enable_language(Fortran)
# include(JuliaFortranCompiler)

# OPENBLAS build options
jl_str_option(OPENBLAS_TARGET_ARCH "OpenBLAS Arch" "")
jl_str_option(OPENBLAS_SYMBOLSUFFIX "OpenBLAS symbol suffix" "")

# If OPENBLAS_TARGET_ARCH is set, we default to disabling OPENBLAS_DYNAMIC_ARCH
if(OPENBLAS_TARGET_ARCH STREQUAL "")
  jl_option(OPENBLAS_DYNAMIC_ARCH "" Off)
else()
  jl_option(OPENBLAS_DYNAMIC_ARCH "" On)
endif()
jl_option(OPENBLAS_USE_THREAD "" On)

# use_system options
use_system_opt(LLVM)
use_system_opt(LIBUNWIND)
use_system_opt(PCRE)
use_system_opt(LIBM)
use_system_opt(OPENLIBM)
jl_option(UNTRUSTED_SYSTEM_LIBM "Do not trust system libm" Off)
use_system_opt(OPENSPECFUN)
use_system_opt(DSFMT)
use_system_opt(BLAS)
use_system_opt(LAPACK)
use_system_opt(FFTW)
use_system_opt(GMP)
use_system_opt(MPFR)
use_system_opt(ARPACK)
use_system_opt(SUITESPARSE)
use_system_opt(RMATH)
use_system_opt(LIBUV)
use_system_opt(UTF8PROC)
use_system_opt(LIBGIT2)
use_system_opt(PATCHELF)

jl_option(USE_GPL_LIBS "Whether to use GPL libraries or not." On)

# LLVM related options
jl_option(USE_LLVM_SHLIB "Link to the LLVM shared library" Off)
jl_option(LLVM_ASSERTIONS "" Off)
jl_option(LLVM_DEBUG "" Off)
# Enable to get lldb (often does not work, no chance with llvm3.2 and earlier)
# see http://lldb.llvm.org/build.html for dependencies
jl_option(BUILD_LLDB "Build lldb" Off)
jl_option(BUILD_LLVM_CLANG "Build clang and compiler-rt" ${BUILD_LLDB})

option(USE_COPY_STACKS "" On)

include(JuliaDeps)

## Settings for various Intel tools
jl_option(USE_INTEL_MKL "Use MKL" Off)
jl_option(USE_INTEL_MKL_FFT "Use MKL FFT" Off)
jl_option(USE_INTEL_LIBM "Use Intel LIBM" Off)
jl_option(USE_INTEL_JITEVENTS
  "Enable profiling with Intel VTune Amplifier" Off)

jl_set_make_flag(CMAKE "${CMAKE_COMMAND}")

set(JL_SYSTEM_IMAGE_PATH
  "${build_private_libdir_rel}/sys${CMAKE_SHARED_LIBRARY_SUFFIX}")

configure_file(src/julia-config.h.in include/julia/julia-config.h @ONLY)
configure_file(src/julia-config-p.h.in include/julia/julia-config-p.h @ONLY)

# This is the trick in cmake to have proper dependency on a file
# For another command that depend on the output of this which needs to be
# updated automatically. The (custom) command should depend on the file output
# and the corresponding (custom) target should depend on this target.
if(WIN32 OR CYGWIN)
  add_custom_command(OUTPUT "${build_sysconfdir}/julia/juliarc.jl"
    COMMAND "${CMAKE_COMMAND}"
    "-DNSRC=2" "-DSRC1=${CMAKE_SOURCE_DIR}/etc/juliarc.jl"
    "-DSRC2=${CMAKE_SOURCE_DIR}/contrib/windows/juliarc.jl"
    "-DDST=${build_sysconfdir}/julia/juliarc.jl"
    -P "${CMAKE_SOURCE_DIR}/cmake/CatFiles.cmake"
    DEPENDS etc/juliarc.jl contrib/windows/juliarc.jl
    "${CMAKE_SOURCE_DIR}/cmake/CatFiles.cmake")
elseif(APPLE)
  add_custom_command(OUTPUT "${build_sysconfdir}/julia/juliarc.jl"
    COMMAND "${CMAKE_COMMAND}"
    "-DNSRC=2" "-DSRC1=${CMAKE_SOURCE_DIR}/etc/juliarc.jl"
    "-DSRC2=${CMAKE_SOURCE_DIR}/contrib/mac/juliarc.jl"
    "-DDST=${build_sysconfdir}/julia/juliarc.jl"
    -P "${CMAKE_SOURCE_DIR}/cmake/CatFiles.cmake"
    DEPENDS etc/juliarc.jl contrib/mac/juliarc.jl
    "${CMAKE_SOURCE_DIR}/cmake/CatFiles.cmake")
else()
  add_custom_command(OUTPUT "${build_sysconfdir}/julia/juliarc.jl"
    COMMAND "${CMAKE_COMMAND}" -E copy
    "${CMAKE_SOURCE_DIR}/etc/juliarc.jl"
    "${build_sysconfdir}/julia/juliarc.jl"
    DEPENDS etc/juliarc.jl)
endif()

add_custom_target(juliarc-jl
  DEPENDS "${build_sysconfdir}/julia/juliarc.jl")

add_custom_command(OUTPUT "${build_man1dir}/julia.1"
  COMMAND "${CMAKE_COMMAND}" -E copy
  "${CMAKE_SOURCE_DIR}/doc/man/julia.1"
  "${build_man1dir}/julia.1"
  DEPENDS doc/man/julia.1)

add_custom_target(julia-man1
  DEPENDS "${build_man1dir}/julia.1")

# Intel VTune Amplifier
if(USE_INTEL_JITEVENTS)
  push_c_flags(JLCPP_FLAGS -DJL_USE_INTEL_JITEVENTS)
endif()

# TODO
if(USE_SYSTEM_LIBUV)
  set(LIBUV /usr/lib/libuv-julia.a)
  set(LIBUV_INC /usr/include)
else()
  set(LIBUV "${build_libdir}/libuv.a")
  set(LIBUV_INC "${build_includedir}")
endif()

jl_set_make_flag(LIBUV "${LIBUV}")
jl_set_make_flag(LIBUV_INC "${LIBUV_INC}")

if(USE_SYSTEM_UTF8PROC)
  set(LIBUTF8PROC -lutf8proc)
  set(UTF8PROC_INC /usr/include)
else()
  set(LIBUTF8PROC "${build_libdir}/libutf8proc.a")
  set(UTF8PROC_INC "${build_includedir}")
endif()

if(USE_SYSTEM_LLVM)
  set(LLVM_CONFIG "llvm-config${CMAKE_EXECUTABLE_SUFFIX}")
  add_definitions(-DSYSTEM_LLVM)
else()
  set(LLVM_CONFIG "${build_bindir}/llvm-config${CMAKE_EXECUTABLE_SUFFIX}")
endif()

jl_set_make_flag(LLVM_CONFIG "${LLVM_CONFIG}")

# TODO FindLLVM
execute_process(COMMAND "${LLVM_CONFIG}" --includedir
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE LLVM_INC)
execute_process(COMMAND "${LLVM_CONFIG}" --ldflags
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE LLVM_LDFLAGS)
execute_process(COMMAND "${LLVM_CONFIG}" --libs
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE LLVM_LIBS)
execute_process(COMMAND "${LLVM_CONFIG}" --system-libs
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE LLVM_SYSTEMLIBS)
execute_process(COMMAND "${LLVM_CONFIG}" --version
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE LLVM_VERSION)
execute_process(COMMAND "${LLVM_CONFIG}" --cxxflags
  OUTPUT_STRIP_TRAILING_WHITESPACE
  OUTPUT_VARIABLE LLVM_CXXFLAGS)

if(APPLE)
  set(WHOLE_ARCHIVE "-Xlinker -all_load")
  set(NO_WHOLE_ARCHIVE)
elseif(NOT USEMSVC)
  set(WHOLE_ARCHIVE "-Wl,--whole-archive")
  set(NO_WHOLE_ARCHIVE "-Wl,--no-whole-archive")
endif()

jl_set_make_flag(WHOLE_ARCHIVE "${WHOLE_ARCHIVE}")
jl_set_make_flag(NO_WHOLE_ARCHIVE "${NO_WHOLE_ARCHIVE}")

if(WIN32)
  push_c_flags(LIBUNWIND)
elseif(USE_SYSTEM_LIBUNWIND)
  if(NOT APPLE)
    push_c_flags(LIBUNWIND -lunwind-generic -lunwind)
  endif()
elseif(APPLE)
  push_c_flags(LIBUNWIND ${build_libdir}/libosxunwind.a)
  add_definitions(-DLIBOSXUNWIND)
else()
  push_c_flags(LIBUNWIND ${build_libdir}/libunwind-generic.a
    ${build_libdir}/libunwind.a)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  push_c_flags(OSLIBS -ldl -lrt -lpthread -Wl,--export-dynamic
    -Wl,--no-whole-archive ${LIBUNWIND})
  # TODO
  if(NOT SANITIZE AND NOT SANITIZE_MEMORY AND NOT LLVM_SANITIZE)
    push_c_flags(-Wl,--version-script=${CMAKE_SOURCE_DIR}/src/julia.expmap)
  endif()
  push_c_flags(JL_LD_FLAGS -Wl,-Bdynamic)
  # TODO try compile?
  push_c_flags(CMAKE_SHARED_LINKER_FLAGS -Wl,-Bsymbolic-functions)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
  push_c_flags(JL_LD_FLAGS -Wl,-Bdynamic)
  push_c_flags(OSLIBS -lkvm -lrt -Wl,--export-dynamic
    -Wl,--version-script=${CMAKE_SOURCE_DIR}/src/julia.expmap
    ${NO_WHOLE_ARCHIVE} ${LIBUNWIND})
endif()

if(APPLE)
  push_c_flags(OSLIBS -ldl -Wl,-w -framework CoreFoundation
    -framework CoreServices ${LIBUNWIND})
  set(HAVE_SSP On)
endif()

if(WIN32)
  if(NOT USEMSVC)
    set(HAVE_SSP On)
    push_c_flags(OSLIBS -Wl,--export-all-symbols
      -Wl,--version-script=${CMAKE_SOURCE_DIR}/src/julia.expmap
      ${NO_WHOLE_ARCHIVE} -lpsapi -lkernel32 -lws2_32 -liphlpapi -lwinmm
      -ldbghelp)
    push_c_flags(JL_LD_FLAGS, -Wl,--stack,8388608)
    if(CMAKE_SIZEOF_VOID_P EQUAL 4)
      push_c_flags(JL_LD_FLAGS -Wl,--large-address-aware)
    endif()
  else() #USEMSVC
    push_c_flags(OSLIBS kernel32.lib ws2_32.lib psapi.lib advapi32.lib
      iphlpapi.lib shell32.lib winmm.lib)
    push_c_flags(JL_LD_FLAGS -stack:8388608)
  endif()
  add_definitions(-D_WIN32_WINNT=0x0502)
  set(UNTRUSTED_SYSTEM_LIBM On CACHE INTERNAL "" FORCE)
  jl_set_option(UNTRUSTED_SYSTEM_LIBM On)
endif()

if(HAVE_SSP)
  add_definitions(-DHAVE_SSP=1)
  if(USEGCC)
    push_c_flags(OSLIBS -lssp)
  endif()
endif()

jl_set_make_flag(CC "${CMAKE_C_COMPILER}")
jl_set_make_flag(CXX "${CMAKE_CXX_COMPILER}")
string(SUBSTRING "${CMAKE_SHARED_LIBRARY_SUFFIX}" 1 -1 SHLIB)
jl_set_make_flag(SHLIB_EXT "${SHLIB}")

jl_get_make_flags(jl_make_flags)

# message("${jl_make_flags}")

add_subdirectory(deps)
add_subdirectory(test)
add_subdirectory(contrib)
add_subdirectory(src)
add_subdirectory(doc)
add_subdirectory(ui)
add_subdirectory(base)

add_custom_target(julia ALL
  DEPENDS libccalltest stringreplace julia-ui julia_symlink julia-inference-so
  julia-errno_h.jl
  COMMAND make ${jl_make_flags}
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
