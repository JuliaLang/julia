cmake_minimum_required(VERSION 2.8)
project(libm64)

enable_language(ASM)

set(CMAKE_ASM_FLAGS_RELEASE -m64 -fPIC)
set(CMAKE_ASM_FLAGS_RELEASE -m64 -fPIC)

set(CMAKE_SHARED_LINKER_FLAGS -nostdlib -m64)

set(SOURCES e_remainder.S
e_remainderf.S
e_remainderl.S
e_sqrt.S
e_sqrtf.S
e_sqrtl.S
s_fabs.S
s_fabsf.S
s_llrint.S
s_llrintf.S
s_llrintl.S
s_logbl.S
s_lrint.S
s_lrintf.S
s_lrintl.S
s_remquo.S
s_remquof.S
s_remquol.S
s_rintl.S
s_scalbn.S
s_scalbnf.S
s_scalbnl.S)

add_library(libm64 SHARED ${SOURCES})