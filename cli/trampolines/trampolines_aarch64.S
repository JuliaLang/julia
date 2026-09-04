// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
GLOBAL(CNAME(name)) SEP \
CNAME(name)##: SEP \
.cfi_startproc SEP \
.p2align    2 SEP \
    adrp x16, PAGE(CNAME(name##_addr)) SEP \
    ldr x16, [x16, PAGEOFF(CNAME(name##_addr))] SEP \
    br x16 SEP \
.cfi_endproc SEP \

JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
#ifndef JL_LIBRARY_STATIC
// in the static build the codegen functions are defined directly (their
// fallbacks are renamed to the public names, see jl_static_codegen_fallbacks.inc)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#endif
#undef XX
