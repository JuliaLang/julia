// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define SEP ;

#define XX(name) \
GLOBAL(CNAME(name)) SEP \
.cfi_startproc SEP \
.p2align    2 SEP \
 CNAME(name)##: SEP \
    auipc t3, %pcrel_hi(CNAMEADDR(name)) SEP \
    ld t3, %pcrel_lo(CNAME(name))(t3) SEP \
    jr t3 SEP \
.cfi_endproc SEP \

JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifndef JL_LIBRARY_STATIC
// in the static build the codegen functions are defined directly (their
// fallbacks are renamed to the public names, see jl_static_codegen_fallbacks.inc)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#endif
#undef XX
