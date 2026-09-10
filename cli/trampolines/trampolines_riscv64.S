// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define SEP ;

#define XX(name) \
.global CNAME(name) SEP \
.cfi_startproc SEP \
.p2align    2 SEP \
 CNAME(name)##: SEP \
    auipc t3, %pcrel_hi(CNAMEADDR(name)) SEP \
    ld t3, %pcrel_lo(CNAME(name))(t3) SEP \
    jr t3 SEP \
.cfi_endproc SEP \

JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifndef JL_LIBRARY_STATIC
// static build: the codegen fallbacks are defined under the public names
JL_CODEGEN_EXPORTED_FUNCS(XX)
#endif
#undef XX
