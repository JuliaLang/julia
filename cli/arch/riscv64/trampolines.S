// This file is a part of Julia. License is MIT: https://julialang.org/license

// The functions libjulia exports on behalf of libjulia-internal and
// libjulia-codegen: each jumps through the pointer `<name>_addr`, leaving all
// argument registers and the stack as the caller set them up.
//
// Coupled to C code in cli/: the list of functions is src/jl_exported_funcs.inc,
// the `<name>_addr` pointers are defined in cli/jl_exports.h, and
// cli/loader_lib.c fills them in when it loads the libraries.

#include "../../../src/arch/common.h"
#include "../../../src/jl_exported_funcs.inc"

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
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
