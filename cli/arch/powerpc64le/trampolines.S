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

// Notes:
// bctr: branch to CTR without LR update (tail-call)
// localentry: On PPC functions have a localentry that assumes r2 contains
//             the TOC pointer, and a global entry point that sets r2.
// See 64-Bit ELF V2 ABI Specification: Power Architecture v1.4

#define XX(name) \
.global CNAME(name); \
.type CNAME(name)##, @function; \
.cfi_startproc; \
CNAME(name)##: ; \
    addis 2, 12, .TOC.-CNAME(name)##@ha; \
    addi 2, 2, .TOC.-CNAME(name)##@l; \
    .localentry CNAME(name)##,.-CNAME(name)##; \
    addis 12,2,CNAMEADDR(name)##@toc@ha; \
    ld 12,CNAMEADDR(name)##@toc@l(12); \
    mtctr 12; \
    bctr; \
.cfi_endproc; \
.size CNAME(name)##,.-CNAME(name)##; \

JL_RUNTIME_EXPORTED_FUNCS(XX)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
