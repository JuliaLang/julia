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
// bctr: branch to CTR without LR update (tail-call), into the target's global
//       entry point (r12), which sets r2 to the target library's TOC.
// localentry 1: a single entry point that does not preserve r2, so that the
//       linker makes even local callers restore their TOC afterwards (a
//       separate local entry would promise r2 is preserved, and local callers
//       would not set r12). The slot is therefore addressed relative to the pc
//       (read with bcl 20,31, which does not disturb return address
//       prediction), through an offset stored after the code.
// See 64-Bit ELF V2 ABI Specification: Power Architecture v1.5

#define XX(name) \
.global CNAME(name); \
.type CNAME(name)##, @function; \
.cfi_startproc; \
CNAME(name)##: ; \
    .localentry CNAME(name)##, 1; \
    mflr 0; \
    .cfi_register lr, 0; \
    bcl 20, 31, 1f; \
1:  mflr 11; \
    mtlr 0; \
    .cfi_restore lr; \
    ld 12, 2f-1b(11); \
    add 12, 12, 11; \
    ld 12, 0(12); \
    mtctr 12; \
    bctr; \
    .p2align 3; \
2:  .quad CNAMEADDR(name)-1b; \
.cfi_endproc; \
.size CNAME(name)##,.-CNAME(name)##; \

JL_RUNTIME_EXPORTED_FUNCS(XX)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
