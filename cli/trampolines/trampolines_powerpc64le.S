#include "common.h"
#include "../../src/jl_exported_funcs.inc"

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

JL_EXPORTED_FUNCS(XX)
#undef XX
