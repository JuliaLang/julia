#include "../../src/jl_exported_funcs.inc"

// Notes:
// bctr: branch to CTR without LR update (tail-call)
// localentry: On PPC functions have a localentry that assumes r2 contains
//             the TOC pointer, and a global entry point that sets r2.
// See 64-Bit ELF V2 ABI Specification: Power Architecture v1.4

#define XX(name) \
.global name; \
.type name##, @function; \
.cfi_startproc; \
name##: ; \
    addis 2, 12, .TOC.-name##@ha; \
    addi 2, 2, .TOC.-name##@l; \
    .localentry name##,.-name##; \
    addis 9,2,name##_addr@toc@ha; \
    ld 12,name##_addr@toc@l(9); \
    mtctr 12; \
    bctr; \
.cfi_endproc; \
.size name##,.-name##; \

JL_EXPORTED_FUNCS(XX)
#undef XX
