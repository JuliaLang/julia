#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
.global name; \
.cfi_startproc; \
name##: ; \
    std 2,24(1); \
    ld 12,name##_addr@toc(2); \
    mtctr 12; \
    bctr; \
.cfi_endproc; \

JL_EXPORTED_FUNCS(XX)
#undef XX
