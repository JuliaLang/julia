#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
.global name; \
.cfi_startproc; \
name##:; \
    adrp x0, name##_addr; \
    ldr x0, [x0, #:lo12:name##_addr]; \
    br x0; \
.cfi_endproc; \
JL_EXPORTED_FUNCS(XX)
#undef XX
