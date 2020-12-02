#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
.global name; \
.cfi_startproc; \
name##:; \
    ldr ip, .L##name; \
.L##name: ;\
    add ip, pc, ip; \
    ldr pc, [ip]; \
    .align 2; \
.L##name##_addr: ; \
    .word name##_addr-(.L##name + 8); \
.cfi_endproc; \

JL_EXPORTED_FUNCS(XX)
#undef XX
