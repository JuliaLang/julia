#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
.global CNAME(name); \
.cfi_startproc; \
CNAME(name)##:; \
    ldr ip, .L##CNAME(name); \
.L##CNAME(name): ;\
    add ip, pc, ip; \
    ldr pc, [ip]; \
    .align 2; \
.L##CNAMEADDR(name)##: ; \
    .word CNAMEADDR(name)##-(.L##name + 8); \
.cfi_endproc; \

JL_EXPORTED_FUNCS(XX)
#undef XX
