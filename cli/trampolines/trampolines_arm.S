#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
.global CNAME(name); \
.cfi_startproc; \
CNAME(name)##:; \
    ldr ip, CONCAT(.L,CNAMEADDR(name)); \
CONCAT(.L,CNAME(name)): ;\
    add ip, pc, ip; \
    ldr pc, [ip]; \
    .align 2; \
CONCAT(.L,CNAMEADDR(name))##: ; \
    .word CNAMEADDR(name)##-(CONCAT(.L,CNAME(name)) + 8); \
.cfi_endproc; \

JL_EXPORTED_FUNCS(XX)
#undef XX
