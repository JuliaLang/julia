#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
DEBUGINFO(name); \
.global CNAME(name); \
.cfi_startproc; \
SEH_START1(name); \
CNAME(name)##:; \
SEH_START2(); \
    CET_START(); \
    mov CNAMEADDR(name)(%rip),%r11; \
    jmpq *%r11; \
    ud2; \
SEH_END(); \
.cfi_endproc; \
EXPORT(name); \

JL_EXPORTED_FUNCS(XX)
#undef XX
