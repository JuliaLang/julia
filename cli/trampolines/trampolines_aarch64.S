#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
.global CNAME(name) SEP \
.cfi_startproc SEP \
.p2align    2 SEP \
CNAME(name)##: SEP \
    adrp x16, PAGE(CNAME(name##_addr)) SEP \
    ldr x16, [x16, PAGEOFF(CNAME(name##_addr))] SEP \
    br x16 SEP \
.cfi_endproc SEP \

JL_EXPORTED_FUNCS(XX)
#undef XX
