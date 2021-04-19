#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
DEBUGINFO(CNAME(name)); \
.global CNAME(name); \
.cfi_startproc; \
CNAME(name)##:; \
    CET_START(); \
    jmpl *(CNAMEADDR(name)); \
    ud2; \
.cfi_endproc; \
EXPORT(name); \

JL_EXPORTED_FUNCS(XX)
#undef XX
