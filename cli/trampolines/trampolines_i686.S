// This file is a part of Julia. License is MIT: https://julialang.org/license

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

JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
