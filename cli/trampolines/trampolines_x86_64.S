// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "common.h"
#include "../../src/jl_exported_funcs.inc"

#define XX(name) \
DEBUGINFO(name); \
GLOBAL(CNAME(name)); \
CNAME(name)##:; \
.cfi_startproc; \
SEH_START1(name); \
SEH_START2(); \
    CET_START(); \
    mov CNAMEADDR(name)(%rip),%r11; \
    jmpq *%r11; \
    ud2; \
SEH_END(); \
.cfi_endproc; \
EXPORT(name); \

JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
#ifndef JL_LIBRARY_STATIC
// in the static build the codegen functions are defined directly (their
// fallbacks are renamed to the public names, see jl_static_codegen_fallbacks.inc)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#endif
#undef XX
