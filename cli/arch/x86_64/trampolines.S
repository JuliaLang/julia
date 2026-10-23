// This file is a part of Julia. License is MIT: https://julialang.org/license

// The functions libjulia exports on behalf of libjulia-internal and
// libjulia-codegen: each jumps through the pointer `<name>_addr`, leaving all
// argument registers and the stack as the caller set them up.
//
// Coupled to C code in cli/: the list of functions is src/jl_exported_funcs.inc,
// the `<name>_addr` pointers are defined in cli/jl_exports.h, and
// cli/loader_lib.c fills them in when it loads the libraries.

#include "../../../src/arch/common.h"
#include "../../../src/jl_exported_funcs.inc"

#define XX(name) \
DEBUGINFO(name); \
.global CNAME(name); \
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
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
