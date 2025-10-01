// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "common.h"
#include "../../src/jl_exported_funcs.inc"

// set this option to 1 to get very slightly slower trampolines which however do not trigger
// this linker warning:
//   ld: ./loader_trampolines.o: warning: relocation against `jl_***_addr' in read-only section `.text'
//   ld: warning: creating DT_TEXTREL in a shared object
// If you have a large libjulia.so file or other restrictions on using TEXTREL for some
// reason, this may be worthwhile.
// This is not relevant on Windows (though it is valid there), since it always uses
// DT_TEXTREL anyways, and does not support this notion of PIC.
#define USE_PC32 0

#if USE_PC32
.cfi_startproc
julia__x86.get_pc_thunk.ax:
    mov    (%esp),%eax
    ret
.cfi_endproc

#define CALL(name) \
    call julia__x86.get_pc_thunk.ax; \
    jmpl *(CNAMEADDR(name) - .)(%eax); \

#else

#define CALL(name) \
    jmpl *(CNAMEADDR(name)); \

#endif

#define XX(name) \
DEBUGINFO(CNAME(name)); \
.global CNAME(name); \
.cfi_startproc; \
CNAME(name)##:; \
    CET_START(); \
    CALL(name); \
    ud2; \
.cfi_endproc; \
EXPORT(name); \

JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
