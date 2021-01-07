#include "../../src/jl_exported_funcs.inc"

// On macOS, we need to prepend underscores on symbols
#if defined(__APPLE__) && defined(__MACH__)
#define CNAME(x)    _##x
#define PAGE(x)     x##@PAGE
#define PAGEOFF(x)  x##@PAGEOFF
#define SEP         %%
#else
#define CNAME(x)    x
#define PAGE(x)     x
#define PAGEOFF(x)  #:lo12:##x
#define SEP         ;
#endif

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
