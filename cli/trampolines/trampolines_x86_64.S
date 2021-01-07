#include "../../src/jl_exported_funcs.inc"

// On macOS, we need to prepend underscores on symbols
#if defined(__APPLE__) && defined(__MACH__)
#define CNAME(x) _##x
#else
#define CNAME(x) x
#endif

#if defined(_WIN64)
#define DEBUGINFO(name)     .def CNAME(name); \
                            .scl 2; \
                            .type 32; \
                            .endef
#define EXPORT(name)        .section .drectve,"r"; \
                            .ascii " -export:"#name""; \
                            .section .text
#define SEH_START1(name)    .seh_proc name
#define SEH_START2()        .seh_endprologue
#define SEH_END()           .seh_endproc
#else
#define DEBUGINFO(name)
#define EXPORT(name)
#define SEH_START1(name)
#define SEH_START2()
#define SEH_END()
#endif

#if defined(__CET__) && __CET__ & 1 != 0
#define CET_START() endbr64
#else
#define CET_START()
#endif

#define XX(name) \
DEBUGINFO(name); \
.global CNAME(name); \
.cfi_startproc; \
SEH_START1(name); \
CNAME(name)##:; \
SEH_START2(); \
    CET_START(); \
    mov CNAME(name##_addr)(%rip),%r11; \
    jmpq *%r11; \
    ud2; \
SEH_END(); \
.cfi_endproc; \
EXPORT(name); \

JL_EXPORTED_FUNCS(XX)
#undef XX
