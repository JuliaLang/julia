#include "../../src/jl_exported_funcs.inc"

// On Windows, we need to prepend underscores on symbols
#if defined(_WIN32)
#define CNAME(x) _##x
#define DEBUGINFO(name)     .def CNAME(name); \
                            .scl 2; \
                            .type 32; \
                            .endef
#else
#define CNAME(x) x
#define DEBUGINFO(name)
#endif

#if defined(__CET__) && __CET__ & 1 != 0
#define CET_START() endbr32
#else
#define CET_START()
#endif

#define XX(name) \
DEBUGINFO(name); \
.global CNAME(name); \
.cfi_startproc; \
CNAME(name)##:; \
    CET_START(); \
    jmpl *(CNAME(name##_addr)); \
    ud2; \
.cfi_endproc; \

JL_EXPORTED_FUNCS(XX)
#undef XX
