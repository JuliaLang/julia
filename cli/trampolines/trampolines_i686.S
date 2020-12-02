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

#define XX(name) \
DEBUGINFO(name); \
.global name; \
.cfi_startproc; \
name##:; \
    endbr32; \
    jmpl *(CNAME(name##_addr)); \
    ud2; \
.cfi_endproc; \

JL_EXPORTED_FUNCS(XX)
#undef XX
