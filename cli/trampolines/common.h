#include "../../src/support/platform.h"

// Preprocessor annoyances
#define CONCAT_(x,y)    x##y
#define CONCAT(x,y)     CONCAT_(x, y)
#define CNAMEADDR(name) CONCAT(CNAME(name),_addr)
#define STR_(x)         #x
#define STR(x)          STR_(x)
#define I(x)            x

// On macOS and 32-bit windows, we need to prepend underscores on symbols to match the C ABI
#if defined(__APPLE__) || (defined(_WIN32) && !defined(_WIN64))
#define UNDERSCORE(x) _##x
#else
#define UNDERSCORE(x)    x
#endif

// Windows requires some help with the linker when it comes to debuginfo/exporting
#if defined(_WIN32) || defined(_WIN64)
#define DEBUGINFO(name)     .def name; \
                            .scl 2; \
                            .type 32; \
                            .endef
#define EXPORT(name)        .section .drectve,"r"; \
                            .ascii STR(-export:##I(name)); \
                            .ascii " "; \
                            .section .text
#elif defined(__ELF__)
#define DEBUGINFO(name)     .type CNAME(name),@function
#define EXPORT(name)        .size CNAME(name), . - CNAME(name)
#else
#define DEBUGINFO(name)
#define EXPORT(name)
#endif

// Windows 64-bit uses SEH
#if defined(_WIN64)
#define SEH_START1(name)    .seh_proc CNAME(name)
#define SEH_START2()        .seh_endprologue
#define SEH_END()           .seh_endproc
#else
#define SEH_START1(name)
#define SEH_START2()
#define SEH_END()
#endif

// If we're compiling with control-flow branch protection, mark the trampoline entry
// points with `endbr{32,64}`, as appropriate on this arch
#if defined(__CET__) && __CET__ & 1 != 0
#if defined(__x86_64__)
#define CET_START()     endbr64
#else
#define CET_START()     endbr32
#endif
#else
#define CET_START()
#endif

// aarch64 on mac requires some special assembler syntax for both calculating memory
// offsets and even just the assembler statement separator token
#if defined(__aarch64__)
#if defined(__APPLE__)
#define PAGE(x)     x##@PAGE
#define PAGEOFF(x)  x##@PAGEOFF
#define SEP         %%
#else
#define PAGE(x)     x
#define PAGEOFF(x)  :lo12:##x
#define SEP         ;
#endif
#endif

// If someday we need to mangle everything, we do so by defining this `CNAME()`
// to do something more complex than just `UNDERSCORE(x)`.
#define CNAME(x)    UNDERSCORE(x)
