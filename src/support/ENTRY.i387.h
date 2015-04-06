#define _START_ENTRY .p2align 2,0x90
#define STR(csym)           #csym
#define XSTR(csym)          STR(csym)
#if defined(__linux__) || defined(__FreeBSD__) || defined(__ELF__)
#ifndef __APPLE__
#define EXT_(csym)          csym
#define EXT(csym)           EXT_(csym)
#endif
#define HIDENAME(asmsym)    .asmsym
.text
_START_ENTRY
.globl EXT(CNAME)
.type EXT(CNAME),@function
EXT(CNAME):

#elif defined(_WIN32)
#define EXT_(csym)          _##csym
#define EXT(csym)           EXT_(csym)
#define HIDENAME(asmsym)    .asmsym

#ifndef _MSC_VER
.intel_syntax
.text
_START_ENTRY
.globl EXT(CNAME)
.section .drectve
.ascii " -export:" XSTR(CNAME)
.section .text
.def EXT(CNAME)
.scl 2
.type 32
.endef
EXT(CNAME):
#else
.586
.model small,C
.code
CNAME proc
#endif

#endif
