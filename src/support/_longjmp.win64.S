#include "../../deps/openlibm/amd64/bsd_asm.h"

.intel_syntax noprefix
ENTRY(jl_longjmp)
    mov    rbx,QWORD PTR [rcx+0x08]
    mov    rsp,QWORD PTR [rcx+0x10]
    mov    rbp,QWORD PTR [rcx+0x18]
    mov    rsi,QWORD PTR [rcx+0x20]
    mov    rdi,QWORD PTR [rcx+0x28]
    mov    r12,QWORD PTR [rcx+0x30]
    mov    r13,QWORD PTR [rcx+0x38]
    mov    r14,QWORD PTR [rcx+0x40]
    mov    r15,QWORD PTR [rcx+0x48]
    mov    r8, QWORD PTR [rcx+0x50]
    movaps xmm6,XMMWORD PTR [rcx+0x60]
    movaps xmm7,XMMWORD PTR [rcx+0x70]
    movaps xmm8,XMMWORD PTR [rcx+0x80]
    movaps xmm9,XMMWORD PTR [rcx+0x90]
    movaps xmm10,XMMWORD PTR [rcx+0xa0]
    movaps xmm11,XMMWORD PTR [rcx+0xb0]
    movaps xmm12,XMMWORD PTR [rcx+0xc0]
    movaps xmm13,XMMWORD PTR [rcx+0xd0]
    movaps xmm14,XMMWORD PTR [rcx+0xe0]
    movaps xmm15,XMMWORD PTR [rcx+0xf0]
    mov    eax,edx
    test   eax,eax
    jne    1f
    inc    eax
1:  mov    QWORD PTR [rsp],r8
    ret
END(jl_longjmp)
