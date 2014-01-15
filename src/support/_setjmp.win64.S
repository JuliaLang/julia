#include "../../deps/openlibm/amd64/bsd_asm.h"

.intel_syntax noprefix
ENTRY(jl_setjmp)
    mov    rdx,QWORD PTR [rsp]
    mov    QWORD PTR [rcx],0x0
    mov    QWORD PTR [rcx+0x08],rbx
    mov    QWORD PTR [rcx+0x10],rsp
    mov    QWORD PTR [rcx+0x18],rbp
    mov    QWORD PTR [rcx+0x20],rsi
    mov    QWORD PTR [rcx+0x28],rdi
    mov    QWORD PTR [rcx+0x30],r12
    mov    QWORD PTR [rcx+0x38],r13
    mov    QWORD PTR [rcx+0x40],r14
    mov    QWORD PTR [rcx+0x48],r15
    mov    QWORD PTR [rcx+0x50],rdx
    mov    QWORD PTR [rcx+0x58],0x0
    movaps XMMWORD PTR [rcx+0x60],xmm6
    movaps XMMWORD PTR [rcx+0x70],xmm7
    movaps XMMWORD PTR [rcx+0x80],xmm8
    movaps XMMWORD PTR [rcx+0x90],xmm9
    movaps XMMWORD PTR [rcx+0xa0],xmm10
    movaps XMMWORD PTR [rcx+0xb0],xmm11
    movaps XMMWORD PTR [rcx+0xc0],xmm12
    movaps XMMWORD PTR [rcx+0xd0],xmm13
    movaps XMMWORD PTR [rcx+0xe0],xmm14
    movaps XMMWORD PTR [rcx+0xf0],xmm15
    xor    rax,rax
    ret
END(jl_setjmp)
