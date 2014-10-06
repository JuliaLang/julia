#include "../../deps/openlibm/amd64/bsd_asm.h"

#ifndef _MSC_VER
.intel_syntax noprefix
ENTRY(jl_setjmp)
#else
.code
jl_setjmp proc
#endif
    mov    rdx,QWORD PTR [rsp]
    mov    QWORD PTR [rcx],0
    mov    QWORD PTR [rcx+8],rbx
    mov    QWORD PTR [rcx+16],rsp
    mov    QWORD PTR [rcx+24],rbp
    mov    QWORD PTR [rcx+32],rsi
    mov    QWORD PTR [rcx+40],rdi
    mov    QWORD PTR [rcx+48],r12
    mov    QWORD PTR [rcx+56],r13
    mov    QWORD PTR [rcx+64],r14
    mov    QWORD PTR [rcx+72],r15
    mov    QWORD PTR [rcx+80],rdx
    mov    QWORD PTR [rcx+88],0
    movaps XMMWORD PTR [rcx+96],xmm6
    movaps XMMWORD PTR [rcx+112],xmm7
    movaps XMMWORD PTR [rcx+128],xmm8
    movaps XMMWORD PTR [rcx+144],xmm9
    movaps XMMWORD PTR [rcx+160],xmm10
    movaps XMMWORD PTR [rcx+176],xmm11
    movaps XMMWORD PTR [rcx+192],xmm12
    movaps XMMWORD PTR [rcx+208],xmm13
    movaps XMMWORD PTR [rcx+224],xmm14
    movaps XMMWORD PTR [rcx+240],xmm15
    xor    rax,rax
    ret
#ifdef _MSC_VER
jl_setjmp endp
#endif
END(jl_setjmp)
