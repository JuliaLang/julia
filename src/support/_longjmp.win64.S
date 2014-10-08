#include "../../deps/openlibm/amd64/bsd_asm.h"

#ifndef _MSC_VER
.intel_syntax noprefix
ENTRY(jl_longjmp)
#else
.code
jl_longjmp proc
#endif
    mov    rbx,QWORD PTR [rcx+8]
    mov    rsp,QWORD PTR [rcx+16]
    mov    rbp,QWORD PTR [rcx+24]
    mov    rsi,QWORD PTR [rcx+32]
    mov    rdi,QWORD PTR [rcx+40]
    mov    r12,QWORD PTR [rcx+48]
    mov    r13,QWORD PTR [rcx+56]
    mov    r14,QWORD PTR [rcx+64]
    mov    r15,QWORD PTR [rcx+72]
    mov    r8, QWORD PTR [rcx+80]
    movaps xmm6,XMMWORD PTR [rcx+96]
    movaps xmm7,XMMWORD PTR [rcx+112]
    movaps xmm8,XMMWORD PTR [rcx+128]
    movaps xmm9,XMMWORD PTR [rcx+144]
    movaps xmm10,XMMWORD PTR [rcx+160]
    movaps xmm11,XMMWORD PTR [rcx+176]
    movaps xmm12,XMMWORD PTR [rcx+192]
    movaps xmm13,XMMWORD PTR [rcx+208]
    movaps xmm14,XMMWORD PTR [rcx+224]
    movaps xmm15,XMMWORD PTR [rcx+240]
    mov    eax,edx
    test   eax,eax
    jne    a
    inc    eax
a:  mov    QWORD PTR [rsp],r8
    ret
#ifdef _MSC_VER
jl_longjmp endp
#endif
END(jl_longjmp)
