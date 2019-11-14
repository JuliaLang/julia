// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
 * TODO: save/restore floating point control state
 * and reset avx state
 * and update gs:[0x1478] to contain the address of the stack
 */

#define CNAME jl_setjmp
#include "ENTRY.amd64.h"
    mov    rdx,QWORD PTR [rsp] // rta
    mov    rax,QWORD PTR gs:[0] // SEH
    mov    QWORD PTR [rcx+0],rax
    mov    QWORD PTR [rcx+8],rbx
    mov    QWORD PTR [rcx+16],rsp
    mov    QWORD PTR [rcx+24],rbp
    mov    QWORD PTR [rcx+32],rsi
    mov    QWORD PTR [rcx+40],rdi
    mov    QWORD PTR [rcx+48],r12
    mov    QWORD PTR [rcx+56],r13
    mov    QWORD PTR [rcx+64],r14
    mov    QWORD PTR [rcx+72],r15
    mov    QWORD PTR [rcx+80],rdx // rip
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
    xor    rax,rax # return 0
    ret
#include "END.h"


#define CNAME jl_longjmp
#include "ENTRY.amd64.h"
    mov    rax,QWORD PTR [rcx+0]
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
    mov    QWORD PTR gs:[0],rax
    mov    eax,edx // move arg2 to return
    test   eax,eax
    jne    a
    inc    eax
a:  mov    QWORD PTR [rsp],r8
    ret
#include "END.h"


#define CNAME jl_swapcontext
#include "ENTRY.amd64.h"
    // save stack registers
    mov    r8,QWORD PTR gs:[16] // stack top (low)
    mov    rax,QWORD PTR gs:[8] // stack bottom (high)
    mov    QWORD PTR [rcx+0],r8 // sp
    sub    rax,r8
    mov    QWORD PTR [rcx+8],rax // ssize
    add    rcx,16
    // save uc_mcontext
    mov    r8,QWORD PTR [rsp] // rta
    mov    rax,QWORD PTR gs:[0] // SEH
    mov    QWORD PTR [rcx+0],rax
    mov    QWORD PTR [rcx+8],rbx
    mov    QWORD PTR [rcx+16],rsp
    mov    QWORD PTR [rcx+24],rbp
    mov    QWORD PTR [rcx+32],rsi
    mov    QWORD PTR [rcx+40],rdi
    mov    QWORD PTR [rcx+48],r12
    mov    QWORD PTR [rcx+56],r13
    mov    QWORD PTR [rcx+64],r14
    mov    QWORD PTR [rcx+72],r15
    mov    QWORD PTR [rcx+80],r8 // rip
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
    mov    rcx,rdx
    jmp jl_setcontext
#include "END.h"


#define CNAME jl_setcontext
#include "ENTRY.amd64.h"
    // restore stack registers
    mov    r8,QWORD PTR [rcx+0]
    mov    rax,QWORD PTR [rcx+8]
    mov    QWORD PTR gs:[16],r8 // stack top (low)
    add    rax,r8
    mov    QWORD PTR gs:[8],rax // stack bottom (high)
    add    rcx,16
    // restore uc_mcontext
    mov    rax,QWORD PTR [rcx+0]
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
    mov    QWORD PTR gs:[0],rax
    mov    QWORD PTR [rsp],r8
    xor    rax,rax # return 0
    inc    rax # HACK: return 1
    ret
#include "END.h"
