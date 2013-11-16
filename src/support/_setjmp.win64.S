#include "../../deps/openlibm/amd64/bsd_asm.h"

ENTRY(jl_setjmp)
    movq    0(%rsp),%rdx
    movq    $0,0(%rcx)
    movq    %rbx,8(%rcx)
    movq    %rsp,16(%rcx)
    movq    %rbp,24(%rcx)
    movq    %rsi,32(%rcx)
    movq    %rdi,40(%rcx)
    movq    %r12,48(%rcx)
    movq    %r13,56(%rcx)
    movq    %r14,64(%rcx)
    movq    %r15,72(%rcx)
    movq    %rdx,80(%rcx)
    movq    $0,88(%rcx)
    movaps  %xmm6,96(%rcx)
    movaps  %xmm7,112(%rcx)
    movaps  %xmm8,128(%rcx)
    movaps  %xmm9,144(%rcx)
    movaps  %xmm10,160(%rcx)
    movaps  %xmm11,176(%rcx)
    movaps  %xmm12,192(%rcx)
    movaps  %xmm13,208(%rcx)
    movaps  %xmm14,224(%rcx)
    movaps  %xmm15,240(%rcx)

    xorq    %rax,%rax
    ret
END(jl_setjmp)
