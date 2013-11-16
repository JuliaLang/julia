#include "../../deps/openlibm/amd64/bsd_asm.h"

ENTRY(jl_longjmp)
    movq    8(%rcx),%rbx
    movq    16(%rcx),%rsp
    movq    24(%rcx),%rbp
    movq    32(%rcx),%rsi
    movq    40(%rcx),%rdi
    movq    48(%rcx),%r12
    movq    56(%rcx),%r13
    movq    64(%rcx),%r14
    movq    72(%rcx),%r15
    movq    80(%rcx),%r8
    movaps  96(%rcx),%xmm6
    movaps  112(%rcx),%xmm7
    movaps  128(%rcx),%xmm8
    movaps  144(%rcx),%xmm9
    movaps  160(%rcx),%xmm10
    movaps  176(%rcx),%xmm11
    movaps  192(%rcx),%xmm12
    movaps  208(%rcx),%xmm13
    movaps  224(%rcx),%xmm14
    movaps  240(%rcx),%xmm15
    
    movl    %edx,%eax
    testl   %eax,%eax
    jnz     1f
    incl    %eax
1:  movq    %r8,0(%rsp)
    ret
END(jl_longjmp)
