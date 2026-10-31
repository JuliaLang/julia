// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (x86_64 SysV): the shared resolver for lazily bound calls
// (see src/plt.h). Each per-entry trampoline (from the templates in
// plt_stubs.inc) pushes 8 bytes of padding and then the address of its
// jl_plt_entry_t, and jumps here. The padding holds the frame record ({%rbp,
// return address}) while the resolver runs.

#include "../common.h"

#if defined(__x86_64__) && !defined(_WIN64)

    .text

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
CNAME(jl_plt_resolver):
    .cfi_startproc
    // The return address is 16 bytes up: the per-entry trampoline pushed
    // a pad and &entry before jumping here.
    .cfi_def_cfa_offset 24
    CET_START()

    // Arguments are in rdi, rsi, rdx, rcx, r8, r9, rax (the number of vector
    // arguments of a varargs call) and zmm0-zmm7; r10 and r11 are scratch. The
    // vector registers are saved at the width that both the CPU and the OS
    // enable, found before any AVX instruction runs: the resolution may run
    // code that clobbers the upper lanes of YMM and ZMM arguments.
    //
    // Frame layout (from rsp after sub):
    //   0x000-0x037: rdi, rsi, rdx, rcx, r8, r9, rax
    //   0x038:       vector width (16, 32, or 64 bytes)
    //   0x040-0x23f: xmm/ymm/zmm0-zmm7, each in a 64-byte slot
    //   0x240:       rbx (while probing CPUID)
    //   0x248:       &entry (the trampoline's slot, above the frame)
    //   0x250:       rbp (the trampoline's pad)
    //   0x258:       retaddr_user
    // Subtracting 0x248 from entry rsp (8 mod 16) aligns the resolve call.

    // frame record {%rbp, retaddr_user}
    movq    %rbp, 8(%rsp)
    .cfi_offset %rbp, -16
    leaq    8(%rsp), %rbp

    subq    $0x248, %rsp
    .cfi_def_cfa_offset 0x248 + 24   // +24 for the return address and the trampoline-pushed &entry + pad

    movq    %rdi, 0x00(%rsp)
    .cfi_rel_offset rdi, 0x00
    movq    %rsi, 0x08(%rsp)
    .cfi_rel_offset rsi, 0x08
    movq    %rdx, 0x10(%rsp)
    .cfi_rel_offset rdx, 0x10
    movq    %rcx, 0x18(%rsp)
    .cfi_rel_offset rcx, 0x18
    movq    %r8,  0x20(%rsp)
    .cfi_rel_offset r8, 0x20
    movq    %r9,  0x28(%rsp)
    .cfi_rel_offset r9, 0x28
    movq    %rax, 0x30(%rsp)
    .cfi_rel_offset rax, 0x30

    // The save slots of the vector argument registers are not described to
    // the unwinder (libunwind cannot handle vector registers in CFI and stops
    // unwinding at this frame).
    movq    %rbx, 0x240(%rsp)
    .cfi_rel_offset rbx, 0x240
    movl    $1, %eax
    cpuid
    andl    $0x18000000, %ecx    // AVX and OSXSAVE
    cmpl    $0x18000000, %ecx
    jne     16f
    xorl    %ecx, %ecx
    xgetbv
    movl    %eax, %edx
    andl    $6, %edx            // XMM and YMM state enabled by the OS
    cmpl    $6, %edx
    jne     16f
    andl    $0xe6, %eax         // also opmask and both ZMM state components
    cmpl    $0xe6, %eax
    jne     32f
    movl    $7, %eax
    xorl    %ecx, %ecx
    cpuid
    testl   $0x10000, %ebx      // AVX512F
    jz      32f

64:
    movl    $64, 0x38(%rsp)
    vmovdqu64 %zmm0, 0x040(%rsp)
    vmovdqu64 %zmm1, 0x080(%rsp)
    vmovdqu64 %zmm2, 0x0c0(%rsp)
    vmovdqu64 %zmm3, 0x100(%rsp)
    vmovdqu64 %zmm4, 0x140(%rsp)
    vmovdqu64 %zmm5, 0x180(%rsp)
    vmovdqu64 %zmm6, 0x1c0(%rsp)
    vmovdqu64 %zmm7, 0x200(%rsp)
    jmp     1f
32:
    movl    $32, 0x38(%rsp)
    vmovdqu   %ymm0, 0x040(%rsp)
    vmovdqu   %ymm1, 0x080(%rsp)
    vmovdqu   %ymm2, 0x0c0(%rsp)
    vmovdqu   %ymm3, 0x100(%rsp)
    vmovdqu   %ymm4, 0x140(%rsp)
    vmovdqu   %ymm5, 0x180(%rsp)
    vmovdqu   %ymm6, 0x1c0(%rsp)
    vmovdqu   %ymm7, 0x200(%rsp)
    jmp     1f
16:
    movl    $16, 0x38(%rsp)
    movdqu    %xmm0, 0x040(%rsp)
    movdqu    %xmm1, 0x080(%rsp)
    movdqu    %xmm2, 0x0c0(%rsp)
    movdqu    %xmm3, 0x100(%rsp)
    movdqu    %xmm4, 0x140(%rsp)
    movdqu    %xmm5, 0x180(%rsp)
    movdqu    %xmm6, 0x1c0(%rsp)
    movdqu    %xmm7, 0x200(%rsp)
1:
    movq    0x240(%rsp), %rbx
    .cfi_restore rbx

    movq    0x248(%rsp), %rdi

    callq   *(%rdi)             // entry->resolve(entry)
    movq    %rax, %r11          // the resolved target, across the restores

    cmpl    $64, 0x38(%rsp)
    je      64f
    cmpl    $32, 0x38(%rsp)
    je      32f
16:
    movdqu    0x200(%rsp), %xmm7
    movdqu    0x1c0(%rsp), %xmm6
    movdqu    0x180(%rsp), %xmm5
    movdqu    0x140(%rsp), %xmm4
    movdqu    0x100(%rsp), %xmm3
    movdqu    0x0c0(%rsp), %xmm2
    movdqu    0x080(%rsp), %xmm1
    movdqu    0x040(%rsp), %xmm0
    jmp     2f
32:
    vmovdqu   0x200(%rsp), %ymm7
    vmovdqu   0x1c0(%rsp), %ymm6
    vmovdqu   0x180(%rsp), %ymm5
    vmovdqu   0x140(%rsp), %ymm4
    vmovdqu   0x100(%rsp), %ymm3
    vmovdqu   0x0c0(%rsp), %ymm2
    vmovdqu   0x080(%rsp), %ymm1
    vmovdqu   0x040(%rsp), %ymm0
    jmp     2f
64:
    vmovdqu64 0x200(%rsp), %zmm7
    vmovdqu64 0x1c0(%rsp), %zmm6
    vmovdqu64 0x180(%rsp), %zmm5
    vmovdqu64 0x140(%rsp), %zmm4
    vmovdqu64 0x100(%rsp), %zmm3
    vmovdqu64 0x0c0(%rsp), %zmm2
    vmovdqu64 0x080(%rsp), %zmm1
    vmovdqu64 0x040(%rsp), %zmm0
2:

    movq    0x30(%rsp), %rax
    .cfi_restore rax
    movq    0x28(%rsp), %r9
    .cfi_restore r9
    movq    0x20(%rsp), %r8
    .cfi_restore r8
    movq    0x18(%rsp), %rcx
    .cfi_restore rcx
    movq    0x10(%rsp), %rdx
    .cfi_restore rdx
    movq    0x08(%rsp), %rsi
    .cfi_restore rsi
    movq    0x00(%rsp), %rdi
    .cfi_restore rdi
    movq    0x250(%rsp), %rbp
    .cfi_restore rbp

    // Drop our frame and the trampoline's 16 bytes: the target returns straight
    // to the caller.
    addq    $0x258, %rsp
    .cfi_def_cfa_offset 8

    jmpq    *%r11
    .cfi_endproc
    EXPORT(jl_plt_resolver)

#endif // defined(__x86_64__) && !defined(_WIN64)
