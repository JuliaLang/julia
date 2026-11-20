// This file is a part of Julia. License is MIT: https://julialang.org/license

// The trampolines and slot jump thunks of the tests of jl_plt_resolver
// (plt_resolver.c), in each variant codegen emits, from its templates
// (plt_stubs.inc, as the assembler macros of plt_stubs.sed). The tables
// test_plt_trampolines ({name, the trampolines that pass &test_plt_entries[i]})
// and test_plt_thunks ({name, a thunk that jumps through the slot of entry 0})
// list the variants, and end with a NULL name. (The macros are used in .text.)

#include "../common.h"
#include "plt_stubs.S"

#if defined(_CPU_X86_) || defined(_CPU_ARM_)
#define PTR .long
#else
#define PTR .quad
#endif

    .data
    .p2align 3
    .globl CNAME(test_plt_entries)
CNAME(test_plt_entries):
.irp i,0,1,2,3,4,5,6,7
test_plt_entry_\i: PTR 0, 0
.endr
    .globl CNAME(test_plt_slots)
CNAME(test_plt_slots):
.irp i,0,1,2,3,4,5,6,7
test_plt_slot_\i: PTR 0
.endr
// the local pointer to the resolver (i686, PPC64LE)
test_resolver_ptr: PTR CNAME(jl_plt_resolver)

    .text
#if defined(_CPU_ARM_)
    .syntax unified
    .arm // (Julia generates no Thumb code)
#elif defined(_CPU_X86_)
test_pc_thunk:
    movl    (%esp), %eax
    retl
#endif

.macro table name
    .data
    .p2align 3
    .globl CNAME(\name)
CNAME(\name):
    .text
.endm
.macro end_table
    .data
    PTR 0
    .text
.endm

// variant v of the trampolines
.macro trampolines v, template, align, op1, op2
test_plt_tramp_name_\v: .asciz "\template"
    .data
    PTR test_plt_tramp_name_\v
    .text
.irp i,0,1,2,3,4,5,6,7
    DEBUGINFO(test_plt_tramp_\v\()_\i)
    .p2align \align
CNAME(test_plt_tramp_\v\()_\i):
    \template test_plt_entry_\i, \op1, \op2
    .data
    PTR CNAME(test_plt_tramp_\v\()_\i)
    .text
.endr
.endm

// variant v of the thunk
.macro thunk v, template, align, op1
test_plt_thunk_name_\v: .asciz "\template"
    .globl CNAME(test_plt_thunk_\v)
    DEBUGINFO(test_plt_thunk_\v)
    .p2align \align
CNAME(test_plt_thunk_\v):
    \template test_plt_slot_0, \op1
    .data
    PTR test_plt_thunk_name_\v, CNAME(test_plt_thunk_\v)
    .text
.endm

    table test_plt_trampolines
#if defined(_CPU_X86_64_)
    trampolines 0, plt_tramp_asm_x86_64, 4, CNAME(jl_plt_resolver)
#ifndef _OS_DARWIN_ // (Mach-O executables cannot have absolute addresses in code)
    trampolines 1, plt_tramp_asm_x86_64_large, 4, CNAME(jl_plt_resolver)
#endif
#elif defined(_CPU_X86_) && defined(_OS_WINDOWS_)
    trampolines 0, plt_tramp_asm_i686_coff, 4, test_resolver_ptr
#elif defined(_CPU_X86_)
    trampolines 0, plt_tramp_asm_i686, 4, test_resolver_ptr, test_pc_thunk
#elif defined(_CPU_AARCH64_) && defined(_OS_DARWIN_)
    trampolines 0, plt_tramp_asm_aarch64_macho, 2, CNAME(jl_plt_resolver)
#elif defined(_CPU_AARCH64_)
    trampolines 0, plt_tramp_asm_aarch64, 2, CNAME(jl_plt_resolver)
    trampolines 1, plt_tramp_asm_aarch64_large, 2, CNAME(jl_plt_resolver)
#elif defined(_CPU_ARM_)
    trampolines 0, plt_tramp_asm_arm, 2, CNAME(jl_plt_resolver)
#elif defined(_CPU_PPC64_)
    trampolines 0, plt_tramp_asm_ppc64le, 4, test_resolver_ptr
#elif defined(_CPU_RISCV64_)
    trampolines 0, plt_tramp_asm_riscv64, 2, CNAME(jl_plt_resolver)
    trampolines 1, plt_tramp_asm_riscv64_large, 3, CNAME(jl_plt_resolver) // (as codegen aligns it)
#endif
    end_table

    table test_plt_thunks
#if defined(_CPU_X86_64_)
    thunk 0, plt_asm_x86_64, 4
#ifndef _OS_DARWIN_
    thunk 1, plt_asm_x86_64_large, 4
#endif
#elif defined(_CPU_X86_) && defined(_OS_WINDOWS_)
    thunk 0, plt_asm_i686_coff, 4
#elif defined(_CPU_X86_)
    thunk 0, plt_asm_i686, 4, test_pc_thunk
#elif defined(_CPU_AARCH64_) && defined(_OS_DARWIN_)
    thunk 0, plt_asm_aarch64_macho, 2
#elif defined(_CPU_AARCH64_)
    thunk 0, plt_asm_aarch64, 2
    thunk 1, plt_asm_aarch64_large, 2
#elif defined(_CPU_ARM_)
    thunk 0, plt_asm_arm, 2
#elif defined(_CPU_PPC64_)
    thunk 0, plt_asm_ppc64le, 4, CNAME(test_plt_thunk_0)
#elif defined(_CPU_RISCV64_)
    thunk 0, plt_asm_riscv64, 2
    thunk 1, plt_asm_riscv64_large, 3
#endif
    end_table

// Deliberately destroy argument registers during resolution.
    .text
    .globl CNAME(test_plt_clobber_args)
#if defined(_CPU_ARM_)
    .type CNAME(test_plt_clobber_args), %function
    .p2align 2
#endif
CNAME(test_plt_clobber_args):
#if defined(_CPU_X86_64_)
    xorq    %rax, %rax
    xorq    %rcx, %rcx
    xorq    %rdx, %rdx
    xorq    %r8, %r8
    xorq    %r9, %r9
    xorq    %r10, %r10
    xorq    %r11, %r11
#ifndef _WIN64
    xorq    %rdi, %rdi
    xorq    %rsi, %rsi
#endif
    pxor    %xmm0, %xmm0
    pxor    %xmm1, %xmm1
    pxor    %xmm2, %xmm2
    pxor    %xmm3, %xmm3
    pxor    %xmm4, %xmm4
    pxor    %xmm5, %xmm5
#ifndef _WIN64
    pxor    %xmm6, %xmm6
    pxor    %xmm7, %xmm7
#endif
    retq
#elif defined(_CPU_X86_)
    xorl    %eax, %eax
    xorl    %ecx, %ecx
    xorl    %edx, %edx
    pxor    %mm0, %mm0
    pxor    %mm1, %mm1
    pxor    %mm2, %mm2
    emms
    pxor    %xmm0, %xmm0
    pxor    %xmm1, %xmm1
    pxor    %xmm2, %xmm2
    retl
#elif defined(_CPU_AARCH64_)
    mov     x0, xzr
    mov     x1, xzr
    mov     x2, xzr
    mov     x3, xzr
    mov     x4, xzr
    mov     x5, xzr
    mov     x6, xzr
    mov     x7, xzr
    mov     x8, xzr
    eor     v0.16b, v0.16b, v0.16b
    eor     v1.16b, v1.16b, v1.16b
    eor     v2.16b, v2.16b, v2.16b
    eor     v3.16b, v3.16b, v3.16b
    eor     v4.16b, v4.16b, v4.16b
    eor     v5.16b, v5.16b, v5.16b
    eor     v6.16b, v6.16b, v6.16b
    eor     v7.16b, v7.16b, v7.16b
    ret
#elif defined(_CPU_ARM_)
    mov     r0, #0
    mov     r1, #0
    mov     r2, #0
    mov     r3, #0
    mov     ip, #0
#if !defined(__SOFTFP__)
    vmov    d0, r0, r0
    vmov    d1, r0, r0
    vmov    d2, r0, r0
    vmov    d3, r0, r0
    vmov    d4, r0, r0
    vmov    d5, r0, r0
    vmov    d6, r0, r0
    vmov    d7, r0, r0
#endif
    bx      lr
#elif defined(_CPU_RISCV64_)
    li      a0, 0
    fmv.d.x fa0, zero
    li      a1, 0
    fmv.d.x fa1, zero
    li      a2, 0
    fmv.d.x fa2, zero
    li      a3, 0
    fmv.d.x fa3, zero
    li      a4, 0
    fmv.d.x fa4, zero
    li      a5, 0
    fmv.d.x fa5, zero
    li      a6, 0
    fmv.d.x fa6, zero
    li      a7, 0
    fmv.d.x fa7, zero
    ret
#elif defined(_CPU_PPC64_)
    li      3, 0
    li      4, 0
    li      5, 0
    li      6, 0
    li      7, 0
    li      8, 0
    li      9, 0
    li      10, 0
    fsub    1, 1, 1
    fsub    2, 2, 2
    fsub    3, 3, 3
    fsub    4, 4, 4
    fsub    5, 5, 5
    fsub    6, 6, 6
    fsub    7, 7, 7
    fsub    8, 8, 8
    fsub    9, 9, 9
    fsub    10, 10, 10
    fsub    11, 11, 11
    fsub    12, 12, 12
    fsub    13, 13, 13
    vxor    2, 2, 2
    vxor    3, 3, 3
    vxor    4, 4, 4
    vxor    5, 5, 5
    vxor    6, 6, 6
    vxor    7, 7, 7
    vxor    8, 8, 8
    vxor    9, 9, 9
    vxor    10, 10, 10
    vxor    11, 11, 11
    vxor    12, 12, 12
    vxor    13, 13, 13
    blr
#endif

#if (defined(_CPU_X86_64_) && !defined(_WIN64)) || defined(_CPU_X86_)
    .globl CNAME(test_plt_clobber_avx)
CNAME(test_plt_clobber_avx):
    vzeroall
    ret

    .globl CNAME(test_plt_clobber_avx512)
CNAME(test_plt_clobber_avx512):
    vpxord  %zmm0, %zmm0, %zmm0
    vpxord  %zmm1, %zmm1, %zmm1
    vpxord  %zmm2, %zmm2, %zmm2
    vpxord  %zmm3, %zmm3, %zmm3
    vpxord  %zmm4, %zmm4, %zmm4
    vpxord  %zmm5, %zmm5, %zmm5
    vpxord  %zmm6, %zmm6, %zmm6
    vpxord  %zmm7, %zmm7, %zmm7
    ret
#endif
