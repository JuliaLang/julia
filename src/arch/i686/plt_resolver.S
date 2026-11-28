// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (i686): the shared resolver for lazily bound calls (see
// src/plt.h). Each per-entry trampoline (from the templates in plt_stubs.inc)
// pushes 12 bytes of padding and then the address of its jl_plt_entry_t, and
// jumps here. The top of the padding holds the frame record ({%ebp, return
// address}) while the resolver runs.

#include "../common.h"

#if defined(__i386__) || defined(_M_IX86)

    .text

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
CNAME(jl_plt_resolver):
    .cfi_startproc
    // The return address is 16 bytes up: the per-entry trampoline pushed 12
    // bytes of padding and then &entry before jumping here.
    .cfi_def_cfa_offset 20
    CET_START()

    // Integer arguments use the stack, or ecx/edx for fastcall/thiscall.
    // SysV also passes up to three SIMD arguments in mm0-mm2 or xmm/ymm/zmm0-2.
    // eax is scratch in these conventions (regparm is not supported by the
    // per-entry trampoline protocol). FXSR and SSE2 are Julia's i686 baseline.
    //
    // Frame layout (from esp after sub):
    //   0x000:       arg0 for entry->resolve (&entry)
    //   0x004:       ecx
    //   0x008:       edx
    //   0x00c:       ebx (while probing CPUID)
    //   0x010:       vector width (16, 32, or 64 bytes)
    //   0x020-0x21f: FXSAVE area, including MMX and XMM argument registers
    //   0x220-0x2df: ymm/zmm0-zmm2, each in a 64-byte slot
    //   0x2ec:       &entry (the trampoline's slot, above the frame)
    //   0x2f8:       ebp (the top of the trampoline's padding)
    // Subtracting 0x2ec from entry esp (12 mod 16) aligns both the FXSAVE
    // area and the call to resolve.

    // frame record {%ebp, retaddr_user}
    movl    %ebp, 12(%esp)
    .cfi_offset %ebp, -8
    leal    12(%esp), %ebp

    subl    $0x2ec, %esp
    .cfi_def_cfa_offset 0x2ec + 20

    movl    %ecx, 0x04(%esp)
    .cfi_rel_offset ecx, 0x04
    movl    %edx, 0x08(%esp)
    .cfi_rel_offset edx, 0x08

    // Save x87/MMX state as well as XMM registers. MMX arguments can leave
    // the x87 stack full, so empty it before entering ordinary C code.
    fxsave  0x20(%esp)
    movl    %ebx, 0x0c(%esp)
    .cfi_rel_offset ebx, 0x0c
    movl    $1, %eax
    cpuid
    andl    $0x18000000, %ecx    // AVX and OSXSAVE
    cmpl    $0x18000000, %ecx
    jne     16f
    xorl    %ecx, %ecx
    xgetbv
    movl    %eax, %edx
    andl    $6, %edx
    cmpl    $6, %edx
    jne     16f
    andl    $0xe6, %eax
    cmpl    $0xe6, %eax
    jne     32f
    movl    $7, %eax
    xorl    %ecx, %ecx
    cpuid
    testl   $0x10000, %ebx      // AVX512F
    jz      32f

64:
    movl    $64, 0x10(%esp)
    vmovdqu64 %zmm0, 0x220(%esp)
    vmovdqu64 %zmm1, 0x260(%esp)
    vmovdqu64 %zmm2, 0x2a0(%esp)
    jmp     1f
32:
    movl    $32, 0x10(%esp)
    vmovdqu   %ymm0, 0x220(%esp)
    vmovdqu   %ymm1, 0x260(%esp)
    vmovdqu   %ymm2, 0x2a0(%esp)
    jmp     1f
16:
    movl    $16, 0x10(%esp)     // the XMM values are already in the FXSAVE area
1:
    movl    0x0c(%esp), %ebx
    .cfi_restore ebx
    emms
    movl    0x2ec(%esp), %eax
    movl    %eax, 0x00(%esp)

    calll   *(%eax)             // entry->resolve(entry)

    fxrstor 0x20(%esp)
    cmpl    $64, 0x10(%esp)
    je      64f
    cmpl    $32, 0x10(%esp)
    jne     2f
32:
    vmovdqu   0x2a0(%esp), %ymm2
    vmovdqu   0x260(%esp), %ymm1
    vmovdqu   0x220(%esp), %ymm0
    jmp     2f
64:
    vmovdqu64 0x2a0(%esp), %zmm2
    vmovdqu64 0x260(%esp), %zmm1
    vmovdqu64 0x220(%esp), %zmm0
2:
    movl    0x08(%esp), %edx
    .cfi_restore edx
    movl    0x04(%esp), %ecx
    .cfi_restore ecx
    movl    0x2f8(%esp), %ebp
    .cfi_restore ebp

    // Drop our frame and the trampoline's 16 bytes: the target returns straight
    // to the caller.
    addl    $0x2fc, %esp
    .cfi_def_cfa_offset 4

    jmpl    *%eax
    .cfi_endproc
    EXPORT(jl_plt_resolver)

#endif // defined(__i386__) || defined(_M_IX86)
