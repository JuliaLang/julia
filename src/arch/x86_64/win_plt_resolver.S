// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (x86_64 Windows): the shared resolver for lazily bound calls
// (see src/plt.h). Each per-entry trampoline (from the templates in
// plt_stubs.inc) pushes 8 bytes of padding and then the address of its
// jl_plt_entry_t, and jumps here.

#include "../common.h"

#if defined(_WIN64)

    .text

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
CNAME(jl_plt_resolver):
    SEH_START1(jl_plt_resolver)
    // The per-entry trampoline pushed an 8-byte pad and &entry before
    // jumping here; describe those 16 bytes as part of this frame so unwinding
    // finds the return address above them.
    .seh_stackalloc 16
    .cfi_startproc
    .cfi_def_cfa_offset 24
    CET_START()

    // Arguments are in rcx, rdx, r8, r9 and xmm0-xmm3, and on the stack above
    // the return address, after the caller's shadow space. There is no red
    // zone.
    //
    // Frame layout (from rsp after sub):
    //   0x00-0x1f: shadow space for callq (32 bytes)
    //   0x20-0x27: rcx
    //   0x28-0x2f: rdx
    //   0x30-0x37: r8
    //   0x38-0x3f: r9
    //   0x40-0x4f: xmm0
    //   0x50-0x5f: xmm1
    //   0x60-0x6f: xmm2
    //   0x70-0x7f: xmm3
    //   0x80-0x87: (alignment padding)
    //   0x88:      &entry        (above the frame, pushed by trampoline)
    //   0x90:      0             (above the frame, pushed by trampoline as pad)
    //   0x98:      retaddr_user
    // Subtracting 0x88 from entry rsp (8 mod 16) aligns the resolve call.

    subq    $0x88, %rsp
    .seh_stackalloc 0x88
    .cfi_def_cfa_offset 0x88 + 24   // +24 for the return address and the trampoline-pushed &entry + pad
    SEH_START2()

    movq    %rcx, 0x20(%rsp)
    .cfi_rel_offset rcx, 0x20
    movq    %rdx, 0x28(%rsp)
    .cfi_rel_offset rdx, 0x28
    movq    %r8,  0x30(%rsp)
    .cfi_rel_offset r8, 0x30
    movq    %r9,  0x38(%rsp)
    .cfi_rel_offset r9, 0x38

    // The save slots of the vector argument registers are not described to
    // the unwinder (libunwind cannot handle vector registers in CFI and stops
    // unwinding at this frame).
    movdqu  %xmm0, 0x40(%rsp)
    movdqu  %xmm1, 0x50(%rsp)
    movdqu  %xmm2, 0x60(%rsp)
    movdqu  %xmm3, 0x70(%rsp)

    movq    0x88(%rsp), %rcx

    callq   *(%rcx)             // entry->resolve(entry)
    movq    %rax, %r11          // the resolved target, across the restores

    movdqu  0x70(%rsp), %xmm3
    movdqu  0x60(%rsp), %xmm2
    movdqu  0x50(%rsp), %xmm1
    movdqu  0x40(%rsp), %xmm0

    movq    0x38(%rsp), %r9
    .cfi_restore r9
    movq    0x30(%rsp), %r8
    .cfi_restore r8
    movq    0x28(%rsp), %rdx
    .cfi_restore rdx
    movq    0x20(%rsp), %rcx
    .cfi_restore rcx

    // Drop our frame and the trampoline's 16 bytes: the target returns straight
    // to the caller.
    addq    $0x98, %rsp
    .cfi_def_cfa_offset 8

    // rsp has changed back to the user's stack. Describe the tail transfer
    // with a separate, empty unwind frame: register jumps are not recognized
    // as epilogues by every Windows unwinder (notably older Wine versions).
    SEH_END()
    .seh_proc .Lplt_resolver_tail
.Lplt_resolver_tail:
    .seh_endprologue
    jmpq    *%r11
    .seh_endproc
    .cfi_endproc
    EXPORT(jl_plt_resolver)

#endif // defined(_WIN64)
