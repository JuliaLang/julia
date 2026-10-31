// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (PPC64LE): the shared resolver for lazily bound calls (see
// src/plt.h). Each per-entry trampoline (from the templates in plt_stubs.inc)
// allocates a 48-byte frame (`stdu 1, -48(1)`) with the address of its
// jl_plt_entry_t at offset 32, and jumps here, leaving lr as the caller set it.
//
// The resolve function and the target are entered at their global entry point
// (r12 holds their address), so they set up their own TOC, and this resolver
// uses none: callers of stubs treat them as calls into another module and
// restore their TOC (r2) afterwards.

#include "../common.h"

#if defined(_CPU_PPC64_) && !defined(_OS_WINDOWS_)

    .text

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
    .p2align 2
CNAME(jl_plt_resolver):
    .cfi_startproc
    // The caller's sp is 48 bytes above ours: the per-entry trampoline's frame.
    .cfi_def_cfa_offset 48
    // a single entry point, which does not preserve r2
    .localentry CNAME(jl_plt_resolver), 1

    // Arguments are in r3-r10, f1-f13 and v2-v13. lr holds the caller's
    // return address: save it in the trampoline's frame.
    mflr    0
    std     0, 16(1)
    .cfi_offset lr, 16 - 48

    // Frame layout (from r1 after the stdu):
    //   0x000-0x01f: back chain, CR, LR and TOC save words (for our calls)
    //   0x020-0x05f: r3-r10
    //   0x060-0x0c7: f1-f13
    //   0x0c8-0x0cf: (padding)
    //   0x0d0-0x18f: v2-v13
    //   0x190:       the trampoline's frame (&entry at 0x190 + 32)
    stdu    1, -0x190(1)
    .cfi_def_cfa_offset 48 + 0x190
    std     3, 0x20(1)
    std     4, 0x28(1)
    std     5, 0x30(1)
    std     6, 0x38(1)
    std     7, 0x40(1)
    std     8, 0x48(1)
    std     9, 0x50(1)
    std     10, 0x58(1)
    stfd    1, 0x60(1)
    stfd    2, 0x68(1)
    stfd    3, 0x70(1)
    stfd    4, 0x78(1)
    stfd    5, 0x80(1)
    stfd    6, 0x88(1)
    stfd    7, 0x90(1)
    stfd    8, 0x98(1)
    stfd    9, 0xa0(1)
    stfd    10, 0xa8(1)
    stfd    11, 0xb0(1)
    stfd    12, 0xb8(1)
    stfd    13, 0xc0(1)
    li      0, 0xd0
    stvx    2, 1, 0
    li      0, 0xe0
    stvx    3, 1, 0
    li      0, 0xf0
    stvx    4, 1, 0
    li      0, 0x100
    stvx    5, 1, 0
    li      0, 0x110
    stvx    6, 1, 0
    li      0, 0x120
    stvx    7, 1, 0
    li      0, 0x130
    stvx    8, 1, 0
    li      0, 0x140
    stvx    9, 1, 0
    li      0, 0x150
    stvx    10, 1, 0
    li      0, 0x160
    stvx    11, 1, 0
    li      0, 0x170
    stvx    12, 1, 0
    li      0, 0x180
    stvx    13, 1, 0

    ld      3, 0x190 + 32(1)
    ld      12, 0(3)
    mtctr   12
    bctrl                       // entry->resolve(entry), at its global entry point
    mr      12, 3               // the resolved target, across the restores

    li      0, 0xd0
    lvx     2, 1, 0
    li      0, 0xe0
    lvx     3, 1, 0
    li      0, 0xf0
    lvx     4, 1, 0
    li      0, 0x100
    lvx     5, 1, 0
    li      0, 0x110
    lvx     6, 1, 0
    li      0, 0x120
    lvx     7, 1, 0
    li      0, 0x130
    lvx     8, 1, 0
    li      0, 0x140
    lvx     9, 1, 0
    li      0, 0x150
    lvx     10, 1, 0
    li      0, 0x160
    lvx     11, 1, 0
    li      0, 0x170
    lvx     12, 1, 0
    li      0, 0x180
    lvx     13, 1, 0
    lfd     1, 0x60(1)
    lfd     2, 0x68(1)
    lfd     3, 0x70(1)
    lfd     4, 0x78(1)
    lfd     5, 0x80(1)
    lfd     6, 0x88(1)
    lfd     7, 0x90(1)
    lfd     8, 0x98(1)
    lfd     9, 0xa0(1)
    lfd     10, 0xa8(1)
    lfd     11, 0xb0(1)
    lfd     12, 0xb8(1)
    lfd     13, 0xc0(1)
    ld      3, 0x20(1)
    ld      4, 0x28(1)
    ld      5, 0x30(1)
    ld      6, 0x38(1)
    ld      7, 0x40(1)
    ld      8, 0x48(1)
    ld      9, 0x50(1)
    ld      10, 0x58(1)
    addi    1, 1, 0x190
    .cfi_def_cfa_offset 48
    ld      0, 16(1)
    mtlr    0
    .cfi_restore lr
    // Drop the trampoline's frame: the target returns straight to the caller.
    addi    1, 1, 48
    .cfi_def_cfa_offset 0
    mtctr   12
    bctr
    .cfi_endproc
    EXPORT(jl_plt_resolver)

#endif
