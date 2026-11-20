// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (aarch64): the shared resolver for lazily bound calls (see
// src/plt.h). Each per-entry trampoline (from the templates in plt_stubs.inc)
// stores the address of its jl_plt_entry_t into a 16-byte slot below the stack
// pointer (`str x16, [sp, #-16]!`) and branches here, leaving lr as the caller
// set it.

#include "../common.h"

#if defined(__arm64__) || defined(__aarch64__)

    .text

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
CNAME(jl_plt_resolver):
    .cfi_startproc
    // The caller's sp is 16 bytes above ours: the per-entry trampoline pushed
    // &entry and a pad before branching here.
    .cfi_def_cfa_offset 16

    // Arguments are in x0-x7, q0-q7 and x8 (the indirect result). lr holds the
    // caller's return address.
    //
    // Frame layout (from sp after the stp):
    //   0x00-0x0f: frame record {x29, lr}
    //   0x10-0x4f: x0-x7
    //   0x50-0xcf: q0-q7
    //   0xd0-0xd7: x8 (indirect result)
    //   0xd8-0xdf: (padding)
    //   0xe0:      &entry (the trampoline's slot, above the frame)

    stp     x29, lr, [sp, #-0xe0]!
    .cfi_def_cfa_offset 0xe0 + 16   // +16 for the trampoline-pushed &entry + pad
    .cfi_rel_offset x29, 0x00
    .cfi_rel_offset lr, 0x08
    mov     x29, sp
    stp     x0, x1, [sp, #0x10]
    .cfi_rel_offset x0, 0x10
    .cfi_rel_offset x1, 0x18
    stp     x2, x3, [sp, #0x20]
    .cfi_rel_offset x2, 0x20
    .cfi_rel_offset x3, 0x28
    stp     x4, x5, [sp, #0x30]
    .cfi_rel_offset x4, 0x30
    .cfi_rel_offset x5, 0x38
    stp     x6, x7, [sp, #0x40]
    .cfi_rel_offset x6, 0x40
    .cfi_rel_offset x7, 0x48
    // The save slots of the vector argument registers are not described to
    // the unwinder.
    stp     q0, q1, [sp, #0x50]
    stp     q2, q3, [sp, #0x70]
    stp     q4, q5, [sp, #0x90]
    stp     q6, q7, [sp, #0xb0]
    str     x8,  [sp, #0xd0]
    .cfi_rel_offset x8, 0xd0

    ldr     x0, [sp, #0xe0]

    ldr     x16, [x0]
    blr     x16                 // entry->resolve(entry)
    mov     x16, x0             // the resolved target, across the restores

    ldr     x8,  [sp, #0xd0]
    .cfi_restore x8
    ldp     q6, q7, [sp, #0xb0]
    ldp     q4, q5, [sp, #0x90]
    ldp     q2, q3, [sp, #0x70]
    ldp     q0, q1, [sp, #0x50]
    ldp     x6, x7, [sp, #0x40]
    .cfi_restore x6
    .cfi_restore x7
    ldp     x4, x5, [sp, #0x30]
    .cfi_restore x4
    .cfi_restore x5
    ldp     x2, x3, [sp, #0x20]
    .cfi_restore x2
    .cfi_restore x3
    ldp     x0, x1, [sp, #0x10]
    .cfi_restore x0
    .cfi_restore x1
    // Drop our frame and the trampoline's slot: the target returns straight to
    // the caller.
    ldp     x29, lr, [sp], #0xe0
    .cfi_def_cfa_offset 16
    .cfi_restore x29
    .cfi_restore lr
    add     sp, sp, #16
    .cfi_def_cfa_offset 0

    br      x16
    .cfi_endproc
    EXPORT(jl_plt_resolver)

#endif // defined(__arm64__) || defined(__aarch64__)
