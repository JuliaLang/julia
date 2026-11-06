// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (riscv64): the shared resolver for lazily bound calls (see
// src/plt.h). Each per-entry trampoline (from the templates in plt_stubs.inc)
// stores the address of its jl_plt_entry_t into a 16-byte slot below the stack
// pointer (`addi sp, sp, -16; sd t0, 0(sp)`) and jumps here, leaving ra as the
// caller set it. Once the resolver has read the address, the slot holds its
// frame record ({fp, ra}).

#include "../common.h"

#if defined(__riscv) && __riscv_xlen == 64

    .text

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
CNAME(jl_plt_resolver):
    .cfi_startproc
    // The caller's sp is 16 bytes above ours: the per-entry trampoline's slot.
    .cfi_def_cfa_offset 16

    // Arguments are in a0-a7 and fa0-fa7. ra holds the caller's return address.
    //
    // Frame layout (from sp after addi):
    //   0x00-0x07: a0 (x10)
    //   0x08-0x0f: a1 (x11)
    //   0x10-0x17: a2 (x12)
    //   0x18-0x1f: a3 (x13)
    //   0x20-0x27: a4 (x14)
    //   0x28-0x2f: a5 (x15)
    //   0x30-0x37: a6 (x16)
    //   0x38-0x3f: a7 (x17)
    //   0x40-0x4f: (padding)
    //   0x50-0x57: fa0 (f10)
    //   0x58-0x5f: fa1 (f11)
    //   0x60-0x67: fa2 (f12)
    //   0x68-0x6f: fa3 (f13)
    //   0x70-0x77: fa4 (f14)
    //   0x78-0x7f: fa5 (f15)
    //   0x80-0x87: fa6 (f16)
    //   0x88-0x8f: fa7 (f17)
    //   0x90:      &entry (the trampoline's slot, above the frame), then fp
    //   0x98:      (the trampoline's pad), then ra

    addi    sp, sp, -0x90
    .cfi_def_cfa_offset 0x90 + 16   // +16 for trampoline-pushed (&entry + pad)

    sd      a0, 0x00(sp)
    .cfi_rel_offset x10, 0x00
    sd      a1, 0x08(sp)
    .cfi_rel_offset x11, 0x08
    sd      a2, 0x10(sp)
    .cfi_rel_offset x12, 0x10
    sd      a3, 0x18(sp)
    .cfi_rel_offset x13, 0x18
    sd      a4, 0x20(sp)
    .cfi_rel_offset x14, 0x20
    sd      a5, 0x28(sp)
    .cfi_rel_offset x15, 0x28
    sd      a6, 0x30(sp)
    .cfi_rel_offset x16, 0x30
    sd      a7, 0x38(sp)
    .cfi_rel_offset x17, 0x38

    fsd     fa0, 0x50(sp)
    .cfi_rel_offset f10, 0x50
    fsd     fa1, 0x58(sp)
    .cfi_rel_offset f11, 0x58
    fsd     fa2, 0x60(sp)
    .cfi_rel_offset f12, 0x60
    fsd     fa3, 0x68(sp)
    .cfi_rel_offset f13, 0x68
    fsd     fa4, 0x70(sp)
    .cfi_rel_offset f14, 0x70
    fsd     fa5, 0x78(sp)
    .cfi_rel_offset f15, 0x78
    fsd     fa6, 0x80(sp)
    .cfi_rel_offset f16, 0x80
    fsd     fa7, 0x88(sp)
    .cfi_rel_offset f17, 0x88

    // &entry, from the trampoline's slot, which then holds the frame record
    ld      a0, 0x90(sp)
    sd      s0, 0x90(sp)
    .cfi_rel_offset x8, 0x90
    sd      ra, 0x98(sp)
    .cfi_rel_offset x1, 0x98
    addi    s0, sp, 0xa0

    ld      t1, 0(a0)
    jalr    t1                  // entry->resolve(entry)
    mv      t0, a0              // the resolved target, across the restores

    fld     fa7, 0x88(sp)
    .cfi_restore f17
    fld     fa6, 0x80(sp)
    .cfi_restore f16
    fld     fa5, 0x78(sp)
    .cfi_restore f15
    fld     fa4, 0x70(sp)
    .cfi_restore f14
    fld     fa3, 0x68(sp)
    .cfi_restore f13
    fld     fa2, 0x60(sp)
    .cfi_restore f12
    fld     fa1, 0x58(sp)
    .cfi_restore f11
    fld     fa0, 0x50(sp)
    .cfi_restore f10

    ld      ra, 0x98(sp)
    .cfi_restore x1
    ld      s0, 0x90(sp)
    .cfi_restore x8
    ld      a7, 0x38(sp)
    .cfi_restore x17
    ld      a6, 0x30(sp)
    .cfi_restore x16
    ld      a5, 0x28(sp)
    .cfi_restore x15
    ld      a4, 0x20(sp)
    .cfi_restore x14
    ld      a3, 0x18(sp)
    .cfi_restore x13
    ld      a2, 0x10(sp)
    .cfi_restore x12
    ld      a1, 0x08(sp)
    .cfi_restore x11
    ld      a0, 0x00(sp)
    .cfi_restore x10

    // Drop our frame and the trampoline's slot: the target returns straight to
    // the caller.
    addi    sp, sp, 0xa0
    .cfi_def_cfa_offset 0

    jr      t0
    .cfi_endproc
    EXPORT(jl_plt_resolver)

#endif // defined(__riscv) && __riscv_xlen == 64
