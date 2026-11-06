// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_plt_resolver (ARMv7): the shared resolver for lazily bound calls (see
// src/plt.h). Each per-entry trampoline (from the templates in plt_stubs.inc)
// stores the address of its jl_plt_entry_t into an 8-byte slot below the stack
// pointer (`str ip, [sp, #-8]!`) and branches here, leaving lr as the caller
// set it. The frame record ({fp, lr}, fp pointing at lr) is laid out as GCC
// lays it out for ARM code.

#include "../common.h"

#if defined(_CPU_ARM_) && !defined(_OS_WINDOWS_)

    .text
    .syntax unified
    .arm

    .globl CNAME(jl_plt_resolver)
    DEBUGINFO(jl_plt_resolver)
    .p2align 2
CNAME(jl_plt_resolver):
    .fnstart
    .cfi_startproc
    // The caller's sp is 8 bytes above ours: the per-entry trampoline's slot.
    .pad    #8
    .cfi_def_cfa_offset 8

    // Arguments are in r0-r3 and, with hard floats, d0-d7 (s0-s15). lr holds
    // the caller's return address (with the Thumb bit for a Thumb caller).
    //
    // Frame layout (from sp after the push and sub):
    //   0x00-0x3f: d0-d7
    //   0x40-0x4f: r0-r3
    //   0x50-0x57: frame record {fp, lr}
    //   0x58:      &entry (the trampoline's slot, above the frame)

    push    {r0-r3, fp, lr}
    .save   {r0-r3, fp, lr}
    .cfi_def_cfa_offset 8 + 0x18
    .cfi_rel_offset r0, 0x00
    .cfi_rel_offset r1, 0x04
    .cfi_rel_offset r2, 0x08
    .cfi_rel_offset r3, 0x0c
    .cfi_rel_offset fp, 0x10
    .cfi_rel_offset lr, 0x14
    add     fp, sp, #0x14
    sub     sp, sp, #0x40
    .pad    #0x40
    .cfi_def_cfa_offset 8 + 0x18 + 0x40
#if !defined(__SOFTFP__)
    // The save slots of the vector argument registers are not described to
    // the unwinder.
    vstmia  sp, {d0-d7}
#endif

    ldr     r0, [sp, #0x58]
    ldr     ip, [r0]
    blx     ip                  // entry->resolve(entry)
    mov     ip, r0              // the resolved target, across the restores

#if !defined(__SOFTFP__)
    vldmia  sp, {d0-d7}
#endif
    add     sp, sp, #0x40
    .cfi_def_cfa_offset 8 + 0x18
    pop     {r0-r3, fp, lr}
    .cfi_def_cfa_offset 8
    .cfi_restore r0
    .cfi_restore r1
    .cfi_restore r2
    .cfi_restore r3
    .cfi_restore fp
    .cfi_restore lr
    // Drop the trampoline's slot: the target returns straight to the caller.
    add     sp, sp, #8
    .cfi_def_cfa_offset 0
    bx      ip                  // in Thumb state if the target is Thumb code
    .cfi_endproc
    .fnend
    EXPORT(jl_plt_resolver)

#endif
