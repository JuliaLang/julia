#include "../cli/trampolines/common.h"

#if defined(__arm64__) || defined(__aarch64__)

    .text

    .globl CNAME(__orc_rt_sysv_reenter)
CNAME(__orc_rt_sysv_reenter):
    .cfi_startproc
    .cfi_rel_offset fp, 0x00
    .cfi_rel_offset lr, 0x08

    stp     x0, x1, [sp, #-0xd0]!
    .cfi_def_cfa_offset 0xd0
    .cfi_rel_offset x0, 0x00
    .cfi_rel_offset x1, 0x08
    stp     x2, x3, [sp, #0x10]
    .cfi_rel_offset x2, 0x10
    .cfi_rel_offset x3, 0x18
    stp     x4, x5, [sp, #0x20]
    .cfi_rel_offset x4, 0x20
    .cfi_rel_offset x5, 0x28
    stp     x6, x7, [sp, #0x30]
    .cfi_rel_offset x6, 0x30
    .cfi_rel_offset x7, 0x38
    stp     q0, q1, [sp, #0x40]
    .cfi_rel_offset q0, 0x40
    .cfi_rel_offset q1, 0x50
    stp     q2, q3, [sp, #0x60]
    .cfi_rel_offset q2, 0x60
    .cfi_rel_offset q3, 0x70
    stp     q4, q5, [sp, #0x80]
    .cfi_rel_offset q4, 0x80
    .cfi_rel_offset q5, 0x90
    stp     q6, q7, [sp, #0xa0]
    .cfi_rel_offset q6, 0xa0
    .cfi_rel_offset q7, 0xb0
    str     x8,     [sp, #0xc0]
    .cfi_rel_offset x8, 0xc0

    // Trampoline address = return address - 8
    sub     x0, lr, #8

    bl      CNAME(__orc_rt_jl_resolve)
    mov     x16, x0

    ldr     x8,     [sp, #0xc0]
    .cfi_restore x8
    ldp     q6, q7, [sp, #0xa0]
    .cfi_restore q6
    .cfi_restore q7
    ldp     q4, q5, [sp, #0x80]
    .cfi_restore q4
    .cfi_restore q5
    ldp     q2, q3, [sp, #0x60]
    .cfi_restore q2
    .cfi_restore q3
    ldp     q0, q1, [sp, #0x40]
    .cfi_restore q0
    .cfi_restore q1
    ldp     x6, x7, [sp, #0x30]
    .cfi_restore x6
    .cfi_restore x7
    ldp     x4, x5, [sp, #0x20]
    .cfi_restore x4
    .cfi_restore x5
    ldp     x2, x3, [sp, #0x10]
    .cfi_restore x2
    .cfi_restore x3
    ldp     x0, x1, [sp], #0xd0
    .cfi_restore x0
    .cfi_restore x1
    .cfi_def_cfa_offset 0x00
    ldp     fp, lr, [sp], #0x10
    .cfi_restore fp
    .cfi_restore lr
    .cfi_def_cfa_offset 0

    br      x16
    .cfi_endproc

#endif // defined(__arm64__) || defined(__aarch64__)
