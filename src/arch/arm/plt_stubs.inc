// This file is a part of Julia. License is MIT: https://julialang.org/license

// The PLT stub templates for arm (see src/plt.h).

// ARM code (Julia generates no Thumb code); `ldr pc` switches to Thumb state
// for a Thumb target. It reaches the slot under any code model.
static const char *plt_asm_arm =
    "ldr  ip, 1f\n"
    "2: ldr pc, [pc, ip]\n"
    "1: .word ${0}-(2b+8)\n";

// ARM code, like plt_asm_arm.
static const char *plt_tramp_asm_arm =
    "ldr  ip, 1f\n"
    "2: add ip, pc, ip\n"
    "str  ip, [sp, #-8]!\n"
    "b    ${1}\n"
    "1: .word ${0}-(2b+8)\n";
