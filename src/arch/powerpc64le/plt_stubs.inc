// This file is a part of Julia. License is MIT: https://julialang.org/license

// The PLT stub templates for powerpc64le (see src/plt.h).

// Finds the slot through the address `bcl` leaves in lr (saved in r0
// meanwhile), as r12 need not hold the thunk's address, and enters the target
// at its global entry point. Operand 1 is the thunk itself, which does not
// preserve the TOC, so that its callers save it. It reaches the slot under any
// code model.
static const char *plt_asm_ppc64le =
    ".localentry ${1}, 1\n"
    "mflr 0\n"
    "bcl  20, 31, 1f\n"
    "1: mflr 11\n"
    "mtlr 0\n"
    "ld   12, 2f-1b(11)\n"
    "add  11, 11, 12\n"
    "ld   12, 0(11)\n"
    "mtctr 12\n"
    "bctr\n"
    ".p2align 3\n"
    "2: .quad ${0}-1b\n";

// The stub enters the trampoline at its global entry point, with r12 holding
// its address. Operand 1 is a local pointer to the resolver, as an image cannot
// refer to it PC-relatively.
static const char *plt_tramp_asm_ppc64le =
    "0: ld   11, 3f-0b(12)\n"
    "add  11, 11, 12\n"
    "ld   0, 4f-0b(12)\n"
    "add  12, 12, 0\n"
    "ld   12, 0(12)\n"
    "stdu 1, -48(1)\n"
    "std  11, 32(1)\n"
    "mtctr 12\n"
    "bctr\n"
    ".p2align 3\n"
    "3: .quad ${0}-0b\n"
    "4: .quad ${1}-0b\n";
