// This file is a part of Julia. License is MIT: https://julialang.org/license

// The PLT stub templates for riscv64 (see src/plt.h).

static const char *plt_asm_riscv64 =
    "1:  auipc t3, %pcrel_hi(${0})\n"
    "    ld    t3, %pcrel_lo(1b)(t3)\n"
    "    jalr  t1, t3";

// (the thunk is 8-byte aligned, so that the literal is at offset 24)
static const char *plt_asm_riscv64_large =
    ".option push\n"
    ".option norvc\n"
    "1:  auipc t3, 0\n"
    "    ld    t1, 24(t3)\n"
    "    add   t3, t3, t1\n"
    "    ld    t3, 0(t3)\n"
    "    jr    t3\n"
    "    .p2align 3\n"
    "    .quad ${0}-1b\n"
    ".option pop\n";

static const char *plt_tramp_asm_riscv64 =
    "1: auipc t0, %pcrel_hi(${0})\n"
    "   addi  t0, t0, %pcrel_lo(1b)\n"
    "   addi  sp, sp, -16\n"
    "   sd    t0, 0(sp)\n"
    "   tail  ${1}\n"; // through the PLT: the resolver is in another library

// (the trampoline is 8-byte aligned, so that the literals are aligned)
static const char *plt_tramp_asm_riscv64_large =
    ".option push\n"
    ".option norvc\n"
    "1: auipc t0, 0\n"
    "   ld    t1, 32(t0)\n"
    "   ld    t0, 24(t0)\n"
    "   addi  sp, sp, -16\n"
    "   sd    t0, 0(sp)\n"
    "   jr    t1\n"
    "   .dword ${0}\n"
    "   .dword ${1}\n"
    ".option pop\n";
