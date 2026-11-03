// This file is a part of Julia. License is MIT: https://julialang.org/license

// The PLT stub templates for aarch64 (see src/plt.h).

static const char *plt_asm_aarch64 =
    "adrp  x16, ${0}\n"
    "ldr   x16, [x16, :lo12:${0}]\n"
    "br    x16\n";

static const char *plt_asm_aarch64_macho =
    "adrp  x16, ${0}@page\n"
    "ldr   x16, [x16, ${0}@pageoff]\n"
    "br    x16\n";

static const char *plt_asm_aarch64_large =
    "adr   x17, 1f\n"
    "ldr   x16, [x17]\n"
    "add   x16, x16, x17\n"
    "ldr   x16, [x16]\n"
    "br    x16\n"
    ".p2align 3\n"
    "1: .quad ${0}-1b\n";

static const char *plt_tramp_asm_aarch64 =
    "adrp  x16, ${0}\n"
    "add   x16, x16, :lo12:${0}\n"
    "str   x16, [sp, #-16]!\n"
    "b     ${1}\n";

static const char *plt_tramp_asm_aarch64_macho =
    "adrp  x16, ${0}@page\n"
    "add   x16, x16, ${0}@pageoff\n"
    "str   x16, [sp, #-16]!\n"
    "b     ${1}\n";

static const char *plt_tramp_asm_aarch64_large =
    "ldr   x16, 1f\n"
    "str   x16, [sp, #-16]!\n"
    "ldr   x16, 2f\n"
    "br    x16\n"
    ".p2align 3\n"
    "1: .quad ${0}\n"
    "2: .quad ${1}\n";
