// This file is a part of Julia. License is MIT: https://julialang.org/license

// Inline-asm templates for the slot jump thunks (plt_asm_*) and the per-entry
// trampolines (plt_tramp_asm_*) on aarch64, which codegen emits and the tests
// assemble (for the operands, see src/plt.h). The trampolines follow the
// protocol of plt_resolver.S.

// x16, x17 (ip0, ip1) are the intra-procedure-call scratch registers
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

// Under the large code model (in the JIT, which is not position independent),
// the entry and the resolver may be out of PC-relative reach.
static const char *plt_tramp_asm_aarch64_large =
    "ldr   x16, 1f\n"
    "str   x16, [sp, #-16]!\n"
    "ldr   x16, 2f\n"
    "br    x16\n"
    ".p2align 3\n"
    "1: .quad ${0}\n"
    "2: .quad ${1}\n";
