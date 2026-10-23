// This file is a part of Julia. License is MIT: https://julialang.org/license

// Inline-asm templates for the slot jump thunks (plt_asm_*) and the per-entry
// trampolines (plt_tramp_asm_*) on x86_64, which codegen emits and the tests
// assemble (for the operands, see src/plt.h). The trampolines follow the
// protocol of plt_resolver.S.

static const char *plt_asm_x86_64 =
    "jmpq  *${0:a}\n";

// Under the large code model, the slot may be out of PC-relative reach.
static const char *plt_asm_x86_64_large =
    "movabsq $$${0:c}, %r11\n"
    "jmpq    *(%r11)\n";

// Pushes 16 bytes (a pad and &entry), so that the resolver is entered with the
// stack aligned as right after a `call`, as _dl_runtime_resolve assumes when
// binding the resolver itself on the first call.
static const char *plt_tramp_asm_x86_64 =
    "pushq $$0\n"
    "leaq  ${0:c}(%rip), %r11\n"
    "pushq %r11\n"
    "jmp   ${1:c}\n";

// Under the large code model (in the JIT, which is not position independent),
// the entry and the resolver may be out of PC-relative reach.
static const char *plt_tramp_asm_x86_64_large =
    "pushq $$0\n"
    "movabsq $$${0:c}, %r11\n"
    "pushq %r11\n"
    "movabsq $$${1:c}, %r11\n"
    "jmpq  *%r11\n";
