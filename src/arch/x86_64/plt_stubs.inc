// This file is a part of Julia. License is MIT: https://julialang.org/license

// The PLT stub templates for x86_64 (see src/plt.h).

static const char *plt_asm_x86_64 =
    "jmpq  *${0:a}\n";

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

static const char *plt_tramp_asm_x86_64_large =
    "pushq $$0\n"
    "movabsq $$${0:c}, %r11\n"
    "pushq %r11\n"
    "movabsq $$${1:c}, %r11\n"
    "jmpq  *%r11\n";
