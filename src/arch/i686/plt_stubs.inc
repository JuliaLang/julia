// This file is a part of Julia. License is MIT: https://julialang.org/license

// The PLT stub templates for i686 (see src/plt.h).

// i686 has no PC-relative addressing: the PC comes from a thunk (operand 1),
// and %eax is the scratch register, as it is never an argument register in the
// calling conventions Julia emits. The address space is 32 bits wide, so this
// reaches the slot under any code model.
static const char *plt_asm_i686 =
    "calll ${1:c}\n"
    "1: movl ${0:c}-1b(%eax), %eax\n"
    "jmpl  *%eax\n";

// Pushes 16 bytes (12 of padding, then &entry), so that the resolver is entered
// with the stack aligned as right after a `call`. Operand 1 is a local pointer
// to the resolver (a call through the PLT would need %ebx to hold the GOT
// address), operand 2 the PC thunk.
static const char *plt_tramp_asm_i686 =
    "subl  $$12, %esp\n"
    "calll ${2:c}\n"
    "1: pushl ${1:c}-1b(%eax)\n"
    "leal  ${0:c}-1b(%eax), %eax\n"
    "xchgl %eax, (%esp)\n"
    "jmpl  *%eax\n";

// On Windows, where code is not position independent (it is relocated through
// base relocations) and COFF cannot express the differences above, the
// addresses are absolute. Operand 1 of the trampoline is a local pointer to the
// resolver.
static const char *plt_asm_i686_coff =
    "jmpl  *${0:c}\n";

static const char *plt_tramp_asm_i686_coff =
    "subl  $$12, %esp\n"
    "pushl $$${0:c}\n"
    "jmpl  *${1:c}\n";
