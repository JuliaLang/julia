// This file is a part of Julia. License is MIT: https://julialang.org/license

// Lazy binding through slots ("PLT entries").
//
// A call that is bound lazily goes through a stub that jumps through a slot.
// The slot starts out pointing at a per-entry trampoline, which passes the
// address of its jl_plt_entry_t to the shared resolver, jl_plt_resolver
// (src/arch/<arch>/plt_resolver.S, which describes the protocol for each
// architecture). The resolver saves the argument registers, calls the entry's
// resolve function, which binds the slot and returns the target, restores the
// argument registers and jumps to the target, with the caller's arguments and
// return address intact. Later calls go through the slot straight to the
// target.
//
// The trampoline passes the entry's address on the stack: a scratch register
// would not survive the system's lazy binding of jl_plt_resolver itself.
//
// The protocol preserves C ABI arguments, including i686 fastcall/thiscall.
// Before using another LLVM calling convention, check both the resolver and
// its entry trampoline: their scratch registers need not be scratch under
// that convention.
//
// The entry is the start of a larger structure describing what to bind, which
// the resolve function reads.
//
// The stubs (slot jump thunks) and the per-entry trampolines are emitted from
// the inline-asm templates in src/arch/<arch>/plt_stubs.inc, which take these
// operands (in LLVM's syntax: `${N}`, or `${N:c}` for a bare symbol and
// `${N:a}` for an address on x86, and `$$` for a `$`):
//  - plt_asm_*: 0 is the slot; 1 is the PC thunk (i686 ELF) or the thunk itself
//    (PPC64LE).
//  - plt_tramp_asm_*: 0 is the entry; 1 is the resolver, or a local pointer to
//    it (i686, PPC64LE); 2 is the PC thunk (i686 ELF).

#ifndef JL_PLT_H
#define JL_PLT_H

#include "support/dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _jl_plt_entry_t jl_plt_entry_t;
struct _jl_plt_entry_t {
    // Binds the slot (stores the target into it) and returns the target. It
    // may throw, leaving the slot unbound, so that the next call resolves
    // again.
    void *(*resolve)(jl_plt_entry_t *entry);
    // The slot the stub jumps through
    void **slot;
};

// The shared resolver, jumped to by every per-entry trampoline.
JL_DLLEXPORT void jl_plt_resolver(void);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // JL_PLT_H
