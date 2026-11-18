// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_ARCH_CFI_NORETURN_H
#define JL_ARCH_CFI_NORETURN_H

#include "../support/platform.h"

// If we've smashed the stack, (and not just normal NORETURN)
// this will smash stack-unwind too
#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_64_)
    // install the unhandled exception handler at the top of our stack
    // to call directly into our personality handler
// N.B. do not switch sections here: with function sections, which LTO turns on,
// the rest of the function would be emitted away from its own section, leaving
// the entry point running off the end of a prologue.
#define CFI_NORETURN \
    asm volatile ("\t.seh_handler __julia_personality, @except");
#else
#define CFI_NORETURN
#endif
#else
// wipe out the call-stack unwind capability beyond this function
// (we are noreturn, so it is not a total lie)
// An undefined return address marks the end of the call chain (DWARF 5, 6.4.4).
// This takes effect where the directive is, after the prologue: before it, the
// return address is wherever the function was entered with it, which is zero
// for start_task (see arch/<arch>/start_fiber.S) and the real caller elsewhere.
// N.B. this directive applies to whichever function the code ends up in, so
// every function using CFI_NORETURN must be NOINLINE: inlined into a caller,
// unwinding through any suspended task switch would stop there (the PGO+ThinLTO
// macOS aarch64 build inlined jl_start_fiber_set into ctx_switch and
// jl_start_fiber_swap).
#if defined(_CPU_X86_64_)
#define CFI_NORETURN asm volatile ("\t.cfi_undefined rip");
#elif defined(_CPU_X86_)
#define CFI_NORETURN asm volatile ("\t.cfi_undefined eip");
#elif defined(_CPU_AARCH64_)
#define CFI_NORETURN asm volatile ("\t.cfi_undefined x30");
#elif defined(_CPU_ARM_) || defined(_CPU_PPC64_) || defined(_CPU_PPC_)
#define CFI_NORETURN asm volatile ("\t.cfi_undefined lr");
#elif defined(_CPU_RISCV64_)
#define CFI_NORETURN asm volatile ("\t.cfi_undefined ra");
#else
#error "CFI_NORETURN: the return address register of this CPU is not known"
#endif
#endif

#endif
