// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_start_fiber_on_stack(stack_top, fn): switch to the stack ending at
// stack_top and jump to fn with a terminated call chain. It never returns.
//
// Coupled to C code in src/task.c: jl_start_fiber_set calls it with the top of
// the new task's stack and &start_task, which must not return. Task switching
// uses it only when JL_TASK_SWITCH_ASM is defined (src/julia_threads.h), i.e.
// not on Windows. The unwind info marks the end of the call chain like
// CFI_NORETURN (src/arch/cfi_noreturn.h).

#include "../common.h"

#if defined(_CPU_AARCH64_) && !defined(_OS_WINDOWS_)

    .text
    .globl CNAME(jl_start_fiber_on_stack)
    HIDDEN(jl_start_fiber_on_stack)
    DEBUGINFO(jl_start_fiber_on_stack)
    .p2align 2
CNAME(jl_start_fiber_on_stack):
    .cfi_startproc
    .cfi_undefined x30
    // x0 = stack_top, x1 = fn
    mov     sp, x0
    mov     x29, xzr            // Clear frame pointer (x29)
    mov     x30, xzr            // and link register (x30) to terminate unwinder.
    br      x1                  // call `fn` with fake stack frame
    brk     #0x1                // abort
    .cfi_endproc
    EXPORT(jl_start_fiber_on_stack)

#endif
