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

#if defined(_CPU_RISCV64_) && !defined(_OS_WINDOWS_)

    .text
    .globl CNAME(jl_start_fiber_on_stack)
    HIDDEN(jl_start_fiber_on_stack)
    DEBUGINFO(jl_start_fiber_on_stack)
    .p2align 2
CNAME(jl_start_fiber_on_stack):
    .cfi_startproc
    .cfi_undefined ra
    // a0 = stack_top, a1 = fn
    mv      sp, a0
    mv      ra, zero            // Clear return address register
    mv      fp, zero            // Clear frame pointer
    jr      a1                  // call `fn` with fake stack frame
    ebreak                      // abort
    .cfi_endproc
    EXPORT(jl_start_fiber_on_stack)

#endif
