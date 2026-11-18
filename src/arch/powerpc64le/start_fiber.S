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

#if defined(_CPU_PPC64_) && !defined(_OS_WINDOWS_)

// N.B.: There is two iterations of the PPC64 ABI.
// v2 is current and used here. Make sure you have the
// correct version of the ABI reference when working on this code.
    .text
    .globl CNAME(jl_start_fiber_on_stack)
    HIDDEN(jl_start_fiber_on_stack)
    DEBUGINFO(jl_start_fiber_on_stack)
    .p2align 2
CNAME(jl_start_fiber_on_stack):
    .cfi_startproc
    .cfi_undefined lr
    // r3 = stack_top, r4 = fn
    // Move stack (-0x30 for initial stack frame) to stack pointer
    addi    1, 3, -0x30
    // Build stack frame
    // Skip local variable save area
    std     2, 0x28(1)          // Save TOC
    li      0, 0
    // Clear link editor/compiler words
    std     0, 0x20(1)
    std     0, 0x18(1)
    // Clear LR/CR save area
    std     0, 0x10(1)
    std     0, 0x8(1)
    std     0, 0x0(1)           // Clear back link to terminate unwinder
    mtlr    0                   // Clear link register
    mr      12, 4               // Set up target global entry point
    mtctr   12                  // Move jump target to counter register
    bctr                        // branch to counter (lr update disabled)
    trap
    .cfi_endproc
    EXPORT(jl_start_fiber_on_stack)

#endif
