// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_start_fiber_on_stack(stack_top, fn): switch to the stack ending at
// stack_top and jump to fn with a terminated call chain. It never returns.
//
// Coupled to C code in src/task.c: jl_start_fiber_set calls it with the top of
// the new task's stack and &start_task, which must not return. Task switching
// uses it only when JL_TASK_SWITCH_ASM is defined (src/julia_threads.h), i.e.
// not on Windows. The unwind info marks the end of the call chain like
// CFI_NORETURN (src/julia_internal.h).

#include "../common.h"

#if defined(_CPU_ARM_) && !defined(_OS_WINDOWS_)

    .text
    .syntax unified
    .arm
    .globl CNAME(jl_start_fiber_on_stack)
    HIDDEN(jl_start_fiber_on_stack)
    DEBUGINFO(jl_start_fiber_on_stack)
    .p2align 2
CNAME(jl_start_fiber_on_stack):
    .cfi_startproc
    .cfi_undefined 1
    .cfi_return_column 1
    // r0 = stack_top, r1 = fn
    mov     sp, r0
    mov     lr, #0              // Clear link register (lr)
    mov     r11, #0             // and the frame pointers of both ARM (r11)
    mov     r7, #0              // and Thumb (r7) code to terminate unwinder.
    bx      r1                  // call `fn` with fake stack frame, in Thumb state if
                                // `fn` is Thumb code (the low bit of its address)
    udf     #0                  // abort
    .cfi_endproc
    EXPORT(jl_start_fiber_on_stack)

#endif
