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

#if defined(_CPU_X86_) && !defined(_OS_WINDOWS_)

    .text
    .globl CNAME(jl_start_fiber_on_stack)
    HIDDEN(jl_start_fiber_on_stack)
    DEBUGINFO(jl_start_fiber_on_stack)
CNAME(jl_start_fiber_on_stack):
    .cfi_startproc
    .cfi_undefined 1
    .cfi_return_column 1
    CET_START()
    movl    4(%esp), %eax       // stack_top
    movl    8(%esp), %ecx       // fn
    movl    %eax, %esp
    xorl    %ebp, %ebp
    push    %ebp                // instead of a return address
    jmpl    *%ecx               // call `fn` with fake stack frame
    ud2
    .cfi_endproc
    EXPORT(jl_start_fiber_on_stack)

#endif
