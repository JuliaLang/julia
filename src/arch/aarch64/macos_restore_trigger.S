// This file is a part of Julia. License is MIT: https://julialang.org/license

// The return address planted under a function that the signal handling code
// makes an interrupted thread call (macOS): when that function returns, the
// trigger reads from a page with no access, and the fault handler restores the
// interrupted state that was saved on the stack. It must not move the stack
// pointer before the faulting read: the handler finds the saved state relative
// to it.
//
// Coupled to C code in src/signals-mach.c: jl_call_in_state saves the
// interrupted thread state at the new stack pointer and sets the link register
// to jl_mach_restore_trigger, which reads from jl_mach_restore_page (mapped by
// init_mach_restore_trigger). catch_mach_exception_raise recognizes the fault
// with jl_addr_is_restore_trigger and restores the state from the stack
// pointer.

#include "../common.h"

#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)

    .text
    .globl CNAME(jl_mach_restore_trigger)
    HIDDEN(jl_mach_restore_trigger)
    .p2align 2
CNAME(jl_mach_restore_trigger):
    adrp    x16, PAGE(CNAME(jl_mach_restore_page))
    ldr     x16, [x16, PAGEOFF(CNAME(jl_mach_restore_page))]
    ldr     x17, [x16]          // EXC_BAD_ACCESS here
    brk     #1                  // should never reach here

#endif
