// This file is a part of Julia. License is MIT: https://julialang.org/license

// The return address planted under a function that the signal handling code
// makes an interrupted thread call (macOS): when that function returns, the
// trigger reads from a page with no access, and the fault handler restores the
// interrupted state that was saved on the stack. It must not move the stack
// pointer before the faulting read: the handler finds the saved state relative
// to it.
//
// Coupled to C code in src/signals-mach.c: jl_call_in_state saves the
// interrupted thread state at the new stack pointer and returns into
// jl_mach_restore_trigger, which reads from jl_mach_restore_page (mapped by
// init_mach_restore_trigger). catch_mach_exception_raise recognizes the fault
// with jl_addr_is_restore_trigger and restores the state from the stack
// pointer.

#include "../common.h"

#if defined(_OS_DARWIN_) && defined(_CPU_X86_64_)

    .text
    .globl CNAME(jl_mach_restore_trigger)
    HIDDEN(jl_mach_restore_trigger)
CNAME(jl_mach_restore_trigger):
    movq    CNAME(jl_mach_restore_page)(%rip), %r11
    movq    (%r11), %r11        // EXC_BAD_ACCESS here
    ud2                         // should never reach here

#endif
