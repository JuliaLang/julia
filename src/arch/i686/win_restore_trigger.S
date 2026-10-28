// This file is a part of Julia. License is MIT: https://julialang.org/license

// The return address planted under a function that the signal handling code
// makes an interrupted thread call (Windows): when that function returns, the
// trigger reads from a page with no access, and the fault handler restores the
// interrupted state that was saved on the stack. It must not move the stack
// pointer before the faulting read: the handler finds the saved state relative
// to it.
//
// Coupled to C code in src/signals-win.c: jl_win_call_in_context saves the
// CONTEXT above the argument slot under the return address, and returns into
// jl_win_restore_trigger, which reads from jl_win_restore_page.
// jl_win_restore_veh recognizes the fault with jl_addr_is_win_restore_trigger
// and restores the CONTEXT at Esp + 4.

#include "../common.h"

#if defined(_OS_WINDOWS_) && defined(_CPU_X86_)

    .text
    .globl CNAME(jl_win_restore_trigger)
CNAME(jl_win_restore_trigger):
    movl    CNAME(jl_win_restore_page), %eax
    movl    (%eax), %eax        // EXCEPTION_ACCESS_VIOLATION here
    ud2                         // should never reach here

#endif
