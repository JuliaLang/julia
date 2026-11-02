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
    // The unwind info describes the interrupted code's frame from the thread
    // state saved at sp (_STRUCT_ARM_THREAD_STATE64), where the function that
    // returns here left it.
    .cfi_startproc
    .cfi_signal_frame
    // CFA = the saved sp, [sp + 248] (DW_CFA_def_cfa_expression: DW_OP_breg31 248, DW_OP_deref)
    // lr (the pc) at [sp + 256] (DW_CFA_expression: DW_OP_breg31 256)
    .cfi_escape 0x0f, 0x04, 0x8f, 0xf8, 0x01, 0x06
    .cfi_escape 0x10, 0x1e, 0x03, 0x8f, 0x80, 0x02
    nop                         // the return address is past this (&jl_mach_restore_trigger + 4), so that
                                // unwinders that look up a return address less one find this function
    adrp    x16, PAGE(CNAME(jl_mach_restore_page))
    ldr     x16, [x16, PAGEOFF(CNAME(jl_mach_restore_page))]
    ldr     x17, [x16]          // EXC_BAD_ACCESS here
    brk     #1                  // should never reach here
    .cfi_endproc

#endif
