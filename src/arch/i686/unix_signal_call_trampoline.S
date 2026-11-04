// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_fake_signal_return: the function a signal handler "calls" to run fptr on
// the interrupted thread (Linux and FreeBSD). It rebuilds a frame for the
// interrupted pc/sp, so that unwinding from fptr continues into the
// interrupted code, and calls fptr, which must not return.
//
// Coupled to C code in src/signals-unix.c: jl_call_in_ctx saves the
// interrupted pc, sp, the target fptr and the original value of %eax in
// ptls->signal_ctx_pc, signal_ctx_sp, signal_ctx_fptr and signal_ctx_arg,
// points %eax at &ptls->signal_ctx_pc, and resumes the thread here. The loads
// below depend on those four fields being consecutive, in this order, in
// jl_tls_states_t (src/julia_threads.h).
// signals-unix.c falls back to a C definition where this file defines none.

#include "../common.h"

#if (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_X86_)

    .text
    .globl CNAME(jl_fake_signal_return)
    HIDDEN(jl_fake_signal_return)
    DEBUGINFO(jl_fake_signal_return)
    .p2align 4                  // as the start of the .text of the C file it was in
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    // eax points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg).
    // The CFA is the interrupted sp throughout (so that no unwinder needs to
    // follow a rule for sp). Until the frame below is built, through eax:
    // CFA = [eax + 4] (DW_CFA_def_cfa_expression: DW_OP_breg0 4, DW_OP_deref)
    // eip at [eax] (DW_CFA_expression: DW_OP_breg0 0)
    .cfi_escape 0x0f, 0x03, 0x70, 0x04, 0x06
    .cfi_escape 0x10, 0x08, 0x02, 0x70, 0x00
    pushl   (%eax)              // push pc (signal_ctx_pc), as the return address
    pushl   %ebp                // frame record {ebp, pc}
    // ebp at [esp] (DW_CFA_expression: DW_OP_breg4 0)
    .cfi_escape 0x10, 0x05, 0x02, 0x74, 0x00
    movl    %esp, %ebp
    pushl   4(%eax)             // push sp (signal_ctx_sp)
    // From here, through the frame:
    // CFA = [ebp - 4] (DW_CFA_def_cfa_expression: DW_OP_breg5 -4, DW_OP_deref)
    // eip at [ebp + 4] and ebp at [ebp] (DW_CFA_expression: DW_OP_breg5 4 / 0)
    .cfi_escape 0x0f, 0x03, 0x75, 0x7c, 0x06
    .cfi_escape 0x10, 0x08, 0x02, 0x75, 0x04
    .cfi_escape 0x10, 0x05, 0x02, 0x75, 0x00
    pushl   8(%eax)             // push fptr (signal_ctx_fptr), leaving the stack aligned
    movl    12(%eax), %eax      // restore original eax from signal_ctx_arg
    cld                         // the interrupted code may have set the direction flag
    calll   *(%esp)             // call fptr
    ud2                         // unreachable
    .cfi_endproc
    EXPORT(jl_fake_signal_return)

#endif
