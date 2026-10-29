// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_fake_signal_return: the function a signal handler "calls" to run fptr on
// the interrupted thread (Linux and FreeBSD). It rebuilds a frame for the
// interrupted pc/sp, so that unwinding from fptr continues into the
// interrupted code, and calls fptr, which must not return.
//
// Coupled to C code in src/signals-unix.c: jl_call_in_ctx saves the
// interrupted pc, sp, the target fptr and the original value of x0 in
// ptls->signal_ctx_pc, signal_ctx_sp, signal_ctx_fptr and signal_ctx_arg,
// points x0 at &ptls->signal_ctx_pc, and resumes the thread here. The loads
// below depend on those four fields being consecutive, in this order, in
// jl_tls_states_t (src/julia_threads.h).
// signals-unix.c falls back to a C definition where this file defines none.

#include "../common.h"

#if (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_AARCH64_)

    .text
    .globl CNAME(jl_fake_signal_return)
    HIDDEN(jl_fake_signal_return)
    DEBUGINFO(jl_fake_signal_return)
    .p2align 4                  // as the start of the .text of the C file it was in
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    // x0 points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg).
    // The CFA is the interrupted sp throughout, as unwinders that take the
    // caller's sp to be the CFA need (libunwind, despite a rule for sp).
    // Until the frame below is built, through x0:
    // CFA = [x0 + 8] (DW_CFA_def_cfa_expression: DW_OP_breg0 8, DW_OP_deref)
    // lr (the pc) at [x0] (DW_CFA_expression: DW_OP_breg0 0)
    .cfi_escape 0x0f, 0x03, 0x70, 0x08, 0x06
    .cfi_escape 0x10, 0x1e, 0x02, 0x70, 0x00
    ldp     x1, x2, [x0]        // load pc (x1) and sp (x2)
    stp     x29, x1, [sp, #-32]! // frame record {x29, pc}
    str     x2, [sp, #16]       // and sp above it
    mov     x29, sp
    // From here, through the frame:
    // CFA = [x29 + 16] (DW_CFA_def_cfa_expression: DW_OP_breg29 16, DW_OP_deref)
    // x29 at [x29] and lr (the pc) at [x29 + 8] (DW_CFA_expression: DW_OP_breg29 0 / 8)
    .cfi_escape 0x0f, 0x03, 0x8d, 0x10, 0x06
    .cfi_escape 0x10, 0x1d, 0x02, 0x8d, 0x00
    .cfi_escape 0x10, 0x1e, 0x02, 0x8d, 0x08
    // This is not quite valid, since the AArch64 DWARF spec lacks the ability to define how to restore the LR register correctly,
    // so normally libunwind implementations on linux detect this function specially and hack around the invalid info:
    // https://github.com/llvm/llvm-project/commit/c82deed6764cbc63966374baf9721331901ca958
    ldp     x1, x2, [x0, #16]   // load fptr (x1) and saved x0 (x2)
    mov     x0, x2              // restore original x0
    blr     x1                  // call fptr
    brk     #1                  // unreachable
    .cfi_endproc
    EXPORT(jl_fake_signal_return)

#endif
