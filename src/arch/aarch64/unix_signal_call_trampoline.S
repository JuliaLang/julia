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
    .p2align 2
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    // Mark as end of stack until frame is set up
    .cfi_undefined 1
    // x0 points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg)
    ldp     x1, x2, [x0]        // load pc (x1) and sp (x2)
    stp     x2, x1, [sp, #-16]! // push sp and pc (sp at lower addr, pc at higher addr)
    // stack layout: [sp, pc] (pc at higher address, like return address after call)
    .cfi_def_cfa sp, 16
    .cfi_offset lr, -8          // previous lr (pc) at CFA-8 (pc slot at sp+8)
    .cfi_offset sp, -16         // previous sp at CFA-16 (sp slot at sp+0)
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
