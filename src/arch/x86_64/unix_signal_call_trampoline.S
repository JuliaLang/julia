// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_fake_signal_return: the function a signal handler "calls" to run fptr on
// the interrupted thread (Linux, FreeBSD and OpenBSD). It rebuilds a frame for
// the interrupted pc/sp, so that unwinding from fptr continues into the
// interrupted code, and calls fptr, which must not return.
//
// Coupled to C code in src/signals-unix.c: jl_call_in_ctx saves the
// interrupted pc, sp, the target fptr and the original value of %rdi in
// ptls->signal_ctx_pc, signal_ctx_sp, signal_ctx_fptr and signal_ctx_arg,
// points %rdi at &ptls->signal_ctx_pc, and resumes the thread here. The loads
// below depend on those four fields being consecutive, in this order, in
// jl_tls_states_t (src/julia_threads.h).
// signals-unix.c falls back to a C definition where this file defines none.

#include "../common.h"

#if (defined(_OS_LINUX_) || defined(_OS_FREEBSD_) || defined(_OS_OPENBSD_)) && defined(_CPU_X86_64_)

    .text
    .globl CNAME(jl_fake_signal_return)
    HIDDEN(jl_fake_signal_return)
    DEBUGINFO(jl_fake_signal_return)
    .p2align 4                  // as the start of the .text of the C file it was in
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    // rdi points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg).
    // The CFA is the interrupted sp throughout (so that no unwinder needs to
    // follow a rule for sp). Until the frame below is built, through rdi:
    // CFA = [rdi + 8] (DW_CFA_def_cfa_expression: DW_OP_breg5 8, DW_OP_deref)
    // rip at [rdi] (DW_CFA_expression: DW_OP_breg5 0)
    .cfi_escape 0x0f, 0x03, 0x75, 0x08, 0x06
    .cfi_escape 0x10, 0x10, 0x02, 0x75, 0x00
    pushq   (%rdi)              // push pc (signal_ctx_pc), as the return address
    pushq   %rbp                // frame record {rbp, pc}
    // rbp at [rsp] (DW_CFA_expression: DW_OP_breg7 0)
    .cfi_escape 0x10, 0x06, 0x02, 0x77, 0x00
    movq    %rsp, %rbp
    pushq   8(%rdi)             // push sp (signal_ctx_sp)
    // From here, through the frame:
    // CFA = [rbp - 8] (DW_CFA_def_cfa_expression: DW_OP_breg6 -8, DW_OP_deref)
    // rip at [rbp + 8] and rbp at [rbp] (DW_CFA_expression: DW_OP_breg6 8 / 0)
    .cfi_escape 0x0f, 0x03, 0x76, 0x78, 0x06
    .cfi_escape 0x10, 0x10, 0x02, 0x76, 0x08
    .cfi_escape 0x10, 0x06, 0x02, 0x76, 0x00
    pushq   16(%rdi)            // push fptr (signal_ctx_fptr), leaving the stack aligned
    movq    24(%rdi), %rdi      // restore original rdi from signal_ctx_arg
    cld                         // the interrupted code may have set the direction flag
    callq   *(%rsp)             // call fptr
    ud2                         // unreachable
    .cfi_endproc
    EXPORT(jl_fake_signal_return)

#endif
