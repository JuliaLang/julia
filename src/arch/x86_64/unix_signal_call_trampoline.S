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
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    // Mark as end of stack until frame is set up
    .cfi_undefined %rip
    .cfi_undefined %rsp
    // rdi points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg)
    pushq   (%rdi)              // push pc (signal_ctx_pc)
    pushq   8(%rdi)             // push sp (signal_ctx_sp)
    // stack layout: [sp, pc] (pc at higher address, like return address after call)
    .cfi_def_cfa %rsp, 8
    .cfi_offset %rip, 0         // previous %rip at CFA+0 (pc slot at rsp+8)
    .cfi_offset %rsp, -8        // previous %rsp at CFA-8 (sp slot at rsp+0)
    pushq   16(%rdi)            // push fptr (signal_ctx_fptr)
    .cfi_def_cfa %rsp, 16
    movq    24(%rdi), %rdi      // restore original rdi from signal_ctx_arg
    subq    $8, %rsp            // align stack to 16 bytes
    .cfi_def_cfa %rsp, 24
    callq   *8(%rsp)            // call fptr
    ud2                         // unreachable
    .cfi_endproc
    EXPORT(jl_fake_signal_return)

#endif
