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
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    // Mark as end of stack until frame is set up
    .cfi_undefined %eip
    // eax points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg)
    pushl   (%eax)              // push pc (signal_ctx_pc), as the return address
    .cfi_def_cfa %esp, 4
    .cfi_offset %eip, -4
    pushl   %ebp                // frame record {ebp, pc}
    .cfi_def_cfa_offset 8
    .cfi_offset %ebp, -8
    movl    %esp, %ebp
    .cfi_def_cfa_register %ebp
    pushl   4(%eax)             // push sp (signal_ctx_sp)
    .cfi_offset %esp, -12
    pushl   8(%eax)             // push fptr (signal_ctx_fptr), leaving the stack aligned
    movl    12(%eax), %eax      // restore original eax from signal_ctx_arg
    cld                         // the interrupted code may have set the direction flag
    calll   *(%esp)             // call fptr
    ud2                         // unreachable
    .cfi_endproc
    EXPORT(jl_fake_signal_return)

#endif
