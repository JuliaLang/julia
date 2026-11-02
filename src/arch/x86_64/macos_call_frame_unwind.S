// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_fake_signal_return (macOS): never executed. It exists only for its unwind
// info, which describes the frame that jl_noreturn_call_in_state builds when it
// makes an interrupted thread call a function that does not return, so that
// unwinding from that function continues into the interrupted code.
//
// Coupled to C code in src/signals-mach.c: jl_noreturn_call_in_state pushes
// the interrupted %rsp and %rip and then &jl_fake_signal_return + 1 as the
// return address. The CFA rules below locate the interrupted %rip and %rsp
// relative to the stack pointer at that return address.

#include "../common.h"

#if defined(_OS_DARWIN_) && defined(_CPU_X86_64_)

    .text
    .globl CNAME(jl_fake_signal_return)
    HIDDEN(jl_fake_signal_return)
    .p2align 4, 0x90
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    .cfi_def_cfa %rsp, 0        // CFA here uses %rsp directly
    .cfi_offset %rip, 0         // previous value of %rip at CFA
    .cfi_offset %rsp, 8         // previous value of %rsp at CFA
    ud2
    ud2
    .cfi_endproc

#endif
