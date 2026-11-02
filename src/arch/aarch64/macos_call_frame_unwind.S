// This file is a part of Julia. License is MIT: https://julialang.org/license

// jl_fake_signal_return (macOS): never executed. It exists only for its unwind
// info, which describes the frame that jl_noreturn_call_in_state builds when it
// makes an interrupted thread call a function that does not return, so that
// unwinding from that function continues into the interrupted code.
//
// Coupled to C code in src/signals-mach.c: jl_noreturn_call_in_state pushes
// the interrupted pc and sp and sets the link register to
// &jl_fake_signal_return + 4. The CFA rules below locate the interrupted pc
// and sp relative to the stack pointer.

#include "../common.h"

#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)

    .text
    .globl CNAME(jl_fake_signal_return)
    HIDDEN(jl_fake_signal_return)
    .p2align 2
CNAME(jl_fake_signal_return):
    .cfi_startproc
    .cfi_signal_frame
    .cfi_def_cfa sp, 0          // use sp as fp here
    // This is not quite valid, since the AArch64 DWARF spec lacks the ability to define how to restore the LR register correctly,
    // so normally libunwind implementations on linux detect this function specially and hack around the invalid info:
    // https://github.com/llvm/llvm-project/commit/c82deed6764cbc63966374baf9721331901ca958
    .cfi_offset lr, 0
    .cfi_offset sp, 8
    brk     #1
    brk     #1
    .cfi_endproc

#endif
