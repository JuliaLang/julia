// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "common.h"
#include "../../src/jl_exported_funcs.inc"

// LoongArch does not require the SEP macro; it natively supports backslash line continuation.
// Critical Fix: The relocation symbol for ld.d must be identical to the one used for pcaddu12i.
// Both must use CNAMEADDR(name) (the _addr symbol), NOT the original function name.

#define XX(name) \
    .global CNAME(name); \
    .type CNAME(name), @function; \
    .cfi_startproc; \
    .p2align 4; \
CNAME(name): \
    /* 1. Load the high 20 bits of the _addr symbol (PC-relative) */ \
    pcalau12i $t0, %pc_hi20(CNAMEADDR(name)); \
    /* 2. Load the low 12 bits of the _addr symbol and dereference to get the function pointer */ \
    /* IMPORTANT: Must use CNAMEADDR(name) here to match the high bits calculation above. */ \
    /* Using CNAME(name) directly would cause incorrect address calculation on LoongArch. */ \
    ld.d      $t1, $t0, %pc_lo12(CNAMEADDR(name)); \
    /* 3. Indirect jump to the target function */ \
    jr        $t1; \
    .cfi_endproc; \
    .size CNAME(name), . - CNAME(name);

JL_RUNTIME_EXPORTED_FUNCS(XX)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
