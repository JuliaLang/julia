// This file is a part of Julia. License is MIT: https://julialang.org/license

// Minimal setjmp/longjmp implementation for x86_64 Linux
// These are designed to work with the preserve_none calling convention,
// where all registers are caller-saved. This means we only need to save
// the stack pointer - the compiler will spill all live registers before
// calling setjmp.
//
// IMPORTANT: These functions ONLY work correctly when called with the
// preserve_none calling convention. Without preserve_none, callee-saved
// registers won't be restored properly after longjmp.

#if defined(__x86_64__) && defined(__linux__)

// Mark stack as non-executable
.section .note.GNU-stack,"",@progbits

.text

// ============================================================================
// jl_minimal_setjmp / ijl_minimal_setjmp
// ============================================================================
.p2align 4,0x90
.globl ijl_minimal_setjmp
.globl jl_minimal_setjmp
.type ijl_minimal_setjmp,@function
.type jl_minimal_setjmp,@function
ijl_minimal_setjmp:
jl_minimal_setjmp:
    // N.B: In preserve_none ABI, r12 is the first argument.
    // r12 = pointer to buffer (three pointer-sized slots: rbp, rsp, rip)
    // Save RSP as it was *before* the call instruction pushed the return address.
    lea     8(%rsp), %rax       // rax = original RSP (before call)
    mov     %rbp, 0(%r12)        // save RBP to buffer[0]
    mov     %rax, 8(%r12)        // save RSP to buffer[1]
    mov     (%rsp), %rax        // rax = return address
    mov     %rax, 16(%r12)       // save return address to buffer[2]
    xor     %eax, %eax          // return 0 (setjmp returns 0 on first call)
    ret
.size ijl_minimal_setjmp, . - ijl_minimal_setjmp
.size jl_minimal_setjmp, . - jl_minimal_setjmp


// ============================================================================
// jl_minimal_longjmp / ijl_minimal_longjmp
// ============================================================================
.p2align 4,0x90
.globl ijl_minimal_longjmp
.globl jl_minimal_longjmp
.type ijl_minimal_longjmp,@function
.type jl_minimal_longjmp,@function
ijl_minimal_longjmp:
jl_minimal_longjmp:
    // rdi = pointer to buffer (two pointer-sized slots: rsp, rip)
    // esi = return value (passed to setjmp caller)
    mov     %esi, %eax          // set return value
    test    %eax, %eax          // longjmp must return non-zero
    jne     1f
    inc     %eax                // if val was 0, return 1
1:
    mov     0(%rdi), %rbp        // restore RBP
    mov     8(%rdi), %rsp        // restore RSP (to pre-call value)
    jmp     *16(%rdi)            // jump to saved return address
.size ijl_minimal_longjmp, . - ijl_minimal_longjmp
.size jl_minimal_longjmp, . - jl_minimal_longjmp

#endif
