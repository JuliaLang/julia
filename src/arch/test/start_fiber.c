// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_start_fiber_on_stack (src/arch/<arch>/start_fiber.S): it must run
// the function on the new stack, with the stack aligned as at a call, and end
// the call chain there, both for unwinding with the unwind info and for
// walking frame pointers, whatever the caller left in its registers.

#include <setjmp.h>
#include <string.h>
#include "archtest.h"

extern __attribute__((noreturn)) void jl_start_fiber_on_stack(char *stack_top, void (*fn)(void));

// Calls jl_start_fiber_on_stack with every register except the stack pointer,
// its arguments and the platform's reserved registers set to garbage, so that
// nothing the new fiber's call chain depends on can come from the caller.
extern __attribute__((noreturn)) void start_fiber_poisoned(char *stack_top, void (*fn)(void));
#define ASM_STR_(x) #x
#define ASM_STR(x) ASM_STR_(x)
#define ASM_SYM(x) ASM_STR(__USER_LABEL_PREFIX__) #x
#if defined(__x86_64__)
__asm__(".text\n.globl " ASM_SYM(start_fiber_poisoned) "\n" ASM_SYM(start_fiber_poisoned) ":\n"
        "  movabsq $0x5a5a5a5a5a5a5a5a, %rax\n"
        "  movq %rax, %rbx\n  movq %rax, %rcx\n  movq %rax, %rdx\n  movq %rax, %rbp\n"
        "  movq %rax, %r8\n  movq %rax, %r9\n  movq %rax, %r10\n  movq %rax, %r11\n"
        "  movq %rax, %r12\n  movq %rax, %r13\n  movq %rax, %r14\n  movq %rax, %r15\n"
        "  jmp " ASM_SYM(jl_start_fiber_on_stack) "\n");
#elif defined(__i386__)
// the arguments are on the stack, where this function's caller put them
__asm__(".text\n.globl " ASM_SYM(start_fiber_poisoned) "\n" ASM_SYM(start_fiber_poisoned) ":\n"
        "  movl $0x5a5a5a5a, %eax\n"
        "  movl %eax, %ebx\n  movl %eax, %ecx\n  movl %eax, %edx\n"
        "  movl %eax, %esi\n  movl %eax, %edi\n  movl %eax, %ebp\n"
        "  jmp " ASM_SYM(jl_start_fiber_on_stack) "\n");
#elif defined(__aarch64__)
// x18 is reserved by some platforms
__asm__(".text\n.p2align 2\n.globl " ASM_SYM(start_fiber_poisoned) "\n" ASM_SYM(start_fiber_poisoned) ":\n"
        "  mov x2, #0x5a5a\n"
        "  mov x3, x2\n  mov x4, x2\n  mov x5, x2\n  mov x6, x2\n  mov x7, x2\n  mov x8, x2\n"
        "  mov x9, x2\n  mov x10, x2\n  mov x11, x2\n  mov x12, x2\n  mov x13, x2\n  mov x14, x2\n"
        "  mov x15, x2\n  mov x16, x2\n  mov x17, x2\n  mov x19, x2\n  mov x20, x2\n  mov x21, x2\n"
        "  mov x22, x2\n  mov x23, x2\n  mov x24, x2\n  mov x25, x2\n  mov x26, x2\n  mov x27, x2\n"
        "  mov x28, x2\n  mov x29, x2\n  mov x30, x2\n"
        "  b " ASM_SYM(jl_start_fiber_on_stack) "\n");
#elif defined(__arm__)
// r9 is reserved by some platforms
__asm__(".text\n.syntax unified\n.arm\n.p2align 2\n.globl " ASM_SYM(start_fiber_poisoned) "\n" ASM_SYM(start_fiber_poisoned) ":\n"
        "  movw r2, #0x5a5a\n"
        "  mov r3, r2\n  mov r4, r2\n  mov r5, r2\n  mov r6, r2\n  mov r7, r2\n  mov r8, r2\n"
        "  mov r10, r2\n  mov r11, r2\n  mov r12, r2\n  mov lr, r2\n"
        "  b " ASM_SYM(jl_start_fiber_on_stack) "\n");
#elif defined(__riscv) && __riscv_xlen == 64
// gp and tp are reserved
__asm__(".text\n.p2align 2\n.globl " ASM_SYM(start_fiber_poisoned) "\n" ASM_SYM(start_fiber_poisoned) ":\n"
        "  li t0, 0x5a5a\n"
        "  mv t1, t0\n  mv t2, t0\n  mv t3, t0\n  mv t4, t0\n  mv t5, t0\n  mv t6, t0\n"
        "  mv a2, t0\n  mv a3, t0\n  mv a4, t0\n  mv a5, t0\n  mv a6, t0\n  mv a7, t0\n"
        "  mv s0, t0\n  mv s1, t0\n  mv s2, t0\n  mv s3, t0\n  mv s4, t0\n  mv s5, t0\n"
        "  mv s6, t0\n  mv s7, t0\n  mv s8, t0\n  mv s9, t0\n  mv s10, t0\n  mv s11, t0\n"
        "  mv ra, t0\n"
        "  tail " ASM_SYM(jl_start_fiber_on_stack) "\n");
#elif defined(__powerpc64__)
// r2 (TOC) and r13 (thread pointer) are reserved
__asm__(".text\n.p2align 2\n.globl " ASM_SYM(start_fiber_poisoned) "\n" ASM_SYM(start_fiber_poisoned) ":\n"
        "  li 0, 0x5a5a\n"
        "  mr 5, 0\n  mr 6, 0\n  mr 7, 0\n  mr 8, 0\n  mr 9, 0\n  mr 10, 0\n  mr 11, 0\n  mr 12, 0\n"
        "  mr 14, 0\n  mr 15, 0\n  mr 16, 0\n  mr 17, 0\n  mr 18, 0\n  mr 19, 0\n  mr 20, 0\n"
        "  mr 21, 0\n  mr 22, 0\n  mr 23, 0\n  mr 24, 0\n  mr 25, 0\n  mr 26, 0\n  mr 27, 0\n"
        "  mr 28, 0\n  mr 29, 0\n  mr 30, 0\n  mr 31, 0\n  mtlr 0\n"
        "  b " ASM_SYM(jl_start_fiber_on_stack) "\n");
#else
#error "start_fiber_poisoned is not implemented for this architecture"
#endif

#define STACK_SIZE (256 * 1024)

static char *stack_lo, *stack_hi;
static jmp_buf back;
static backtrace_t bt;
static volatile int ran;
static volatile uintptr_t sp_in_fiber;
static volatile int aligned_in_fiber;
static volatile int frame_chain_ends;

// Walk the frame-pointer chain from the caller: every frame must be on the
// fiber's stack, and the chain must end (in a null frame pointer).
static __attribute__((noinline)) int frame_chain_ends_on_stack(void)
{
    void **fp = (void**)__builtin_frame_address(0);
    for (int i = 0; i < 16; i++) {
        if (fp == NULL)
            return 1;
#if defined(__riscv)
        // RISC-V's frame pointer is the CFA, with the link two words below it.
        uintptr_t link = (uintptr_t)fp - 2 * sizeof(void*);
#elif defined(__arm__) && defined(__GNUC__) && !defined(__clang__) && !defined(__thumb__)
        // GCC's ARM frame pointer points at the saved LR.
        uintptr_t link = (uintptr_t)fp - sizeof(void*);
#else
        uintptr_t link = (uintptr_t)fp;
#endif
        if (link < (uintptr_t)stack_lo || link > (uintptr_t)stack_hi - sizeof(void*) ||
            link % sizeof(void*) != 0)
            return 0;
        fp = *(void***)link;
    }
    return 0;
}

static __attribute__((noinline, noreturn)) void fiber(void)
{
    volatile char local = 0;
    sp_in_fiber = (uintptr_t)&local;
    aligned_in_fiber = STACK_ALIGNED_AT_ENTRY();
#if defined(__thumb__) && defined(__GNUC__) && !defined(__clang__)
    // GCC's Thumb frame pointer points at the locals, not at a link to the
    // caller's frame, so there is no chain to walk.
    frame_chain_ends = 1;
#else
    frame_chain_ends = frame_chain_ends_on_stack();
#endif
    get_backtrace(&bt);
    ran = 1;
    longjmp(back, 1);
}

static void run_fiber(void (*start)(char*, void (*)(void)), const char *how)
{
    ran = 0; sp_in_fiber = 0; aligned_in_fiber = 0; frame_chain_ends = 0;
    memset(&bt, 0, sizeof(bt));
    // An incorrect frame-chain walk must not succeed by reading fresh zero pages.
    memset(stack_lo, 0xa5, STACK_SIZE);
    if (!setjmp(back))
        start(stack_hi, fiber);
    int failures = archtest_failures;
    CHECK(ran);
    CHECK(sp_in_fiber > (uintptr_t)stack_lo && sp_in_fiber < (uintptr_t)stack_hi);
    CHECK(aligned_in_fiber);
    CHECK(frame_chain_ends);
    // The fiber is the last non-null frame; some unwinders append a null IP.
    int last = backtrace_index(&bt, bt.caller_ip);
    CHECK(last >= 0);
    CHECK(bt.n == last + 1 || (bt.n == last + 2 && bt.ips[last + 1] == 0));
    if (archtest_failures != failures)
        fprintf(stderr, "  (started %s)\n", how);
}

int main(void)
{
    stack_lo = (char*)aligned_alloc(64, STACK_SIZE);
    if (stack_lo == NULL)
        return 1;
    stack_hi = stack_lo + STACK_SIZE;
    run_fiber(jl_start_fiber_on_stack, "from C");
    run_fiber(start_fiber_poisoned, "with garbage in the other registers");
    free(stack_lo);
    ARCHTEST_DONE("start_fiber");
}
