// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_start_fiber_on_stack (src/arch/<arch>/start_fiber.S): it must run
// the function on the new stack, with the stack aligned as at a call, and end
// the call chain there, both for unwinding with the unwind info and for
// walking frame pointers, whatever the caller left in its registers, and at
// every instruction until the function has set up its frame.

#define _GNU_SOURCE
#include <setjmp.h>
#include <string.h>
#include "archtest.h"
#include "archstep.h"
#include "../cfi_noreturn.h"

extern __attribute__((noreturn)) void jl_start_fiber_on_stack(char *stack_top, void (*fn)(void));

// Calls jl_start_fiber_on_stack as jl_start_fiber_set does in src/task.c,
// with the same unwind info.
__attribute__((noinline, noreturn, used)) void start_fiber(char *stack_top, void (*fn)(void))
{
CFI_NORETURN
    jl_start_fiber_on_stack(stack_top, fn);
}

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
#if (defined(__thumb__) && defined(__GNUC__) && !defined(__clang__)) || defined(ARCHTEST_NO_FP)
    // GCC's Thumb frame pointer points at the locals, not at a link to the
    // caller's frame, so there is no chain to walk; nor without frame pointers.
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

#ifdef HAVE_ARCHSTEP
// Unwinding from every instruction from start_fiber (as jl_start_fiber_set)
// through jl_start_fiber_on_stack to the first frame of the new task, as a
// profiler's signal could interrupt them: until the switch, it must find
// start_fiber's callers, or none once the unwind info ends the call chain; in
// the task's first function, none. (GCC does not save the return address in a
// function that does not return, which leaves start_fiber's frame record for a
// frame-pointer walk incomplete on ARM, RISC-V and PowerPC.)
#if defined(__arm__) || defined(__riscv) || defined(__powerpc64__)
#define START_FIBER_FP_CHAIN 0
#else
#define START_FIBER_FP_CHAIN 1
#endif

// Like start_task, the first function of the task
static __attribute__((noinline, noreturn, used)) void task_start(void)
{
CFI_NORETURN
    longjmp(back, 1);
}

// start_fiber, jl_start_fiber_on_stack, task_start
static step_fn_t step_fns_[3];
// start_fiber's callers, as each unwinder finds them, and none
static step_chain_t callers, callers_lu, no_callers;
static int step_bad;

// After the return address into step_run, which is that of this call and not
// of start_fiber's (and whose stack pointer may differ).
static void add_callers(step_chain_t *c, const backtrace_t *cap)
{
    int i = 0;
    while (i < cap->n && !step_chain_has(c, 0, cap->ips[i]))
        i++;
    step_chain_add_found(c, cap, i + 1);
}

static __attribute__((noinline)) void capture_callers(void)
{
    backtrace_t cap;
    get_backtrace(&cap);
    add_callers(&callers, &cap);
#ifdef HAVE_STEP_LIBUNWIND
    step_libunwind_here(&cap);
    add_callers(&callers_lu, &cap);
#endif
}

static __attribute__((noinline)) void step_run(void)
{
    if (callers.n == 1) {
        step_pause();
        capture_callers();
        step_resume();
    }
    memset(stack_lo, 0xa5, STACK_SIZE);
    if (!setjmp(back))
        start_fiber(stack_hi, task_start);
}

static void step_sample_fiber(ucontext_t *uc, step_fn_t *fn)
{
    int in_start = fn != &step_fns_[2];
    const step_chain_t *expected = in_start ? &callers : &no_callers;
    int ok = 1;
    backtrace_t dw, lu, fp;
    dw.n = lu.n = fp.n = 0;
    dw.ended = lu.ended = 1;
#ifdef HAVE_STEP_UNWIND
    step_unwind(uc, &dw);
    ok &= step_chain_ok(&dw, expected, 0);
#endif
#ifdef HAVE_STEP_LIBUNWIND
    step_libunwind(uc, &lu);
    ok &= step_chain_ok(&lu, in_start ? &callers_lu : &no_callers, 0);
#endif
#ifdef HAVE_FP_BACKTRACE
    if (!in_start || START_FIBER_FP_CHAIN) {
        step_fp_unwind(uc, &fp);
        ok &= step_fp_chain_ok(&fp, expected);
    }
#endif
    if (!ok && step_bad++ < 8) {
        fprintf(stderr, "  unwinding from %s+%#lx:\n", fn->name, (unsigned long)(UC_PC(uc) - fn->lo));
        step_print_chain("unwind info (libgcc)", &dw, 1);
        step_print_chain("unwind info (libunwind)", &lu, 1);
        step_print_chain("frame pointers", &fp, 0);
        step_print_expected(expected);
        if (in_start)
            step_print_expected(&callers_lu);
    }
}

static void step_fiber(void)
{
    step_print_unwinders("start_fiber");
    step_set_stacks((uintptr_t)stack_lo, (uintptr_t)stack_hi);
    step_fn_t run_fn;
    if (!step_find_function(&step_fns_[0], "start_fiber") ||
        !step_find_function(&step_fns_[1], "jl_start_fiber_on_stack") ||
        !step_find_function(&step_fns_[2], "task_start") ||
        !step_find_function(&run_fn, "step_run")) {
        archtest_failures++;
        return;
    }
    step_chain_add_return_into(&callers, &run_fn);
    step_chain_add_return_into(&callers_lu, &run_fn);
    step_functions(step_fns_, 3, step_run, step_sample_fiber);
    CHECK(step_fns_[0].samples > 0 && step_fns_[1].samples > 0 && step_fns_[2].samples > 0);
    CHECK(step_bad == 0);
    if (step_bad)
        fprintf(stderr, "  (%d of %d instructions sampled)\n", step_bad,
                step_fns_[0].samples + step_fns_[1].samples + step_fns_[2].samples);
}
#endif

int main(void)
{
    stack_lo = (char*)aligned_alloc(64, STACK_SIZE);
    if (stack_lo == NULL)
        return 1;
    stack_hi = stack_lo + STACK_SIZE;
    run_fiber(jl_start_fiber_on_stack, "from C");
    run_fiber(start_fiber_poisoned, "with garbage in the other registers");
#ifdef HAVE_ARCHSTEP
    step_fiber();
#endif
    free(stack_lo);
    ARCHTEST_DONE("start_fiber");
}
