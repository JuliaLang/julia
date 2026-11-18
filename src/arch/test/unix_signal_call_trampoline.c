// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_fake_signal_return (src/arch/<arch>/unix_signal_call_trampoline.S), used
// the way jl_call_in_ctx (src/signals-unix.c) uses it: a signal handler saves
// the interrupted pc and sp and resumes the thread in the trampoline, which
// must call the function on the interrupted stack (below the interrupted
// stack pointer), with the stack aligned as at a call, and with unwind info
// and frame records that lead back into the interrupted code.

#define _GNU_SOURCE
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <ucontext.h>
#include "archtest.h"
#include "archstep.h"

#if defined(__linux__) && (defined(__x86_64__) || defined(__i386__) || defined(__aarch64__))

extern void jl_fake_signal_return(void);

#if defined(__i386__)
#define ARG_ABI __attribute__((regparm(1)))
#else
#define ARG_ABI
#endif
#define ARG_SENTINEL ((uintptr_t)0x12345678)

// Same layout as signal_ctx_pc .. signal_ctx_arg in jl_tls_states_t
static struct {
    uintptr_t pc;
    uintptr_t sp;
    void (ARG_ABI *fptr)(uintptr_t);
    uintptr_t arg;
} signal_ctx;

static jmp_buf back;
static backtrace_t bt, fp_bt;
static volatile int ran;
static volatile uintptr_t frame_in_fptr;
static volatile int aligned_in_fptr;
static volatile uintptr_t arg_in_fptr;
static volatile int df_in_fptr = -1;
static uintptr_t trampoline_ip, caller_ip;

// The frames above the interrupted code, as the signal handler unwinds them
// (by libgcc, and by libunwind)
static backtrace_t interrupted_bt, interrupted_lu;

static __attribute__((noinline, noreturn)) void ARG_ABI call_target(uintptr_t arg)
{
    arg_in_fptr = arg;
#if defined(__x86_64__) || defined(__i386__)
    // the C ABI requires the direction flag to be clear at a call
    uintptr_t flags;
    __asm__ volatile ("pushf\n\tpop %0" : "=r"(flags));
    df_in_fptr = (flags >> 10) & 1;
#endif
    trampoline_ip = RETURN_IP();
    frame_in_fptr = (uintptr_t)__builtin_frame_address(0);
    aligned_in_fptr = STACK_ALIGNED_AT_ENTRY();
    get_backtrace(&bt);
#ifdef HAVE_FP_BACKTRACE
    get_fp_backtrace(&fp_bt);
#endif
    ran = 1;
    longjmp(back, 1);
}

// Fill the stack below the caller with garbage, so that nothing can depend on
// what happens to be there.
static __attribute__((noinline)) void poison_stack_below(void)
{
    volatile char garbage[16384];
    memset((char*)garbage, 0x5a, sizeof(garbage));
}

static void handler(int sig, siginfo_t *info, void *_ctx)
{
    (void)sig; (void)info;
    ucontext_t *ctx = (ucontext_t*)_ctx;
#if defined(__x86_64__)
    uintptr_t sp = ctx->uc_mcontext.gregs[REG_RSP];
    signal_ctx.pc = ctx->uc_mcontext.gregs[REG_RIP];
#elif defined(__i386__)
    uintptr_t sp = ctx->uc_mcontext.gregs[REG_ESP];
    signal_ctx.pc = ctx->uc_mcontext.gregs[REG_EIP];
#elif defined(__aarch64__)
    uintptr_t sp = ctx->uc_mcontext.sp;
    signal_ctx.pc = ctx->uc_mcontext.pc;
#endif
    signal_ctx.sp = sp;
    get_backtrace(&interrupted_bt);
#ifdef HAVE_STEP_LIBUNWIND
    step_libunwind(ctx, &interrupted_lu);
#endif
    signal_ctx.arg = ARG_SENTINEL;
    signal_ctx.fptr = call_target;
    sp = (sp - 256) & ~(uintptr_t)15; // redzone and re-alignment
#if defined(__x86_64__)
    ctx->uc_mcontext.gregs[REG_RSP] = sp;
    ctx->uc_mcontext.gregs[REG_RDI] = (uintptr_t)&signal_ctx;
    ctx->uc_mcontext.gregs[REG_RIP] = (uintptr_t)&jl_fake_signal_return;
#elif defined(__i386__)
    ctx->uc_mcontext.gregs[REG_ESP] = sp;
    ctx->uc_mcontext.gregs[REG_EAX] = (uintptr_t)&signal_ctx;
    ctx->uc_mcontext.gregs[REG_EIP] = (uintptr_t)&jl_fake_signal_return;
#elif defined(__aarch64__)
    ctx->uc_mcontext.sp = sp;
    ctx->uc_mcontext.regs[0] = (uintptr_t)&signal_ctx;
    ctx->uc_mcontext.pc = (uintptr_t)&jl_fake_signal_return;
#endif
}


static __attribute__((noinline)) void interrupted(void)
{
    caller_ip = RETURN_IP();
    poison_stack_below();
    raise(SIGUSR1);
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
}

// A function without a frame pointer, whose unwind info locates its caller
// only by the stack pointer, interrupted by a fault (reading `p`) after it
// has moved it: unwinding from the trampoline only finds its caller if the
// trampoline's unwind info restores the interrupted stack pointer.
extern void sp_leaf(void *p);
#define ASM_STR_(x) #x
#define ASM_STR(x) ASM_STR_(x)
#define ASM_SYM(x) ASM_STR(__USER_LABEL_PREFIX__) #x
#if defined(__x86_64__)
__asm__(".text\n.globl " ASM_SYM(sp_leaf) "\n.type " ASM_SYM(sp_leaf) ",@function\n" ASM_SYM(sp_leaf) ":\n"
        "  .cfi_startproc\n"
        "  subq $40, %rsp\n  .cfi_adjust_cfa_offset 40\n"
        "  movb (%rdi), %al\n"
        "  addq $40, %rsp\n  .cfi_adjust_cfa_offset -40\n"
        "  ret\n  .cfi_endproc\n.size " ASM_SYM(sp_leaf) ", .-" ASM_SYM(sp_leaf) "\n");
#elif defined(__i386__)
__asm__(".text\n.globl " ASM_SYM(sp_leaf) "\n.type " ASM_SYM(sp_leaf) ",@function\n" ASM_SYM(sp_leaf) ":\n"
        "  .cfi_startproc\n"
        "  movl 4(%esp), %eax\n"
        "  subl $40, %esp\n  .cfi_adjust_cfa_offset 40\n"
        "  movb (%eax), %al\n"
        "  addl $40, %esp\n  .cfi_adjust_cfa_offset -40\n"
        "  ret\n  .cfi_endproc\n.size " ASM_SYM(sp_leaf) ", .-" ASM_SYM(sp_leaf) "\n");
#elif defined(__aarch64__)
__asm__(".text\n.p2align 2\n.globl " ASM_SYM(sp_leaf) "\n.type " ASM_SYM(sp_leaf) ",%function\n" ASM_SYM(sp_leaf) ":\n"
        "  .cfi_startproc\n"
        "  sub sp, sp, #48\n  .cfi_def_cfa_offset 48\n"
        "  ldrb w1, [x0]\n"
        "  add sp, sp, #48\n  .cfi_def_cfa_offset 0\n"
        "  ret\n  .cfi_endproc\n.size " ASM_SYM(sp_leaf) ", .-" ASM_SYM(sp_leaf) "\n");
#endif

static void *noaccess_page;
static __attribute__((noinline)) void interrupted_leaf(void)
{
    caller_ip = RETURN_IP();
    poison_stack_below();
    sp_leaf(noaccess_page);
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
}

#ifdef HAVE_ARCHSTEP
// Unwinding from every instruction of the trampoline, and of the function it
// calls, must lead back into the interrupted code and its callers, whether
// that code keeps a frame pointer or only has unwind info.
static step_fn_t step_fns_[2];
static int step_interrupted_here;
static step_chain_t step_expected[2];
static int step_bad;

static void step_run(void)
{
    if (!setjmp(back))
        interrupted();
}

static void step_run_leaf(void)
{
    if (!setjmp(back))
        interrupted_leaf();
}

// From the trampoline: the interrupted pc and its callers, as the signal
// handler found them in this run (from the kernel's signal frame, so with
// their right stack pointers); from the function it calls: the trampoline,
// and then the same. For each unwinder, as it found them.
static step_chain_t step_expected_lu[2];
static void step_expect(void)
{
    memset(step_expected, 0, sizeof(step_expected));
    memset(step_expected_lu, 0, sizeof(step_expected_lu));
    step_chain_add_ip(&step_expected[1], trampoline_ip);
    step_chain_add_ip(&step_expected_lu[1], trampoline_ip);
    int i = backtrace_index(&interrupted_bt, signal_ctx.pc);
    for (int k = 0; k < 2; k++) {
        // the CFA below the interrupted code is the trampoline's here, but was
        // the signal frame's for the handler; its stack pointer is that saved
        step_chain_add_frame(&step_expected[k], signal_ctx.pc, signal_ctx.pc + 1, 0, 0);
        if (i >= 0)
            step_chain_add_found(&step_expected[k], &interrupted_bt, i + 1);
        step_chain_add_frame(&step_expected_lu[k], signal_ctx.pc, signal_ctx.pc + 1, 0, signal_ctx.sp);
        step_chain_add_found(&step_expected_lu[k], &interrupted_lu, 0);
    }
}

static void step_sample_trampoline(ucontext_t *uc, step_fn_t *fn)
{
    step_expect();
    const step_chain_t *expected = &step_expected[fn - step_fns_];
    const step_chain_t *expected_lu = &step_expected_lu[fn - step_fns_];
    int ok = expected->n > 2;
    backtrace_t dw, lu, fp;
    dw.n = lu.n = fp.n = 0;
    dw.ended = lu.ended = 1;
#if defined(__aarch64__)
    // The unwind info can only give the interrupted pc as the return address
    // in lr, which loses the interrupted lr: a function interrupted before it
    // has saved it cannot be unwound past.
    int check_unwind_info = !step_interrupted_here;
#else
    int check_unwind_info = 1;
#endif
#ifdef HAVE_STEP_UNWIND
    step_unwind(uc, &dw);
    if (check_unwind_info)
        ok &= step_chain_ok(&dw, expected, expected->n);
#endif
#ifdef HAVE_STEP_LIBUNWIND
    step_libunwind(uc, &lu);
    if (check_unwind_info)
        ok &= expected_lu->n > 2 && step_chain_ok(&lu, expected_lu, expected_lu->n);
#endif
#ifdef HAVE_FP_BACKTRACE
    // The frame records lead back past the interrupted code, at least when it
    // is this test's own (the C library need not keep frame records).
    step_fp_unwind(uc, &fp);
    ok &= step_fp_chain_ok(&fp, expected);
    if (step_interrupted_here)
        ok &= backtrace_index(&fp, caller_ip) >= 0;
#endif
    if (!ok && step_bad++ < 8) {
        fprintf(stderr, "  unwinding from %s+%#lx:\n", fn->name, (unsigned long)(UC_PC(uc) - fn->lo));
        step_print_chain("unwind info (libgcc)", &dw, 1);
        step_print_chain("unwind info (libunwind)", &lu, 1);
        step_print_chain("frame pointers", &fp, 0);
        step_print_expected(expected);
        step_print_expected(expected_lu);
    }
}

static void step_trampoline(void)
{
    step_print_unwinders("unix_signal_call_trampoline");
    step_set_stacks(0, 0);
    if (!step_find_function(&step_fns_[0], "jl_fake_signal_return") ||
        !step_find_function(&step_fns_[1], "call_target")) {
        archtest_failures++;
        return;
    }
    void (*runs[2])(void) = {step_run, step_run_leaf};
    const char *how[2] = {"in a system call", "in a function without a frame pointer"};
    for (int r = 0; r < 2; r++) {
        step_interrupted_here = runs[r] == step_run_leaf;
        step_bad = 0;
        step_fns_[0].samples = step_fns_[1].samples = 0;
        step_fns_[0].entry_sampled = step_fns_[1].entry_sampled = 0;
        step_functions(step_fns_, 2, runs[r], step_sample_trampoline);
        CHECK(step_fns_[0].samples > 0 && step_fns_[1].samples > 0);
        CHECK(step_bad == 0);
        if (step_bad)
            fprintf(stderr, "  (%d of %d instructions sampled, interrupted %s)\n", step_bad,
                    step_fns_[0].samples + step_fns_[1].samples, how[r]);
    }
}
#endif

int main(void)
{
    // The signal frames go on another stack, leaving the poisoned one intact.
    stack_t alt;
    alt.ss_sp = malloc(65536);
    alt.ss_size = 65536;
    alt.ss_flags = 0;
    CHECK(alt.ss_sp != NULL && sigaltstack(&alt, NULL) == 0);
    struct sigaction act;
    memset(&act, 0, sizeof(act));
    act.sa_sigaction = handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigemptyset(&act.sa_mask);
    CHECK(sigaction(SIGUSR1, &act, NULL) == 0);
    if (!setjmp(back))
        interrupted();
    CHECK(ran);
    // on the interrupted stack, below its red zone (128 bytes on x86-64)
    CHECK(frame_in_fptr < signal_ctx.sp - 128 && frame_in_fptr > signal_ctx.sp - 65536);
    CHECK(aligned_in_fptr);
    CHECK(arg_in_fptr == ARG_SENTINEL);
    // call_target is called from the trampoline, whose unwind info leads to the
    // interrupted pc and from there on through interrupted() to main
    int trampoline = backtrace_index(&bt, trampoline_ip);
    CHECK(trampoline >= 0);
    CHECK(trampoline + 1 < bt.n && bt.ips[trampoline + 1] == signal_ctx.pc);
    CHECK(backtrace_index(&bt, caller_ip) > trampoline);
#ifdef HAVE_FP_BACKTRACE
    // the same along the frame records, to the interrupted pc (the interrupted
    // code itself, in the C library, need not keep frame records)
    int fp_trampoline = backtrace_index(&fp_bt, trampoline_ip);
    CHECK(fp_trampoline >= 0);
    CHECK(fp_trampoline + 1 < fp_bt.n && fp_bt.ips[fp_trampoline + 1] == signal_ctx.pc);
#endif

    noaccess_page = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    CHECK(noaccess_page != MAP_FAILED);
    CHECK(sigaction(SIGSEGV, &act, NULL) == 0);
#ifdef HAVE_ARCHSTEP
    step_trampoline();
#endif

#if defined(__x86_64__) || defined(__i386__)
    // Code may be interrupted with the direction flag set (in a backwards
    // string operation); the function is still called as C code.
    ran = 0;
    if (!setjmp(back))
        __asm__ volatile ("std\n\tmovb (%0), %%al" :: "r"(noaccess_page) : "eax", "memory");
    __asm__ volatile ("cld" ::: "memory");
    CHECK(ran);
    CHECK(df_in_fptr == 0);
#endif
    ARCHTEST_DONE("unix_signal_call_trampoline");
}

#else

int main(void)
{
    printf("unix_signal_call_trampoline: skipped on this platform\n");
    return 0;
}

#endif
