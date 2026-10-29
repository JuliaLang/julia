// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_fake_signal_return (src/arch/<arch>/unix_signal_call_trampoline.S), used
// the way jl_call_in_ctx (src/signals-unix.c) uses it: a signal handler saves
// the interrupted pc and sp and resumes the thread in the trampoline, which
// must call the function on the interrupted stack (below the interrupted
// stack pointer), with the stack aligned as at a call, and with unwind info
// that leads back into the interrupted code once it has built the frame for
// it, and ends the call chain before.

#define _GNU_SOURCE
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <ucontext.h>
#include "archtest.h"

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

// For interrupting the trampoline before it has built a frame: it is resumed
// with its argument pointing at an inaccessible page, so that its first access
// faults, and the fault handler unwinds from there.
static void *noaccess_page;
static volatile int at_entry;
static sigjmp_buf back_from_fault;
static backtrace_t fault_bt;
static volatile uintptr_t fault_pc;

static void fault_handler(int sig, siginfo_t *info, void *_ctx)
{
    (void)sig; (void)info;
    ucontext_t *ctx = (ucontext_t*)_ctx;
#if defined(__x86_64__)
    fault_pc = ctx->uc_mcontext.gregs[REG_RIP];
#elif defined(__i386__)
    fault_pc = ctx->uc_mcontext.gregs[REG_EIP];
#elif defined(__aarch64__)
    fault_pc = ctx->uc_mcontext.pc;
#endif
    get_backtrace(&fault_bt);
    siglongjmp(back_from_fault, 1);
}

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
    signal_ctx.arg = ARG_SENTINEL;
    signal_ctx.fptr = call_target;
    sp = (sp - 256) & ~(uintptr_t)15; // redzone and re-alignment
#if defined(__x86_64__)
    ctx->uc_mcontext.gregs[REG_RSP] = sp;
    ctx->uc_mcontext.gregs[REG_RDI] = at_entry ? (uintptr_t)noaccess_page : (uintptr_t)&signal_ctx;
    ctx->uc_mcontext.gregs[REG_RIP] = (uintptr_t)&jl_fake_signal_return;
#elif defined(__i386__)
    ctx->uc_mcontext.gregs[REG_ESP] = sp;
    ctx->uc_mcontext.gregs[REG_EAX] = at_entry ? (uintptr_t)noaccess_page : (uintptr_t)&signal_ctx;
    ctx->uc_mcontext.gregs[REG_EIP] = (uintptr_t)&jl_fake_signal_return;
#elif defined(__aarch64__)
    ctx->uc_mcontext.sp = sp;
    ctx->uc_mcontext.regs[0] = at_entry ? (uintptr_t)noaccess_page : (uintptr_t)&signal_ctx;
    ctx->uc_mcontext.pc = (uintptr_t)&jl_fake_signal_return;
#endif
}

// Fill the stack below the caller with garbage, so that nothing can depend on
// what happens to be there.
static __attribute__((noinline)) void poison_stack_below(void)
{
    volatile char garbage[16384];
    memset((char*)garbage, 0x5a, sizeof(garbage));
}

static __attribute__((noinline)) void interrupted(void)
{
    caller_ip = RETURN_IP();
    poison_stack_below();
    raise(SIGUSR1);
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
}

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

    // Unwinding from the trampoline's first instruction ends there: the
    // frame its unwind info describes does not exist yet.
    noaccess_page = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    CHECK(noaccess_page != MAP_FAILED);
    act.sa_sigaction = fault_handler;
    CHECK(sigaction(SIGSEGV, &act, NULL) == 0);
    at_entry = 1;
    if (!sigsetjmp(back_from_fault, 1))
        interrupted();
    CHECK(fault_pc == (uintptr_t)&jl_fake_signal_return);
    int entry = backtrace_index(&fault_bt, fault_pc);
    CHECK(entry >= 0);
    CHECK(fault_bt.n == entry + 1 || (fault_bt.n == entry + 2 && fault_bt.ips[entry + 1] == 0));

#if defined(__x86_64__) || defined(__i386__)
    // Code may be interrupted with the direction flag set (in a backwards
    // string operation); the function is still called as C code.
    at_entry = 0;
    ran = 0;
    act.sa_sigaction = handler;
    CHECK(sigaction(SIGSEGV, &act, NULL) == 0);
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
