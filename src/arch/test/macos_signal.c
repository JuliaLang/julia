// This file is a part of Julia. License is MIT: https://julialang.org/license

// Exercise both macOS signal stubs with the stack layouts from signals-mach.c.
// POSIX signal contexts let this standalone test enter the synthetic unwind
// frame, and catch the restore trigger's fault and resume the interrupted code,
// without installing Julia's Mach exception server.

#define _DARWIN_C_SOURCE
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/ucontext.h>
#include <unistd.h>
#include "archtest.h"

#if defined(__APPLE__) && (defined(__x86_64__) || defined(__aarch64__))

extern void jl_fake_signal_return(void);
extern void jl_mach_restore_trigger(void);
void *jl_mach_restore_page;

#if defined(__x86_64__)
#define STATE_SP(s) ((s).__rsp)
#define STATE_PC(s) ((s).__rip)
#define STATE_ARG(s) ((s).__rdi)
#else
#define STATE_SP(s) ((s).__sp)
#define STATE_PC(s) ((s).__pc)
#define STATE_ARG(s) ((s).__x[0])
#endif

static sigjmp_buf back;
static backtrace_t bt;
static volatile sig_atomic_t restoring, ran, faults;
static uintptr_t interrupted_pc, interrupted_sp, caller_ip, target_return_ip;
static uintptr_t saved_sp, fault_sp, arg_in_target;
static int aligned_in_target;
static char signal_stack[64 * 1024] __attribute__((aligned(16)));

static __attribute__((noinline)) void call_target(uintptr_t arg)
{
    arg_in_target = arg;
    aligned_in_target = STACK_ALIGNED_AT_ENTRY();
    target_return_ip = RETURN_IP();
    ran++;
    if (!restoring) {
        get_backtrace(&bt);
        siglongjmp(back, 1);
    }
}

static void handler(int sig, siginfo_t *info, void *context)
{
    ucontext_t *ctx = (ucontext_t*)context;
    __typeof__(ctx->uc_mcontext->__ss) *state = &ctx->uc_mcontext->__ss;
    if (sig != SIGUSR1) {
        if (!restoring || info->si_addr != jl_mach_restore_page)
            _Exit(2);
        fault_sp = STATE_SP(*state);
        if (fault_sp != saved_sp)
            _Exit(3);
        faults++;
        memcpy(ctx->uc_mcontext, (void*)fault_sp, sizeof(*ctx->uc_mcontext));
        return;
    }

    interrupted_pc = STATE_PC(*state);
    interrupted_sp = STATE_SP(*state);
    uintptr_t sp = (interrupted_sp - 256) & ~(uintptr_t)15;
    uintptr_t return_ip;
    if (restoring) {
        sp = (sp - sizeof(*ctx->uc_mcontext)) & ~(uintptr_t)15;
        memcpy((void*)sp, ctx->uc_mcontext, sizeof(*ctx->uc_mcontext));
        saved_sp = sp;
        return_ip = (uintptr_t)jl_mach_restore_trigger;
    }
    else {
        sp -= 2 * sizeof(uintptr_t);
        ((uintptr_t*)sp)[0] = interrupted_pc;
        ((uintptr_t*)sp)[1] = interrupted_sp;
#if defined(__x86_64__)
        return_ip = (uintptr_t)jl_fake_signal_return + 1;
#else
        return_ip = (uintptr_t)jl_fake_signal_return + 4;
#endif
    }
#if defined(__x86_64__)
    sp -= sizeof(uintptr_t);
    *(uintptr_t*)sp = return_ip;
#else
    state->__lr = return_ip;
#endif
    STATE_SP(*state) = sp;
    STATE_PC(*state) = (uintptr_t)call_target;
    STATE_ARG(*state) = 0x12345678;
}

static __attribute__((noinline)) void interrupted(void)
{
    caller_ip = RETURN_IP();
    raise(SIGUSR1);
    __asm__ volatile ("" ::: "memory");
}

int main(void)
{
    size_t page_size = (size_t)sysconf(_SC_PAGESIZE);
    jl_mach_restore_page = mmap(NULL, page_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (jl_mach_restore_page == MAP_FAILED)
        return 1;
    stack_t alt = {.ss_sp = signal_stack, .ss_size = sizeof(signal_stack)};
    if (sigaltstack(&alt, NULL) != 0)
        return 1;
    struct sigaction act;
    memset(&act, 0, sizeof(act));
    act.sa_sigaction = handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigemptyset(&act.sa_mask);
    if (sigaction(SIGUSR1, &act, NULL) != 0 || sigaction(SIGBUS, &act, NULL) != 0 ||
        sigaction(SIGSEGV, &act, NULL) != 0)
        return 1;
    alarm(10);

    if (!sigsetjmp(back, 1))
        interrupted();
    CHECK(ran == 1);
    CHECK(arg_in_target == 0x12345678);
    CHECK(aligned_in_target);
    int trampoline = backtrace_index(&bt, target_return_ip);
    CHECK(trampoline >= 0);
    CHECK(trampoline + 1 < bt.n && bt.ips[trampoline + 1] == interrupted_pc);
    CHECK(backtrace_index(&bt, caller_ip) > trampoline);

    restoring = 1;
    interrupted();
    CHECK(ran == 2);
    CHECK(arg_in_target == 0x12345678);
    CHECK(aligned_in_target);
    CHECK(target_return_ip == (uintptr_t)jl_mach_restore_trigger);
    CHECK(faults == 1);
    CHECK(fault_sp == saved_sp);
    alarm(0);
    munmap(jl_mach_restore_page, page_size);
    ARCHTEST_DONE("macos_signal");
}

#else

int main(void)
{
    printf("macos_signal: skipped on this platform\n");
    return 0;
}

#endif
