// This file is a part of Julia. License is MIT: https://julialang.org/license

// Exercise both macOS signal stubs with the stack layouts from signals-mach.c.
// POSIX signal contexts let this standalone test enter the synthetic unwind
// frame, and catch the restore trigger's fault and resume the interrupted code,
// without installing Julia's Mach exception server. Unwinding, by the unwind
// info and by frame pointers, from the function called and from the restore
// trigger must lead back into the interrupted code and its callers, whether
// that code keeps a frame pointer or not.

#define _DARWIN_C_SOURCE
#include <dlfcn.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/ucontext.h>
#include <unistd.h>
#include "archtest.h"

#if defined(__APPLE__) && (defined(__x86_64__) || defined(__aarch64__))

#ifdef ARCHTEST_LIBUNWIND
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#endif

extern void jl_fake_signal_return(void);
extern void jl_mach_restore_trigger(void);
void *jl_mach_restore_page;
// The test's own faults: one interrupts the code (as a signal for Julia
// would), the other samples the function called (as a profiler could)
static void *interrupt_page, *probe_page;

#if defined(__x86_64__)
#define STATE_SP(s) ((s).__rsp)
#define STATE_PC(s) ((s).__rip)
#define STATE_ARG(s) ((s).__rdi)
// jl_noreturn_call_in_state and jl_call_in_state return past the first
// instruction of jl_fake_signal_return and jl_mach_restore_trigger
#define FAKE_RETURN_IP ((uintptr_t)jl_fake_signal_return + 1)
#define TRIGGER_RETURN_IP ((uintptr_t)jl_mach_restore_trigger + 1)
// a load of known length, to resume past it
#define LOAD_LEN 2
#define LOAD(p) __asm__ volatile ("movb (%%rdi), %%al" :: "D"(p) : "rax", "memory")
#else
#define STATE_SP(s) ((s).__sp)
#define STATE_PC(s) ((s).__pc)
#define STATE_ARG(s) ((s).__x[0])
#define FAKE_RETURN_IP ((uintptr_t)jl_fake_signal_return + 4)
#define TRIGGER_RETURN_IP ((uintptr_t)jl_mach_restore_trigger + 4)
#define LOAD_LEN 4
#define LOAD(p) __asm__ volatile ("ldrb w9, [%0]" :: "r"(p) : "x9", "memory")
#endif

// A function without a frame pointer that has moved its stack pointer (and
// saved its return address there), interrupted by a load from `p`: only the
// right stack pointer for it finds its caller. LLVM's libunwind looks up the
// unwind info of the interrupted code (as of any frame above another) at its
// pc less one, which is the unwind info of the instruction before it: the load
// does not follow a change to the unwind info directly, so that this finds
// the same.
extern void noframe_load(void *p);
#if defined(__x86_64__)
__asm__(".text\n.globl _noframe_load\n.p2align 4\n_noframe_load:\n"
        "  .cfi_startproc\n"
        "  subq $40, %rsp\n  .cfi_adjust_cfa_offset 40\n"
        "  nop\n"
        "  movb (%rdi), %al\n"
        "  addq $40, %rsp\n  .cfi_adjust_cfa_offset -40\n"
        "  ret\n  .cfi_endproc\n");
#else
__asm__(".text\n.globl _noframe_load\n.p2align 2\n_noframe_load:\n"
        "  .cfi_startproc\n"
        "  sub sp, sp, #32\n  .cfi_def_cfa_offset 32\n"
        "  str x30, [sp, #16]\n  .cfi_offset x30, -16\n"
        "  nop\n"
        "  ldrb w9, [x0]\n"
        "  ldr x30, [sp, #16]\n  .cfi_restore x30\n"
        "  add sp, sp, #32\n  .cfi_def_cfa_offset 0\n"
        "  ret\n  .cfi_endproc\n");
#endif

static sigjmp_buf back;
static volatile sig_atomic_t restoring, ran, faults;
static uintptr_t interrupted_pc, interrupted_sp, resumed_pc, caller_ip, target_return_ip;
static uintptr_t saved_sp, fault_sp, fault_pc, arg_in_target;
static int aligned_in_target;
static char signal_stack[64 * 1024] __attribute__((aligned(16)));
// From the function called (by the unwind info and by frame pointers, and
// interrupted), and from the restore trigger's fault
static backtrace_t target_bt, target_fp_bt, probe_bt, fault_bt;

#ifdef ARCHTEST_LIBUNWIND
// As Julia's profiler unwinds a thread it has interrupted (rec_backtrace_ctx
// in src/stackwalk.c): from its thread state, frame by frame. The frames after
// the interrupted one, with their stack pointers.
static void unwind_state(ucontext_t *ctx, backtrace_t *b)
{
    unw_cursor_t cursor;
    b->n = 0;
    b->ended = 0;
    if (unw_init_local(&cursor, (unw_context_t*)&ctx->uc_mcontext->__ss) != 0)
        return;
    int r;
    while ((r = unw_step(&cursor)) > 0 && b->n < (int)(sizeof(b->ips) / sizeof(b->ips[0]))) {
        unw_word_t ip, sp;
        unw_get_reg(&cursor, UNW_REG_IP, &ip);
        unw_get_reg(&cursor, UNW_REG_SP, &sp);
        b->ips[b->n] = ip;
        b->cfas[b->n] = 0;
        b->sps[b->n++] = sp;
    }
    b->ended = r == 0;
}
#endif

static __attribute__((noinline)) void call_target(uintptr_t arg)
{
    arg_in_target = arg;
    aligned_in_target = STACK_ALIGNED_AT_ENTRY();
    target_return_ip = RETURN_IP();
    ran++;
    LOAD(probe_page);
    get_backtrace(&target_bt);
#ifdef HAVE_FP_BACKTRACE
    get_fp_backtrace(&target_fp_bt);
#endif
    if (!restoring)
        siglongjmp(back, 1);
}

static void handler(int sig, siginfo_t *info, void *context)
{
    (void)sig;
    ucontext_t *ctx = (ucontext_t*)context;
    __typeof__(ctx->uc_mcontext->__ss) *state = &ctx->uc_mcontext->__ss;
    if (info->si_addr == probe_page) {
#ifdef ARCHTEST_LIBUNWIND
        unwind_state(ctx, &probe_bt);
#endif
        STATE_PC(*state) += LOAD_LEN;
        return;
    }
    if (info->si_addr == jl_mach_restore_page) {
        if (!restoring)
            _Exit(2);
        fault_sp = STATE_SP(*state);
        if (fault_sp != saved_sp)
            _Exit(3);
        faults++;
        fault_pc = STATE_PC(*state);
#ifdef ARCHTEST_LIBUNWIND
        unwind_state(ctx, &fault_bt);
#endif
        // the whole context was saved after the thread state
        memcpy(ctx->uc_mcontext, (void*)(fault_sp + sizeof(*state)), sizeof(*ctx->uc_mcontext));
        return;
    }
    if (info->si_addr != interrupt_page)
        _Exit(4);

    interrupted_pc = STATE_PC(*state);
    interrupted_sp = STATE_SP(*state);
    // where the interrupted code resumes, once restored
    STATE_PC(*state) += LOAD_LEN;
    resumed_pc = STATE_PC(*state);
    uintptr_t sp = (interrupted_sp - 256) & ~(uintptr_t)15;
    uintptr_t return_ip;
    if (restoring) {
        // the thread state at sp, as jl_call_in_state saves it (here followed
        // by the rest of the context, to restore all of it)
        sp = (sp - sizeof(*state) - sizeof(*ctx->uc_mcontext)) & ~(uintptr_t)15;
        memcpy((void*)sp, state, sizeof(*state));
        memcpy((void*)(sp + sizeof(*state)), ctx->uc_mcontext, sizeof(*ctx->uc_mcontext));
        saved_sp = sp;
        return_ip = TRIGGER_RETURN_IP;
    }
    else {
        // as jl_noreturn_call_in_state pushes them
        sp -= 2 * sizeof(uintptr_t);
        ((uintptr_t*)sp)[0] = interrupted_pc;
        ((uintptr_t*)sp)[1] = interrupted_sp;
        return_ip = FAKE_RETURN_IP;
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

static __attribute__((noinline)) void not_inlined(void)
{
    __asm__ volatile ("" ::: "memory");
}

// Code that keeps a frame record (it calls a function, so it is not a leaf,
// whose return address an AArch64 unwinder could not find in lr past the
// trampolines)
static __attribute__((noinline)) void interrupted(void)
{
    caller_ip = RETURN_IP();
    not_inlined();
    LOAD(interrupt_page);
    __asm__ volatile ("" ::: "memory"); // keep the load out of tail position
}

static __attribute__((noinline)) void interrupted_noframe(void)
{
    caller_ip = RETURN_IP();
    noframe_load(interrupt_page);
    __asm__ volatile ("" ::: "memory");
}

// The frames found must include `ips` in order (the last of them with the
// interrupted stack pointer, if `check_sp`), go on to the interrupted code's
// caller, and (if `check_end`) be all the unwinder found before it ended the
// call chain rather than gave up.
static int chain_ok(const backtrace_t *b, const uintptr_t *ips, int n, int check_sp, int check_end)
{
    int i = backtrace_index(b, ips[0]);
    if (i < 0 || i + n > b->n)
        return 0;
    for (int k = 1; k < n; k++)
        if (b->ips[i + k] != ips[k])
            return 0;
    if (check_sp && b->sps[i + n - 1] != interrupted_sp)
        return 0;
    if (backtrace_index(b, caller_ip) < i + n)
        return 0;
    return !check_end || b->ended;
}

// For failures: the frames found, by address and symbol
static void print_frames(const char *what, const backtrace_t *b, int unwind_info)
{
    fprintf(stderr, "    %s%s:\n", what, unwind_info && !b->ended ? " (the unwinder gave up)" : "");
    for (int i = 0; i < b->n; i++) {
        Dl_info info;
        fprintf(stderr, "      %p", (void*)b->ips[i]);
        if (dladdr((void*)b->ips[i], &info) && info.dli_sname)
            fprintf(stderr, " %s+%#lx", info.dli_sname, (unsigned long)(b->ips[i] - (uintptr_t)info.dli_saddr));
        if (b->sps[i])
            fprintf(stderr, " sp=%p", (void*)b->sps[i]);
        fprintf(stderr, "\n");
    }
}

static void run_case(int restore, int noframe, const char *what)
{
    int failures = archtest_failures;
    ran = faults = 0;
    restoring = restore;
    memset(&target_bt, 0, sizeof(target_bt));
    memset(&target_fp_bt, 0, sizeof(target_fp_bt));
    memset(&probe_bt, 0, sizeof(probe_bt));
    memset(&fault_bt, 0, sizeof(fault_bt));
    if (!sigsetjmp(back, 1)) {
        if (noframe)
            interrupted_noframe();
        else
            interrupted();
    }
    uintptr_t return_ip = restore ? TRIGGER_RETURN_IP : FAKE_RETURN_IP;
    // the interrupted code's pc, as saved for unwinding
    uintptr_t frame_pc = restore ? resumed_pc : interrupted_pc;
    uintptr_t from_target[2] = {return_ip, frame_pc};
    CHECK(ran == 1);
    CHECK(arg_in_target == 0x12345678);
    CHECK(aligned_in_target);
    CHECK(target_return_ip == return_ip);
    if (restore) {
        CHECK(faults == 1);
        CHECK(fault_sp == saved_sp);
        CHECK(fault_pc > (uintptr_t)jl_mach_restore_trigger &&
              fault_pc < (uintptr_t)jl_mach_restore_trigger + 16);
    }
    // from the function called: its return address, then the interrupted code
    CHECK(chain_ok(&target_bt, from_target, 2, 0, 1));
#ifdef HAVE_FP_BACKTRACE
    CHECK(backtrace_index(&target_fp_bt, return_ip) >= 0 &&
          backtrace_index(&target_fp_bt, caller_ip) > backtrace_index(&target_fp_bt, return_ip));
#endif
#ifdef ARCHTEST_LIBUNWIND
    // the same, from the function called interrupted, and from the trigger's fault
    CHECK(chain_ok(&probe_bt, from_target, 2, 1, 1));
    if (restore)
        CHECK(chain_ok(&fault_bt, &frame_pc, 1, 1, 1));
#endif
    if (archtest_failures != failures) {
        fprintf(stderr, "  (%s) interrupted pc %p, sp %p, resumed at %p; its caller %p\n", what,
                (void*)interrupted_pc, (void*)interrupted_sp, (void*)resumed_pc, (void*)caller_ip);
        fprintf(stderr, "    call_target %p, jl_fake_signal_return %p, jl_mach_restore_trigger %p, fault pc %p\n",
                (void*)&call_target, (void*)&jl_fake_signal_return, (void*)&jl_mach_restore_trigger,
                (void*)fault_pc);
        print_frames("from call_target, by unwind info", &target_bt, 1);
        print_frames("from call_target, by frame pointers", &target_fp_bt, 0);
        print_frames("from call_target interrupted, by libunwind", &probe_bt, 1);
        if (restore)
            print_frames("from the trigger's fault, by libunwind", &fault_bt, 1);
    }
}

int main(void)
{
    size_t page_size = (size_t)sysconf(_SC_PAGESIZE);
    jl_mach_restore_page = mmap(NULL, page_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    interrupt_page = mmap(NULL, page_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    probe_page = mmap(NULL, page_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (jl_mach_restore_page == MAP_FAILED || interrupt_page == MAP_FAILED || probe_page == MAP_FAILED)
        return 1;
    stack_t alt = {.ss_sp = signal_stack, .ss_size = sizeof(signal_stack)};
    if (sigaltstack(&alt, NULL) != 0)
        return 1;
    struct sigaction act;
    memset(&act, 0, sizeof(act));
    act.sa_sigaction = handler;
    act.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigemptyset(&act.sa_mask);
    if (sigaction(SIGBUS, &act, NULL) != 0 || sigaction(SIGSEGV, &act, NULL) != 0)
        return 1;
    alarm(10);
#ifdef ARCHTEST_LIBUNWIND
    printf("macos_signal: unwinding checked with libunwind\n");
#endif
    run_case(0, 0, "jl_fake_signal_return");
    run_case(0, 1, "jl_fake_signal_return, without a frame pointer");
    run_case(1, 0, "jl_mach_restore_trigger");
    run_case(1, 1, "jl_mach_restore_trigger, without a frame pointer");
    alarm(0);
    munmap(jl_mach_restore_page, page_size);
    munmap(interrupt_page, page_size);
    munmap(probe_page, page_size);
    ARCHTEST_DONE("macos_signal");
}

#else

int main(void)
{
    printf("macos_signal: skipped on this platform\n");
    return 0;
}

#endif
