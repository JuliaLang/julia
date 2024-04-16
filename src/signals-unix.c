// This file is a part of Julia. License is MIT: https://julialang.org/license

// Note that this file is `#include`d by "signal-handling.c"

#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <pthread.h>
#include <time.h>
#include <errno.h>
#if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif

#ifdef __APPLE__
#include <AvailabilityMacros.h>
#ifdef MAC_OS_X_VERSION_10_9
#include <sys/_types/_ucontext64.h>
#else
#define __need_ucontext64_t
#include <machine/_structs.h>
#endif
#endif

// Figure out the best signals/timers to use for this platform
#if defined(__APPLE__) // Darwin's mach ports allow signal-free thread management
#define HAVE_MACH
#define HAVE_KEVENT
#elif defined(__OpenBSD__)
#define HAVE_KEVENT
#else // generic Linux or FreeBSD
#define HAVE_TIMER
#endif

#ifdef HAVE_KEVENT
#include <sys/event.h>
#endif

// 8M signal stack, same as default stack size (though we barely use this)
static const size_t sig_stack_size = 8 * 1024 * 1024;

#include "julia_assert.h"

// helper function for returning the unw_context_t inside a ucontext_t
// (also used by stackwalk.c)
bt_context_t *jl_to_bt_context(void *sigctx)
{
#ifdef __APPLE__
    return (bt_context_t*)&((ucontext64_t*)sigctx)->uc_mcontext64->__ss;
#elif defined(_CPU_ARM_)
    // libunwind does not use `ucontext_t` on ARM.
    // `unw_context_t` is a struct of 16 `unsigned long` which should
    // have the same layout as the `arm_r0` to `arm_pc` fields in `sigcontext`
    ucontext_t *ctx = (ucontext_t*)sigctx;
    return (bt_context_t*)&ctx->uc_mcontext.arm_r0;
#else
    return (bt_context_t*)sigctx;
#endif
}

static int thread0_exit_count = 0;
static void jl_exit_thread0(int signo, jl_bt_element_t *bt_data, size_t bt_size);

static inline __attribute__((unused)) uintptr_t jl_get_rsp_from_ctx(const void *_ctx)
{
#if defined(_OS_LINUX_) && defined(_CPU_X86_64_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.gregs[REG_RSP];
#elif defined(_OS_LINUX_) && defined(_CPU_X86_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.gregs[REG_ESP];
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.sp;
#elif defined(_OS_LINUX_) && defined(_CPU_ARM_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.arm_sp;
#elif defined(_OS_DARWIN_) && defined(_CPU_X86_64_)
    const ucontext64_t *ctx = (const ucontext64_t*)_ctx;
    return ctx->uc_mcontext64->__ss.__rsp;
#elif defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
    const ucontext64_t *ctx = (const ucontext64_t*)_ctx;
    return ctx->uc_mcontext64->__ss.__sp;
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_64_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.mc_rsp;
#elif defined(_OS_OPENBSD_) && defined(_CPU_X86_64_)
    const struct sigcontext *ctx = (const struct sigcontext *)_ctx;
    return ctx->sc_rsp;
#else
    // TODO Add support for PowerPC(64)?
    return 0;
#endif
}

static int is_addr_on_sigstack(jl_ptls_t ptls, void *ptr)
{
    // One guard page for signal_stack.
    return ptls->signal_stack == NULL ||
           ((char*)ptr >= (char*)ptls->signal_stack - jl_page_size &&
            (char*)ptr <= (char*)ptls->signal_stack + (ptls->signal_stack_size ? ptls->signal_stack_size : sig_stack_size));
}

// Modify signal context `_ctx` so that `fptr` will execute when the signal
// returns. `fptr` will execute on the signal stack, and must not return.
// jl_call_in_ctx is also currently executing on that signal stack,
// so be careful not to smash it
JL_NO_ASAN static void jl_call_in_ctx(jl_ptls_t ptls, void (*fptr)(void), int sig, void *_ctx)
{
    // Modifying the ucontext should work but there is concern that
    // sigreturn oriented programming mitigation can work against us
    // by rejecting ucontext that is modified.
    // The current (staged) implementation in the Linux Kernel only
    // checks that the syscall is made in the signal handler and that
    // the ucontext address is valid. Hopefully the value of the ucontext
    // will not be part of the validation...
    if (!ptls) {
        sigset_t sset;
        sigemptyset(&sset);
        sigaddset(&sset, sig);
        pthread_sigmask(SIG_UNBLOCK, &sset, NULL);
        fptr();
        return;
    }
    uintptr_t rsp = jl_get_rsp_from_ctx(_ctx);
    if (is_addr_on_sigstack(ptls, (void*)rsp))
        rsp = (rsp - 256) & ~(uintptr_t)15; // redzone and re-alignment
    else
        rsp = (uintptr_t)ptls->signal_stack + (ptls->signal_stack_size ? ptls->signal_stack_size : sig_stack_size);
    assert(rsp % 16 == 0);
    rsp -= 16;
#if defined(_OS_LINUX_) && defined(_CPU_X86_64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    ctx->uc_mcontext.gregs[REG_RSP] = rsp;
    ctx->uc_mcontext.gregs[REG_RIP] = (uintptr_t)fptr;
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    ctx->uc_mcontext.mc_rsp = rsp;
    ctx->uc_mcontext.mc_rip = (uintptr_t)fptr;
#elif defined(_OS_LINUX_) && defined(_CPU_X86_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    ctx->uc_mcontext.gregs[REG_ESP] = rsp;
    ctx->uc_mcontext.gregs[REG_EIP] = (uintptr_t)fptr;
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    ctx->uc_mcontext.mc_esp = rsp;
    ctx->uc_mcontext.mc_eip = (uintptr_t)fptr;
#elif defined(_OS_OPENBSD_) && defined(_CPU_X86_64_)
    struct sigcontext *ctx = (struct sigcontext *)_ctx;
    rsp -= sizeof(void*);
    ctx->sc_rsp = rsp;
    ctx->sc_rip = fptr;
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    ctx->uc_mcontext.sp = rsp;
    ctx->uc_mcontext.regs[29] = 0; // Clear link register (x29)
    ctx->uc_mcontext.pc = (uintptr_t)fptr;
#elif defined(_OS_LINUX_) && defined(_CPU_ARM_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    uintptr_t target = (uintptr_t)fptr;
    // Apparently some glibc's sigreturn target is running in thumb state.
    // Mimic a `bx` instruction by setting the T(5) bit of CPSR
    // depending on the target address.
    uintptr_t cpsr = ctx->uc_mcontext.arm_cpsr;
    // Thumb mode function pointer should have the lowest bit set
    if (target & 1) {
        target = target & ~((uintptr_t)1);
        cpsr = cpsr | (1 << 5);
    }
    else {
        cpsr = cpsr & ~(1 << 5);
    }
    ctx->uc_mcontext.arm_cpsr = cpsr;
    ctx->uc_mcontext.arm_sp = rsp;
    ctx->uc_mcontext.arm_lr = 0; // Clear link register
    ctx->uc_mcontext.arm_pc = target;
#elif defined(_OS_DARWIN_) && (defined(_CPU_X86_64_) || defined(_CPU_AARCH64_))
    // Only used for SIGFPE.
    // This doesn't seems to be reliable when the SIGFPE is generated
    // from a divide-by-zero exception, which is now handled by
    // `catch_exception_raise`. It works fine when a signal is received
    // due to `kill`/`raise` though.
    ucontext64_t *ctx = (ucontext64_t*)_ctx;
#if defined(_CPU_X86_64_)
    rsp -= sizeof(void*);
    ctx->uc_mcontext64->__ss.__rsp = rsp;
    ctx->uc_mcontext64->__ss.__rip = (uintptr_t)fptr;
#else
    ctx->uc_mcontext64->__ss.__sp = rsp;
    ctx->uc_mcontext64->__ss.__pc = (uintptr_t)fptr;
    ctx->uc_mcontext64->__ss.__lr = 0;
#endif
#else
#pragma message("julia: throw-in-context not supported on this platform")
    // TODO Add support for PowerPC(64)?
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, sig);
    pthread_sigmask(SIG_UNBLOCK, &sset, NULL);
    fptr();
#endif
}

static void jl_throw_in_ctx(jl_task_t *ct, jl_value_t *e, int sig, void *sigctx)
{
    jl_ptls_t ptls = ct->ptls;
    if (!jl_get_safe_restore()) {
        ptls->bt_size =
            rec_backtrace_ctx(ptls->bt_data, JL_MAX_BT_SIZE, jl_to_bt_context(sigctx),
                              ct->gcstack);
        ptls->sig_exception = e;
    }
    jl_call_in_ctx(ptls, &jl_sig_throw, sig, sigctx);
}

static pthread_t signals_thread;

static int is_addr_on_stack(jl_task_t *ct, void *addr)
{
    if (ct->copy_stack) {
        jl_ptls_t ptls = ct->ptls;
        return ((char*)addr > (char*)ptls->stackbase - ptls->stacksize &&
                (char*)addr < (char*)ptls->stackbase);
    }
    return ((char*)addr > (char*)ct->stkbuf &&
            (char*)addr < (char*)ct->stkbuf + ct->bufsz);
}

static void sigdie_handler(int sig, siginfo_t *info, void *context)
{
    signal(sig, SIG_DFL);
    uv_tty_reset_mode();
    if (sig == SIGILL)
        jl_show_sigill(context);
    jl_task_t *ct = jl_get_current_task();
    jl_critical_error(sig, info->si_code, jl_to_bt_context(context), ct);
    if (ct)
        jl_atomic_store_relaxed(&ct->ptls->safepoint, (size_t*)NULL + 1);
    if (info->si_code == 0 ||
        info->si_code == SI_USER ||
#ifdef SI_KERNEL
        info->si_code == SI_KERNEL ||
#endif
        info->si_code == SI_QUEUE ||
#ifdef SI_MESGQ
        info->si_code == SI_MESGQ ||
#endif
#ifdef SI_ASYNCIO
        info->si_code == SI_ASYNCIO ||
#endif
#ifdef SI_SIGIO
        info->si_code == SI_SIGIO ||
#endif
#ifdef SI_TKILL
        info->si_code == SI_TKILL ||
#endif
        info->si_code == SI_TIMER)
        raise(sig);
    else if (sig != SIGSEGV &&
             sig != SIGBUS &&
             sig != SIGILL &&
             sig != SIGFPE &&
             sig != SIGTRAP)
        raise(sig);
    // fall-through return to re-execute faulting statement (but without the
    // error handler and the pgcstack having been destroyed)
}

#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
enum x86_trap_flags {
    USER_MODE = 0x4,
    WRITE_FAULT = 0x2,
    PAGE_PRESENT = 0x1 // whether this page is currently mapped into memory
};

int exc_reg_is_write_fault(uintptr_t err) {
    return err & WRITE_FAULT;
}
#elif defined(_CPU_AARCH64_)
enum aarch64_esr_layout {
    EC_MASK = ((uint32_t)0b111111) << 26,
    EC_DATA_ABORT = ((uint32_t)0b100100) << 26,
    DFSC_MASK = ((uint32_t)0b111111) << 0,
    ISR_DA_WnR = ((uint32_t)1) << 6
};

int exc_reg_is_write_fault(uintptr_t esr) {
    // n.b. we check that DFSC is either a permission fault (page in memory but not writable) or a translation fault (page not in memory)
    // but because of info->si_code == SEGV_ACCERR, we know the kernel could have brought the page into memory.
    // Access faults happen when trying to write to code or secure memory, which is a more severe violation, so we ignore those.
    // AArch64 appears to leaves it up to a given implementer whether atomic update errors are reported as read or write faults.
    return (esr & EC_MASK) == EC_DATA_ABORT &&
           (((esr & DFSC_MASK) >= 0b000100 &&   // Translation flag fault, level 0.
             (esr & DFSC_MASK) <= 0b000111) ||  // Translation fault, level 3.
            ((esr & DFSC_MASK) >= 0b001100 &&   // Permission flag fault, level 0.
             (esr & DFSC_MASK) <= 0b001111)) && // Permission fault, level 3.
           (esr & ISR_DA_WnR); // Attempted write
}
#endif

#if defined(HAVE_MACH)
#include "signals-mach.c"
#else

int jl_lock_stackwalk(void)
{
    jl_lock_profile();
    return 0;
}

void jl_unlock_stackwalk(int lockret)
{
    (void)lockret;
    jl_unlock_profile();
}


#if defined(_OS_LINUX_) && (defined(_CPU_X86_64_) || defined(_CPU_X86_))
int is_write_fault(void *context) {
    ucontext_t *ctx = (ucontext_t*)context;
    return exc_reg_is_write_fault(ctx->uc_mcontext.gregs[REG_ERR]);
}
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
struct linux_aarch64_ctx_header {
    uint32_t magic;
    uint32_t size;
};
const uint32_t linux_esr_magic = 0x45535201;

int is_write_fault(void *context) {
    ucontext_t *ctx = (ucontext_t*)context;
    struct linux_aarch64_ctx_header *extra =
        (struct linux_aarch64_ctx_header *)ctx->uc_mcontext.__reserved;
    while (extra->magic != 0) {
        if (extra->magic == linux_esr_magic) {
            return exc_reg_is_write_fault(*(uint64_t*)&extra[1]);
        }
        extra = (struct linux_aarch64_ctx_header *)
            (((uint8_t*)extra) + extra->size);
    }
    return 0;
}
#elif defined(_OS_FREEBSD_) && (defined(_CPU_X86_64_) || defined(_CPU_X86_))
int is_write_fault(void *context) {
    ucontext_t *ctx = (ucontext_t*)context;
    return exc_reg_is_write_fault(ctx->uc_mcontext.mc_err);
}
#elif defined(_OS_OPENBSD_) && defined(_CPU_X86_64_)
int is_write_fault(void *context) {
    struct sigcontext *ctx = (struct sigcontext *)context;
    return exc_reg_is_write_fault(ctx->sc_err);
}
#else
#pragma message("Implement this query for consistent PROT_NONE handling")
int is_write_fault(void *context) {
    return 0;
}
#endif

static int jl_is_on_sigstack(jl_ptls_t ptls, void *ptr, void *context)
{
    return (ptls->signal_stack != NULL &&
            is_addr_on_sigstack(ptls, ptr) &&
            is_addr_on_sigstack(ptls, (void*)jl_get_rsp_from_ctx(context)));
}

JL_NO_ASAN static void segv_handler(int sig, siginfo_t *info, void *context)
{
    assert(sig == SIGSEGV || sig == SIGBUS);
    if (jl_get_safe_restore()) { // restarting jl_ or profile
        jl_call_in_ctx(NULL, &jl_sig_throw, sig, context);
        return;
    }
    jl_task_t *ct = jl_get_current_task();
    if (ct == NULL || ct->ptls == NULL || jl_atomic_load_relaxed(&ct->ptls->gc_state) == JL_GC_STATE_WAITING) {
        sigdie_handler(sig, info, context);
        return;
    }
    if (sig == SIGSEGV && info->si_code == SEGV_ACCERR && jl_addr_is_safepoint((uintptr_t)info->si_addr) && !is_write_fault(context)) {
        jl_set_gc_and_wait();
        // Do not raise sigint on worker thread
        if (jl_atomic_load_relaxed(&ct->tid) != 0)
            return;
        // n.b. if the user might have seen that we were in a state where it
        // was safe to run GC concurrently, we might briefly enter a state
        // where our execution is not consistent with the gc_state of this
        // thread. That will quickly be rectified when we rerun the faulting
        // instruction and end up right back here, or we start to run the
        // exception handler and immediately hit the safepoint there.
        if (ct->ptls->defer_signal) {
            jl_safepoint_defer_sigint();
        }
        else if (jl_safepoint_consume_sigint()) {
            jl_clear_force_sigint();
            jl_throw_in_ctx(ct, jl_interrupt_exception, sig, context);
        }
        return;
    }
    if (ct->eh == NULL)
        sigdie_handler(sig, info, context);
    if ((sig != SIGBUS || info->si_code == BUS_ADRERR) && is_addr_on_stack(ct, info->si_addr)) { // stack overflow and not a BUS_ADRALN (alignment error)
        jl_throw_in_ctx(ct, jl_stackovf_exception, sig, context);
    }
    else if (jl_is_on_sigstack(ct->ptls, info->si_addr, context)) {
        // This mainly happens when one of the finalizers during final cleanup
        // on the signal stack has a deep/infinite recursion.
        // There isn't anything more we can do
        // (we are already corrupting that stack running this function)
        // so just call `_exit` to terminate immediately.
        jl_safe_printf("ERROR: Signal stack overflow, exit\n");
        jl_raise(sig);
    }
    else if (sig == SIGSEGV && info->si_code == SEGV_ACCERR && is_write_fault(context)) {  // writing to read-only memory (e.g., mmap)
        jl_throw_in_ctx(ct, jl_readonlymemory_exception, sig, context);
    }
    else {
        sigdie_handler(sig, info, context);
    }
}

#if !defined(JL_DISABLE_LIBUNWIND)
static bt_context_t *signal_context;
pthread_mutex_t in_signal_lock;
static pthread_cond_t exit_signal_cond;
static pthread_cond_t signal_caught_cond;

int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += timeout;
    pthread_mutex_lock(&in_signal_lock);
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    jl_task_t *ct2 = ptls2 ? jl_atomic_load_relaxed(&ptls2->current_task) : NULL;
    if (ct2 == NULL) {
        // this thread is not alive or already dead
        pthread_mutex_unlock(&in_signal_lock);
        return 0;
    }
    jl_atomic_store_release(&ptls2->signal_request, 1);
    pthread_kill(ptls2->system_id, SIGUSR2);
    // wait for thread to acknowledge
    int err = pthread_cond_timedwait(&signal_caught_cond, &in_signal_lock, &ts);
    if (err == ETIMEDOUT) {
        sig_atomic_t request = 1;
        if (jl_atomic_cmpswap(&ptls2->signal_request, &request, 0)) {
            pthread_mutex_unlock(&in_signal_lock);
            return 0;
        }
        // Request is either now 0 (meaning the other thread is waiting for
        //   exit_signal_cond already),
        // Or it is now -1 (meaning the other thread
        //   is waiting for in_signal_lock, and we need to release that lock
        //   here for a bit, until the other thread has a chance to get to the
        //   exit_signal_cond)
        if (request == -1) {
            err = pthread_cond_wait(&signal_caught_cond, &in_signal_lock);
            assert(!err);
        }
    }
    // Now the other thread is waiting on exit_signal_cond (verify that here by
    // checking it is 0, and add an acquire barrier for good measure)
    int request = jl_atomic_load_acquire(&ptls2->signal_request);
    assert(request == 0); (void) request;
    jl_atomic_store_release(&ptls2->signal_request, 1); // prepare to resume normally
    *ctx = *signal_context;
    return 1;
}

void jl_thread_resume(int tid)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    pthread_cond_broadcast(&exit_signal_cond);
    pthread_cond_wait(&signal_caught_cond, &in_signal_lock); // wait for thread to acknowledge (so that signal_request doesn't get mixed up)
    // The other thread is waiting to leave exit_signal_cond (verify that here by
    // checking it is 0, and add an acquire barrier for good measure)
    int request = jl_atomic_load_acquire(&ptls2->signal_request);
    assert(request == 0); (void) request;
    pthread_mutex_unlock(&in_signal_lock);
}
#endif

// Throw jl_interrupt_exception if the master thread is in a signal async region
// or if SIGINT happens too often.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    jl_safepoint_enable_sigint();
    jl_wake_libuv();
    jl_atomic_store_release(&ptls2->signal_request, 2);
    // This also makes sure `sleep` is aborted.
    pthread_kill(ptls2->system_id, SIGUSR2);
}

// Write only by signal handling thread, read only by main thread
// no sync necessary.
static int thread0_exit_signo = 0;
static void JL_NORETURN jl_exit_thread0_cb(void)
{
CFI_NORETURN
    jl_critical_error(thread0_exit_signo, 0, NULL, jl_current_task);
    jl_atexit_hook(128);
    jl_raise(thread0_exit_signo);
}

static void jl_exit_thread0(int signo, jl_bt_element_t *bt_data, size_t bt_size)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    bt_context_t signal_context;
    // This also makes sure `sleep` is aborted.
    if (jl_thread_suspend_and_get_state(0, 30, &signal_context)) {
        thread0_exit_signo = signo;
        ptls2->bt_size = bt_size; // <= JL_MAX_BT_SIZE
        memcpy(ptls2->bt_data, bt_data, ptls2->bt_size * sizeof(bt_data[0]));
        jl_atomic_store_release(&ptls2->signal_request, 3);
        jl_thread_resume(0); // resume with message 3 (call jl_exit_thread0_cb)
    }
    else {
        // thread 0 is gone? just do the exit ourself
        jl_raise(signo);
    }
}

// request:
// -1: beginning processing [invalid outside here]
//  0: nothing [not from here]
//  1: get state
//  2: throw sigint if `!defer_signal && io_wait` or if force throw threshold
//     is reached
//  3: raise `thread0_exit_signo` and try to exit
void usr2_handler(int sig, siginfo_t *info, void *ctx)
{
    jl_task_t *ct = jl_get_current_task();
    if (ct == NULL)
        return;
    jl_ptls_t ptls = ct->ptls;
    if (ptls == NULL)
        return;
    int errno_save = errno;
    // acknowledge that we saw the signal_request
    sig_atomic_t request = jl_atomic_exchange(&ptls->signal_request, -1);
#if !defined(JL_DISABLE_LIBUNWIND)
    if (request == 1) {
        pthread_mutex_lock(&in_signal_lock);
        signal_context = jl_to_bt_context(ctx);
        // acknowledge that we set the signal_caught_cond broadcast
        request = jl_atomic_exchange(&ptls->signal_request, 0);
        assert(request == -1); (void) request;
        pthread_cond_broadcast(&signal_caught_cond);
        pthread_cond_wait(&exit_signal_cond, &in_signal_lock);
        request = jl_atomic_exchange(&ptls->signal_request, 0);
        assert(request == 1 || request == 3);
        // acknowledge that we got the resume signal
        pthread_cond_broadcast(&signal_caught_cond);
        pthread_mutex_unlock(&in_signal_lock);
    }
    else
#endif
    jl_atomic_exchange(&ptls->signal_request, 0); // returns -1
    if (request == 2) {
        int force = jl_check_force_sigint();
        if (force || (!ptls->defer_signal && ptls->io_wait)) {
            jl_safepoint_consume_sigint();
            if (force)
                jl_safe_printf("WARNING: Force throwing a SIGINT\n");
            // Force a throw
            jl_clear_force_sigint();
            jl_throw_in_ctx(ct, jl_interrupt_exception, sig, ctx);
        }
    }
    else if (request == 3) {
        jl_call_in_ctx(ct->ptls, jl_exit_thread0_cb, sig, ctx);
    }
    errno = errno_save;
}

// Because SIGUSR1 is dual-purpose, and the timer can have trailing signals after being deleted,
// a 2-second grace period is imposed to ignore any trailing timer-created signals so they don't get
// confused for user triggers
uint64_t last_timer_delete_time = 0;

int timer_graceperiod_elapsed(void)
{
    return jl_hrtime() > (last_timer_delete_time + 2e9);
}

#if defined(HAVE_TIMER)
// Linux-style
#include <time.h>
#include <string.h>  // for memset

static timer_t timerprof;
static struct itimerspec itsprof;

JL_DLLEXPORT int jl_profile_start_timer(void)
{
    struct sigevent sigprof;

    // Establish the signal event
    memset(&sigprof, 0, sizeof(struct sigevent));
    sigprof.sigev_notify = SIGEV_SIGNAL;
    sigprof.sigev_signo = SIGUSR1;
    sigprof.sigev_value.sival_ptr = &timerprof;
    // Because SIGUSR1 is multipurpose, set `running` before so that we know that the first SIGUSR1 came from the timer
    running = 1;
    if (timer_create(CLOCK_REALTIME, &sigprof, &timerprof) == -1) {
        running = 0;
        return -2;
    }

    // Start the timer
    itsprof.it_interval.tv_sec = 0;
    itsprof.it_interval.tv_nsec = 0;
    itsprof.it_value.tv_sec = nsecprof / GIGA;
    itsprof.it_value.tv_nsec = nsecprof % GIGA;
    if (timer_settime(timerprof, 0, &itsprof, NULL) == -1) {
        running = 0;
        return -3;
    }
    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    if (running) {
        timer_delete(timerprof);
        last_timer_delete_time = jl_hrtime();
        running = 0;
    }
}

#elif defined(__OpenBSD__)

JL_DLLEXPORT int jl_profile_start_timer(void)
{
    return -1;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
}

#else

#error no profile tools available

#endif
#endif // HAVE_MACH

static void allocate_segv_handler(void)
{
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = segv_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    if (sigaction(SIGSEGV, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    // On AArch64, stack overflow triggers a SIGBUS
    if (sigaction(SIGBUS, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
}

void jl_install_thread_signal_handler(jl_ptls_t ptls)
{
#ifdef HAVE_MACH
    attach_exception_port(pthread_mach_thread_np(ptls->system_id), 0);
#endif
    stack_t ss;
    if (sigaltstack(NULL, &ss) < 0)
        jl_errorf("fatal error: sigaltstack: %s", strerror(errno));
    if ((ss.ss_flags & SS_DISABLE) != SS_DISABLE)
        return; // someone else appears to have already set this up, so just use that
    size_t ssize = sig_stack_size;
    void *signal_stack = jl_malloc_stack(&ssize, NULL);
    ss.ss_flags = 0;
    ss.ss_size = ssize;
    assert(ssize != 0);

#ifndef _OS_OPENBSD_
    /* fallback to malloc(), but it isn't possible on OpenBSD */
    if (signal_stack == NULL) {
        signal_stack = malloc(ssize);
        ssize = 0;
        if (signal_stack == NULL)
            jl_safe_printf("\nwarning: julia signal alt stack could not be allocated (StackOverflowError will be fatal on this thread).\n");
        else
            jl_safe_printf("\nwarning: julia signal stack allocated without guard page (launch foreign threads earlier to avoid this warning).\n");
    }
#endif

    if (signal_stack != NULL) {
        ss.ss_sp = signal_stack;
        if (sigaltstack(&ss, NULL) < 0)
            jl_errorf("fatal error: sigaltstack: %s", strerror(errno));
        ptls->signal_stack = signal_stack;
        ptls->signal_stack_size = ssize;
    }
}

const static int sigwait_sigs[] = {
    SIGINT, SIGTERM, SIGQUIT,
#ifdef SIGINFO
    SIGINFO,
#else
    SIGUSR1,
#endif
#if defined(HAVE_TIMER)
    SIGUSR1,
#endif
    0
};

static void jl_sigsetset(sigset_t *sset)
{
    sigemptyset(sset);
    for (const int *sig = sigwait_sigs; *sig; sig++)
        sigaddset(sset, *sig);
}

#ifdef HAVE_KEVENT
static void kqueue_signal(int *sigqueue, struct kevent *ev, int sig)
{
    if (*sigqueue == -1)
        return;
    EV_SET(ev, sig, EVFILT_SIGNAL, EV_ADD, 0, 0, 0);
    if (kevent(*sigqueue, ev, 1, NULL, 0, NULL)) {
        perror("signal kevent");
        close(*sigqueue);
        *sigqueue = -1;
    }
    else {
        // kqueue gets signals before SIG_IGN, but does not remove them from pending (unlike sigwait)
        signal(sig, SIG_IGN);
    }
}
#endif

void trigger_profile_peek(void)
{
    jl_safe_printf("\n======================================================================================\n");
    jl_safe_printf("Information request received. A stacktrace will print followed by a %.1f second profile\n", profile_peek_duration);
    jl_safe_printf("======================================================================================\n");
    if (bt_size_max == 0){
        // If the buffer hasn't been initialized, initialize with default size
        // Keep these values synchronized with Profile.default_init()
        if (jl_profile_init(10000000, 1000000) == -1) {
            jl_safe_printf("ERROR: could not initialize the profile buffer");
            return;
        }
    }
    bt_size_cur = 0; // clear profile buffer
    if (jl_profile_start_timer() < 0)
        jl_safe_printf("ERROR: Could not start profile timer\n");
    else
        profile_autostop_time = jl_hrtime() + (profile_peek_duration * 1e9);
}

static void *signal_listener(void *arg)
{
    static jl_bt_element_t bt_data[JL_MAX_BT_SIZE + 1];
    static size_t bt_size = 0;
    sigset_t sset;
    int sig, critical, profile;
    jl_sigsetset(&sset);
#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L
    siginfo_t info;
#endif
#ifdef HAVE_KEVENT
    struct kevent ev;
    int sigqueue = kqueue();
    if (sigqueue == -1) {
        perror("signal kqueue");
    }
    else {
        for (const int *sig = sigwait_sigs; *sig; sig++)
            kqueue_signal(&sigqueue, &ev, *sig);
        if (sigqueue == -1) {
            // re-enable sigwait for these
            for (const int *sig = sigwait_sigs; *sig; sig++)
                signal(*sig, SIG_DFL);
        }
    }
#endif
    while (1) {
        sig = 0;
        errno = 0;
#ifdef HAVE_KEVENT
        if (sigqueue != -1) {
            int nevents = kevent(sigqueue, NULL, 0, &ev, 1, NULL);
            if (nevents == -1) {
                if (errno == EINTR)
                    continue;
                perror("signal kevent");
            }
            if (nevents != 1) {
                close(sigqueue);
                sigqueue = -1;
                for (const int *sig = sigwait_sigs; *sig; sig++)
                    signal(*sig, SIG_DFL);
                continue;
            }
            sig = ev.ident;
        }
        else
#endif
#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L
        sig = sigwaitinfo(&sset, &info);
#else
        if (sigwait(&sset, &sig))
            sig = -1;
#endif
        if (sig == -1) {
            if (errno == EINTR)
                continue;
            sig = SIGABRT; // this branch can't occur, unless we had stack memory corruption of sset
        }
        profile = 0;
#ifndef HAVE_MACH
#if defined(HAVE_TIMER)
        profile = (sig == SIGUSR1);
#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L
        if (profile && !(info.si_code == SI_TIMER &&
                info.si_value.sival_ptr == &timerprof))
            profile = 0;
#endif
#endif
#endif

        if (sig == SIGINT) {
            if (jl_ignore_sigint()) {
                continue;
            }
            else if (exit_on_sigint) {
                critical = 1;
            }
            else {
                jl_try_deliver_sigint();
                continue;
            }
        }
        else {
            critical = 0;
        }

        critical |= (sig == SIGTERM);
        critical |= (sig == SIGABRT);
        critical |= (sig == SIGQUIT);
#ifdef SIGINFO
        critical |= (sig == SIGINFO);
#else
        critical |= (sig == SIGUSR1 && !profile);
#endif

        int doexit = critical;
#ifdef SIGINFO
        if (sig == SIGINFO) {
            if (running != 1)
                trigger_profile_peek();
            doexit = 0;
        }
#else
        if (sig == SIGUSR1) {
            if (running != 1 && timer_graceperiod_elapsed())
                trigger_profile_peek();
            doexit = 0;
        }
#endif
        if (doexit) {
            // The exit can get stuck if it happens at an unfortunate spot in thread 0
            // (unavoidable due to its async nature).
            // Try much harder to exit next time, if we get multiple exit requests.
            // 1. unblock the signal, so this thread can be killed by it
            // 2. reset the tty next, because we might die before we get another chance to do that
            // 3. attempt a graceful cleanup of julia, followed by an abrupt end to the C runtime (except for fflush)
            // 4. kill this thread with `raise`, to preserve the signo / exit code / and coredump configuration
            // Similar to jl_raise, but a slightly different order of operations
            sigset_t sset;
            sigemptyset(&sset);
            sigaddset(&sset, sig);
            pthread_sigmask(SIG_UNBLOCK, &sset, NULL);
#ifdef HAVE_KEVENT
            signal(sig, SIG_DFL);
#endif
            uv_tty_reset_mode();
            thread0_exit_count++;
            fflush(NULL);
            if (thread0_exit_count > 1) {
                raise(sig); // very unlikely to return
                _exit(128 + sig);
            }
        }

        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        bt_size = 0;
#if !defined(JL_DISABLE_LIBUNWIND)
        bt_context_t signal_context;
        // sample each thread, round-robin style in reverse order
        // (so that thread zero gets notified last)
        if (critical || profile) {
            int lockret = jl_lock_stackwalk();
            int *randperm;
            if (profile)
                 randperm = profile_get_randperm(nthreads);
            for (int idx = nthreads; idx-- > 0; ) {
                // Stop the threads in the random or reverse round-robin order.
                int i = profile ? randperm[idx] : idx;
                // notify thread to stop
                if (!jl_thread_suspend_and_get_state(i, 1, &signal_context))
                    continue;

                // do backtrace on thread contexts for critical signals
                // this part must be signal-handler safe
                if (critical) {
                    bt_size += rec_backtrace_ctx(bt_data + bt_size,
                            JL_MAX_BT_SIZE / nthreads - 1,
                            &signal_context, NULL);
                    bt_data[bt_size++].uintptr = 0;
                }

                // do backtrace for profiler
                if (profile && running) {
                    if (jl_profile_is_buffer_full()) {
                        // Buffer full: Delete the timer
                        jl_profile_stop_timer();
                    }
                    else {
                        // unwinding can fail, so keep track of the current state
                        // and restore from the SEGV handler if anything happens.
                        jl_jmp_buf *old_buf = jl_get_safe_restore();
                        jl_jmp_buf buf;

                        jl_set_safe_restore(&buf);
                        if (jl_setjmp(buf, 0)) {
                            jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
                        } else {
                            // Get backtrace data
                            bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)bt_data_prof + bt_size_cur,
                                    bt_size_max - bt_size_cur - 1, &signal_context, NULL);
                        }
                        jl_set_safe_restore(old_buf);

                        jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[i];

                        // store threadid but add 1 as 0 is preserved to indicate end of block
                        bt_data_prof[bt_size_cur++].uintptr = ptls2->tid + 1;

                        // store task id (never null)
                        bt_data_prof[bt_size_cur++].jlvalue = (jl_value_t*)jl_atomic_load_relaxed(&ptls2->current_task);

                        // store cpu cycle clock
                        bt_data_prof[bt_size_cur++].uintptr = cycleclock();

                        // store whether thread is sleeping but add 1 as 0 is preserved to indicate end of block
                        bt_data_prof[bt_size_cur++].uintptr = jl_atomic_load_relaxed(&ptls2->sleep_check_state) + 1;

                        // Mark the end of this block with two 0's
                        bt_data_prof[bt_size_cur++].uintptr = 0;
                        bt_data_prof[bt_size_cur++].uintptr = 0;
                    }
                }

                // notify thread to resume
                jl_thread_resume(i);
            }
            jl_unlock_stackwalk(lockret);
        }
#ifndef HAVE_MACH
        if (profile && running) {
            jl_check_profile_autostop();
#if defined(HAVE_TIMER)
            timer_settime(timerprof, 0, &itsprof, NULL);
#endif
        }
#endif
#endif

        // this part is async with the running of the rest of the program
        // and must be thread-safe, but not necessarily signal-handler safe
        if (doexit) {
//            // this is probably always SI_USER (0x10001 / 65537), so we suppress it
//            int si_code = 0;
//#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L && !HAVE_KEVENT
//            si_code = info.si_code;
//#endif
            jl_exit_thread0(sig, bt_data, bt_size);
        }
        else if (critical) {
            // critical in this case actually means SIGINFO request
#ifndef SIGINFO // SIGINFO already prints something similar automatically
            int nrunning = 0;
            for (int idx = nthreads; idx-- > 0; ) {
                jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[idx];
                nrunning += !jl_atomic_load_relaxed(&ptls2->sleep_check_state);
            }
            jl_safe_printf("\ncmd: %s %d running %d of %d\n", jl_options.julia_bin ? jl_options.julia_bin : "julia", uv_os_getpid(), nrunning, nthreads);
#endif

            jl_safe_printf("\nsignal (%d): %s\n", sig, strsignal(sig));
            size_t i;
            for (i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
                jl_print_bt_entry_codeloc(bt_data + i);
            }
        }
    }
    return NULL;
}

void restore_signals(void)
{
    sigemptyset(&jl_sigint_sset);
    sigaddset(&jl_sigint_sset, SIGINT);

    sigset_t sset;
    jl_sigsetset(&sset);
    pthread_sigmask(SIG_SETMASK, &sset, 0);

#if !defined(HAVE_MACH) && !defined(JL_DISABLE_LIBUNWIND)
    if (pthread_mutex_init(&in_signal_lock, NULL) != 0 ||
        pthread_cond_init(&exit_signal_cond, NULL) != 0 ||
        pthread_cond_init(&signal_caught_cond, NULL) != 0) {
        jl_error("SIGUSR pthread init failed");
    }
#endif

    if (pthread_create(&signals_thread, NULL, signal_listener, NULL) != 0) {
        jl_error("pthread_create(signal_listener) failed");
    }
}

static void fpe_handler(int sig, siginfo_t *info, void *context)
{
    (void)info;
    if (jl_get_safe_restore()) { // restarting jl_ or profile
        jl_call_in_ctx(NULL, &jl_sig_throw, sig, context);
        return;
    }
    jl_task_t *ct = jl_get_current_task();
    if (ct == NULL || ct->eh == NULL) // exception on foreign thread is fatal
        sigdie_handler(sig, info, context);
    else
        jl_throw_in_ctx(ct, jl_diverror_exception, sig, context);
}

static void sigint_handler(int sig)
{
    jl_sigint_passed = 1;
}

#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
static void sigtrap_handler(int sig, siginfo_t *info, void *context)
{
    uintptr_t pc = ((ucontext_t*)context)->uc_mcontext->__ss.__pc; // TODO: Do this in linux as well
    uint32_t* code = (uint32_t*)(pc);                              // https://gcc.gnu.org/legacy-ml/gcc-patches/2013-11/msg02228.html
    if (*code == 0xd4200020) { // brk #0x1 which is what LLVM defines as trap
        signal(sig, SIG_DFL);
        sig = SIGILL; // redefine this as as an "unreachable reached" error message
        sigdie_handler(sig, info, context);
    }
}
#endif

void jl_install_default_signal_handlers(void)
{
    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_sigaction = fpe_handler;
    actf.sa_flags = SA_SIGINFO;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
    struct sigaction acttrap;
    memset(&acttrap, 0, sizeof(struct sigaction));
    sigemptyset(&acttrap.sa_mask);
    acttrap.sa_sigaction = sigtrap_handler;
    acttrap.sa_flags = SA_SIGINFO;
    if (sigaction(SIGTRAP, &acttrap, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#else
    if (signal(SIGTRAP, SIG_IGN) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGTRAP");
    }
#endif
    struct sigaction actint;
    memset(&actint, 0, sizeof(struct sigaction));
    sigemptyset(&actint.sa_mask);
    actint.sa_handler = sigint_handler;
    actint.sa_flags = 0;
    if (sigaction(SIGINT, &actint, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGPIPE");
    }

#if defined(HAVE_MACH)
    allocate_mach_handler();
#else
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = usr2_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESTART;
    if (sigaction(SIGUSR2, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif

    allocate_segv_handler();

    struct sigaction act_die;
    memset(&act_die, 0, sizeof(struct sigaction));
    sigemptyset(&act_die.sa_mask);
    act_die.sa_sigaction = sigdie_handler;
    act_die.sa_flags = SA_SIGINFO | SA_RESETHAND;
    if (sigaction(SIGILL, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGABRT, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGSYS, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    // need to ensure the following signals are not SIG_IGN, even though they will be blocked
    act_die.sa_flags = SA_SIGINFO | SA_RESTART | SA_RESETHAND;
#ifdef SIGINFO
    if (sigaction(SIGINFO, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#else
    if (sigaction(SIGUSR1, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif
}

JL_DLLEXPORT void jl_install_sigint_handler(void)
{
    // TODO: ?
}

JL_DLLEXPORT int jl_repl_raise_sigtstp(void)
{
    return raise(SIGTSTP);
}
