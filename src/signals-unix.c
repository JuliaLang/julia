// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <pthread.h>
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
#ifdef __APPLE__ // Darwin's mach ports allow signal-free thread management
#define HAVE_MACH
#define HAVE_KEVENT
#elif defined(__FreeBSD__) // generic bsd
#define HAVE_ITIMER
#else // generic linux
#define HAVE_TIMER
#endif

#ifdef HAVE_KEVENT
#include <sys/event.h>
#endif

// 8M signal stack, same as default stack size and enough
// for reasonable finalizers.
// Should also be enough for parallel GC when we have it =)
#define sig_stack_size (8 * 1024 * 1024)

#include "julia_assert.h"

static bt_context_t *jl_to_bt_context(void *sigctx)
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
#elif defined(_OS_DARWIN_)
    const ucontext64_t *ctx = (const ucontext64_t*)_ctx;
    return ctx->uc_mcontext64->__ss.__rsp;
#else
    // TODO Add support for FreeBSD and PowerPC(64)?
    return 0;
#endif
}

// Modify signal context `_ctx` so that `fptr` will execute when the signal
// returns. `fptr` will execute on the signal stack, and must not return.
static void jl_call_in_ctx(jl_ptls_t ptls, void (*fptr)(void), int sig, void *_ctx)
{
    // Modifying the ucontext should work but there is concern that
    // sigreturn oriented programming mitigation can work against us
    // by rejecting ucontext that is modified.
    // The current (staged) implementation in the Linux Kernel only
    // checks that the syscall is made in the signal handler and that
    // the ucontext address is valid. Hopefully the value of the ucontext
    // will not be part of the validation...
    if (!ptls->signal_stack) {
        sigset_t sset;
        sigemptyset(&sset);
        sigaddset(&sset, sig);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        fptr();
        return;
    }
    uintptr_t rsp = (uintptr_t)ptls->signal_stack + sig_stack_size;
    assert(rsp % 16 == 0);
#if defined(_OS_LINUX_) && defined(_CPU_X86_64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;
    ctx->uc_mcontext.gregs[REG_RSP] = rsp;
    ctx->uc_mcontext.gregs[REG_RIP] = (uintptr_t)fptr;
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;
    ctx->uc_mcontext.mc_rsp = rsp;
    ctx->uc_mcontext.mc_rip = (uintptr_t)fptr;
#elif defined(_OS_LINUX_) && defined(_CPU_X86_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;
    ctx->uc_mcontext.gregs[REG_ESP] = rsp;
    ctx->uc_mcontext.gregs[REG_EIP] = (uintptr_t)fptr;
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;
    ctx->uc_mcontext.mc_esp = rsp;
    ctx->uc_mcontext.mc_eip = (uintptr_t)fptr;
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
#elif defined(_OS_DARWIN_)
    // Only used for SIGFPE.
    // This doesn't seems to be reliable when the SIGFPE is generated
    // from a divide-by-zero exception, which is now handled by
    // `catch_exception_raise`. It works fine when a signal is received
    // due to `kill`/`raise` though.
    ucontext64_t *ctx = (ucontext64_t*)_ctx;
    rsp -= sizeof(void*);
    *(void**)rsp = NULL;
    ctx->uc_mcontext64->__ss.__rsp = rsp;
    ctx->uc_mcontext64->__ss.__rip = (uintptr_t)fptr;
#else
#warning "julia: throw-in-context not supported on this platform"
    // TODO Add support for PowerPC(64)?
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, sig);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);
    fptr();
#endif
}

static void jl_throw_in_ctx(jl_ptls_t ptls, jl_value_t *e, int sig, void *sigctx)
{
    if (!ptls->safe_restore)
        ptls->bt_size = rec_backtrace_ctx(ptls->bt_data, JL_MAX_BT_SIZE,
                                          jl_to_bt_context(sigctx), ptls->pgcstack, 0);
    ptls->sig_exception = e;
    jl_call_in_ctx(ptls, &jl_sig_throw, sig, sigctx);
}

static pthread_t signals_thread;

static int is_addr_on_stack(jl_ptls_t ptls, void *addr)
{
    jl_task_t *t = ptls->current_task;
    if (t->copy_stack)
        return ((char*)addr > (char*)ptls->stackbase - ptls->stacksize &&
                (char*)addr < (char*)ptls->stackbase);
    else
        return ((char*)addr > (char*)t->stkbuf &&
                (char*)addr < (char*)t->stkbuf + t->bufsz);
}

static void sigdie_handler(int sig, siginfo_t *info, void *context)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    sigset_t sset;
    uv_tty_reset_mode();
    if (sig == SIGILL)
        jl_show_sigill(context);
    jl_critical_error(sig, jl_to_bt_context(context),
                      ptls->bt_data, &ptls->bt_size);
    sigfillset(&sset);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);
    signal(sig, SIG_DFL);
    if (sig != SIGSEGV &&
        sig != SIGBUS &&
        sig != SIGILL) {
        raise(sig);
    }
    // fall-through return to re-execute faulting statement (but without the error handler)
}

#if defined(HAVE_MACH)
#include <signals-mach.c>
#else

static int is_addr_on_sigstack(jl_ptls_t ptls, void *ptr)
{
    // One guard page for signal_stack.
    return !((char*)ptr < (char*)ptls->signal_stack - jl_page_size ||
             (char*)ptr > (char*)ptls->signal_stack + sig_stack_size);
}

static int jl_is_on_sigstack(jl_ptls_t ptls, void *ptr, void *context)
{
    return (is_addr_on_sigstack(ptls, ptr) &&
            is_addr_on_sigstack(ptls, (void*)jl_get_rsp_from_ctx(context)));
}

static void segv_handler(int sig, siginfo_t *info, void *context)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(sig == SIGSEGV || sig == SIGBUS);

    if (jl_addr_is_safepoint((uintptr_t)info->si_addr)) {
        jl_set_gc_and_wait();
        // Do not raise sigint on worker thread
        if (ptls->tid != 0)
            return;
        if (ptls->defer_signal) {
            jl_safepoint_defer_sigint();
        }
        else if (jl_safepoint_consume_sigint()) {
            jl_clear_force_sigint();
            jl_throw_in_ctx(ptls, jl_interrupt_exception, sig, context);
        }
        return;
    }
    if (ptls->safe_restore || is_addr_on_stack(ptls, info->si_addr)) { // stack overflow, or restarting jl_
        jl_throw_in_ctx(ptls, jl_stackovf_exception, sig, context);
    }
    else if (jl_is_on_sigstack(ptls, info->si_addr, context)) {
        // This mainly happens when one of the finalizers during final cleanup
        // on the signal stack has a deep/infinite recursion.
        // There isn't anything more we can do
        // (we are already corrupting that stack running this function)
        // so just call `_exit` to terminate immediately.
        jl_safe_printf("ERROR: Signal stack overflow, exit\n");
        _exit(sig + 128);
    }
    else if (sig == SIGSEGV && info->si_code == SEGV_ACCERR) {  // writing to read-only memory (e.g., mmap)
        jl_throw_in_ctx(ptls, jl_readonlymemory_exception, sig, context);
    }
    else {
#ifdef SEGV_EXCEPTION
        jl_throw_in_ctx(ptls, jl_segv_exception, sig, context);
#else
        sigdie_handler(sig, info, context);
#endif
    }
}

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

#if !defined(JL_DISABLE_LIBUNWIND)
static unw_context_t *volatile signal_context;
static pthread_mutex_t in_signal_lock;
static pthread_cond_t exit_signal_cond;
static pthread_cond_t signal_caught_cond;

static void jl_thread_suspend_and_get_state(int tid, unw_context_t **ctx)
{
    pthread_mutex_lock(&in_signal_lock);
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    jl_atomic_store_release(&ptls2->signal_request, 1);
    pthread_kill(ptls2->system_id, SIGUSR2);
    pthread_cond_wait(&signal_caught_cond, &in_signal_lock);  // wait for thread to acknowledge
    assert(jl_atomic_load_acquire(&ptls2->signal_request) == 0);
    *ctx = signal_context;
}

static void jl_thread_resume(int tid, int sig)
{
    (void)sig;
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    jl_atomic_store_release(&ptls2->signal_request, 1);
    pthread_cond_broadcast(&exit_signal_cond);
    pthread_cond_wait(&signal_caught_cond, &in_signal_lock); // wait for thread to acknowledge
    assert(jl_atomic_load_acquire(&ptls2->signal_request) == 0);
    pthread_mutex_unlock(&in_signal_lock);
}
#endif

// Throw jl_interrupt_exception if the master thread is in a signal async region
// or if SIGINT happens too often.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_all_tls_states[0];
    jl_safepoint_enable_sigint();
    jl_wake_libuv();
    jl_atomic_store_release(&ptls2->signal_request, 2);
    // This also makes sure `sleep` is aborted.
    pthread_kill(ptls2->system_id, SIGUSR2);
}

// Write only by signal handling thread, read only by main thread
// no sync necessary.
static int thread0_exit_state = 0;
static void jl_exit_thread0_cb(void)
{
    // This can get stuck if it happens at an unfortunate spot
    // (unavoidable due to its async nature).
    // Try harder to exit each time if we get multiple exit requests.
    if (thread0_exit_count <= 1) {
        jl_exit(thread0_exit_state);
    }
    else if (thread0_exit_count == 2) {
        exit(thread0_exit_state);
    }
    else {
        _exit(thread0_exit_state);
    }
}

static void jl_exit_thread0(int state)
{
    jl_ptls_t ptls2 = jl_all_tls_states[0];
    thread0_exit_state = state;
    jl_atomic_store_release(&ptls2->signal_request, 3);
    pthread_kill(ptls2->system_id, SIGUSR2);
}

// request:
// 0: nothing
// 1: get state
// 2: throw sigint if `!defer_signal && io_wait` or if force throw threshold
//    is reached
// 3: exit with `thread0_exit_state`
void usr2_handler(int sig, siginfo_t *info, void *ctx)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    int errno_save = errno;
    sig_atomic_t request = jl_atomic_exchange(&ptls->signal_request, 0);
#if !defined(JL_DISABLE_LIBUNWIND)
    if (request == 1) {
        signal_context = jl_to_bt_context(ctx);

        pthread_mutex_lock(&in_signal_lock);
        pthread_cond_broadcast(&signal_caught_cond);
        pthread_cond_wait(&exit_signal_cond, &in_signal_lock);
        request = jl_atomic_exchange(&ptls->signal_request, 0);
        assert(request == 1);
        (void)request;
        pthread_cond_broadcast(&signal_caught_cond);
        pthread_mutex_unlock(&in_signal_lock);
    }
    else
#endif
    if (request == 2) {
        int force = jl_check_force_sigint();
        if (force || (!ptls->defer_signal && ptls->io_wait)) {
            jl_safepoint_consume_sigint();
            if (force)
                jl_safe_printf("WARNING: Force throwing a SIGINT\n");
            // Force a throw
            jl_clear_force_sigint();
            jl_throw_in_ctx(ptls, jl_interrupt_exception, sig, ctx);
        }
    }
    else if (request == 3) {
        jl_call_in_ctx(ptls, jl_exit_thread0_cb, sig, ctx);
    }
    errno = errno_save;
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
    if (timer_create(CLOCK_REALTIME, &sigprof, &timerprof) == -1)
        return -2;

    // Start the timer
    itsprof.it_interval.tv_sec = nsecprof/GIGA;
    itsprof.it_interval.tv_nsec = nsecprof%GIGA;
    itsprof.it_value.tv_sec = nsecprof/GIGA;
    itsprof.it_value.tv_nsec = nsecprof%GIGA;
    if (timer_settime(timerprof, 0, &itsprof, NULL) == -1)
        return -3;

    running = 1;
    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    if (running)
        timer_delete(timerprof);
    running = 0;
}

#elif defined(HAVE_ITIMER)
// BSD-style timers
#include <string.h>
#include <sys/time.h>
struct itimerval timerprof;

JL_DLLEXPORT int jl_profile_start_timer(void)
{
    timerprof.it_interval.tv_sec = nsecprof/GIGA;
    timerprof.it_interval.tv_usec = (nsecprof%GIGA)/1000;
    timerprof.it_value.tv_sec = nsecprof/GIGA;
    timerprof.it_value.tv_usec = (nsecprof%GIGA)/1000;
    if (setitimer(ITIMER_PROF, &timerprof, 0) == -1)
        return -3;

    running = 1;

    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    if (running) {
        memset(&timerprof, 0, sizeof(timerprof));
        setitimer(ITIMER_PROF, &timerprof, 0);
    }
    running = 0;
}

#else

#error no profile tools available

#endif
#endif // HAVE_MACH

static void *alloc_sigstack(size_t size)
{
    size_t pagesz = jl_getpagesize();
    // Add one guard page to catch stack overflow in the signal handler
    size = LLT_ALIGN(size, pagesz) + pagesz;
    void *stackbuff = mmap(0, size, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stackbuff == MAP_FAILED)
        jl_errorf("fatal error allocating signal stack: mmap: %s",
                  strerror(errno));
    mprotect(stackbuff, pagesz, PROT_NONE);
    return (void*)((char*)stackbuff + pagesz);
}

void jl_install_thread_signal_handler(jl_ptls_t ptls)
{
    void *signal_stack = alloc_sigstack(sig_stack_size);
    stack_t ss;
    ss.ss_flags = 0;
    ss.ss_size = sig_stack_size - 16;
    ss.ss_sp = signal_stack;
    if (sigaltstack(&ss, NULL) < 0) {
        jl_errorf("fatal error: sigaltstack: %s", strerror(errno));
    }

#if !defined(HAVE_MACH)
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = usr2_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESTART;
    if (sigaction(SIGUSR2, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif

    ptls->signal_stack = signal_stack;
}

static void jl_sigsetset(sigset_t *sset)
{
    sigemptyset(sset);
    sigaddset(sset, SIGINT);
    sigaddset(sset, SIGTERM);
    sigaddset(sset, SIGABRT);
    sigaddset(sset, SIGQUIT);
#ifdef SIGINFO
    sigaddset(sset, SIGINFO);
#else
    sigaddset(sset, SIGUSR1);
#endif
#if defined(HAVE_TIMER)
    sigaddset(sset, SIGUSR1);
#elif defined(HAVE_ITIMER)
    sigaddset(sset, SIGPROF);
#endif
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
        signal(sig, SIG_IGN);
    }
}
#endif

static void *signal_listener(void *arg)
{
    static jl_bt_element_t bt_data[JL_MAX_BT_SIZE + 1];
    static size_t bt_size = 0;
    sigset_t sset;
    int sig, critical, profile;
    jl_sigsetset(&sset);
#ifdef HAVE_KEVENT
    struct kevent ev;
    int sigqueue = kqueue();
    if (sigqueue == -1) {
        perror("signal kqueue");
    }
    else {
        kqueue_signal(&sigqueue, &ev, SIGINT);
        kqueue_signal(&sigqueue, &ev, SIGTERM);
        kqueue_signal(&sigqueue, &ev, SIGABRT);
        kqueue_signal(&sigqueue, &ev, SIGQUIT);
#ifdef SIGINFO
        kqueue_signal(&sigqueue, &ev, SIGINFO);
#else
        kqueue_signal(&sigqueue, &ev, SIGUSR1);
#endif
#if defined(HAVE_TIMER)
        kqueue_signal(&sigqueue, &ev, SIGUSR1);
#elif defined(HAVE_ITIMER)
        kqueue_signal(&sigqueue, &ev, SIGPROF);
#endif
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
                continue;
            }
            sig = ev.ident;
        }
        else
#endif
#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L
        siginfo_t info;
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
#ifndef HAVE_MACH
#if defined(HAVE_TIMER)
        profile = (sig == SIGUSR1);
#if _POSIX_C_SOURCE >= 199309L
        if (profile && !(info.si_code == SI_TIMER &&
	            info.si_value.sival_ptr == &timerprof))
            profile = 0;
#endif
#elif defined(HAVE_ITIMER)
        profile = (sig == SIGPROF);
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
        if (sig == SIGINFO)
            doexit = 0;
#else
        if (sig == SIGUSR1)
            doexit = 0;
#endif

        bt_size = 0;
#if !defined(JL_DISABLE_LIBUNWIND)
        unw_context_t *signal_context;
        // sample each thread, round-robin style in reverse order
        // (so that thread zero gets notified last)
        for (int i = jl_n_threads; i-- > 0; ) {
            // notify thread to stop
            jl_thread_suspend_and_get_state(i, &signal_context);

            // do backtrace on thread contexts for critical signals
            // this part must be signal-handler safe
            if (critical) {
                bt_size += rec_backtrace_ctx(bt_data + bt_size,
                        JL_MAX_BT_SIZE / jl_n_threads - 1,
                        signal_context, NULL, 1);
                bt_data[bt_size++].uintptr = 0;
            }

            // do backtrace for profiler
            if (profile && running) {
                if (bt_size_cur < bt_size_max - 1) {
                    // unwinding can fail, so keep track of the current state
                    // and restore from the SEGV handler if anything happens.
                    jl_ptls_t ptls = jl_get_ptls_states();
                    jl_jmp_buf *old_buf = ptls->safe_restore;
                    jl_jmp_buf buf;

                    ptls->safe_restore = &buf;
                    if (jl_setjmp(buf, 0)) {
                        jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
                    } else {
                        // Get backtrace data
                        bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)bt_data_prof + bt_size_cur,
                                bt_size_max - bt_size_cur - 1, signal_context, NULL, 1);
                    }
                    ptls->safe_restore = old_buf;

                    // Mark the end of this block with 0
                    bt_data_prof[bt_size_cur++].uintptr = 0;
                }
                if (bt_size_cur >= bt_size_max - 1) {
                    // Buffer full: Delete the timer
                    jl_profile_stop_timer();
                }
            }

            // notify thread to resume
            jl_thread_resume(i, sig);
        }
#endif

        // this part is async with the running of the rest of the program
        // and must be thread-safe, but not necessarily signal-handler safe
        if (critical) {
            jl_critical_error(sig, NULL, bt_data, &bt_size);
            if (doexit) {
                thread0_exit_count++;
                jl_exit_thread0(128 + sig);
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
    sigprocmask(SIG_SETMASK, &sset, 0);

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
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_throw_in_ctx(ptls, jl_diverror_exception, sig, context);
}

static void sigint_handler(int sig)
{
    jl_sigint_passed = 1;
}

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
    if (signal(SIGTRAP, SIG_IGN) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGTRAP");
    }

    allocate_segv_handler();

    struct sigaction act_die;
    memset(&act_die, 0, sizeof(struct sigaction));
    sigemptyset(&act_die.sa_mask);
    act_die.sa_sigaction = sigdie_handler;
    act_die.sa_flags = SA_SIGINFO;
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
    act_die.sa_flags = SA_SIGINFO | SA_RESTART;
#if defined(HAVE_ITIMER)
    if (sigaction(SIGPROF, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif
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
