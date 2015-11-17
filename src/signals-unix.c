// This file is a part of Julia. License is MIT: http://julialang.org/license


void fpe_handler(int arg)
{
    (void)arg;
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGFPE);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    jl_throw(jl_diverror_exception);
}

static int is_addr_on_stack(void *addr)
{
#ifdef COPY_STACKS
    return ((char*)addr > (char*)jl_stack_lo-3000000 &&
            (char*)addr < (char*)jl_stack_hi);
#else
    return ((char*)addr > (char*)jl_current_task->stkbuf &&
            (char*)addr < (char*)jl_current_task->stkbuf + jl_current_task->ssize);
#endif
}

#ifndef SIGINFO
#define SIGINFO SIGUSR1
#endif
void sigdie_handler(int sig, siginfo_t *info, void *context)
{
    if (sig != SIGINFO) {
        sigset_t sset;
        uv_tty_reset_mode();
        sigfillset(&sset);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        signal(sig, SIG_DFL);
    }
    jl_safe_printf("\nsignal (%d): %s\n", sig, strsignal(sig));
#ifdef __APPLE__
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, (bt_context_t)&((ucontext64_t*)context)->uc_mcontext64->__ss);
#else
    bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, (ucontext_t*)context);
#endif
    jlbacktrace();
    gc_debug_print_status();
    if (sig != SIGSEGV &&
        sig != SIGBUS &&
        sig != SIGILL &&
        sig != SIGINFO) {
        raise(sig);
    }
}

#ifndef __APPLE__ // Apple handles this from a separate thread (catch_exception_raise)
extern int in_jl_;
void segv_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;
    assert(sig == SIGSEGV);

    if (in_jl_ || is_addr_on_stack(info->si_addr)) { // stack overflow, or restarting jl_
        sigemptyset(&sset);
        sigaddset(&sset, SIGSEGV);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_stackovf_exception);
    }
    else if (info->si_code == SEGV_ACCERR) {  // writing to read-only memory (e.g., mmap)
        sigemptyset(&sset);
        sigaddset(&sset, SIGSEGV);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_readonlymemory_exception);
    }
    else {
#ifdef SEGV_EXCEPTION
        sigemptyset(&sset);
        sigaddset(&sset, SIGSEGV);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_throw(jl_segv_exception);
#else
        sigdie_handler(sig, info, context);
#endif
    }
}
#endif

void restore_signals(void)
{
    sigset_t sset;
    sigemptyset(&sset);
    sigprocmask(SIG_SETMASK, &sset, 0);
}

void sigint_handler(int sig, siginfo_t *info, void *context)
{
    if (jl_defer_signal) {
        jl_signal_pending = sig;
    }
    else {
        jl_signal_pending = 0;
        sigset_t sset;
        sigemptyset(&sset);
        sigaddset(&sset, SIGINT);
        sigprocmask(SIG_UNBLOCK, &sset, NULL);
        jl_sigint_action();
    }
}

DLLEXPORT void jl_install_sigint_handler(void)
{
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = sigint_handler;
    act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINT, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
}

void jl_install_default_signal_handlers(void)
{
#if defined(__linux__) && defined(JL_USE_INTEL_JITEVENTS)
    if (jl_using_intel_jitevents)
        // Intel VTune Amplifier needs at least 64k for alternate stack.
        if (SIGSTKSZ < 1<<16)
            sig_stack_size = 1<<16;
#endif
    signal_stack = malloc(sig_stack_size);
    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_handler = fpe_handler;
    actf.sa_flags = 0;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (signal(SIGPIPE,SIG_IGN) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGPIPE");
    }
#if defined (_OS_DARWIN_)
    kern_return_t ret;
    mach_port_t self = mach_task_self();
    ret = mach_port_allocate(self,MACH_PORT_RIGHT_RECEIVE,&segv_port);
    HANDLE_MACH_ERROR("mach_port_allocate",ret);
    ret = mach_port_insert_right(self,segv_port,segv_port,MACH_MSG_TYPE_MAKE_SEND);
    HANDLE_MACH_ERROR("mach_port_insert_right",ret);

    // Alright, create a thread to serve as the listener for exceptions
    pthread_t thread;
    pthread_attr_t attr;
    if (pthread_attr_init(&attr) != 0) {
        jl_error("pthread_attr_init failed");
    }
    pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
    if (pthread_create(&thread,&attr,mach_segv_listener,NULL) != 0) {
        jl_error("pthread_create failed");
    }
    pthread_attr_destroy(&attr);

    attach_exception_port();
#else // defined(_OS_DARWIN_)
    stack_t ss;
    ss.ss_flags = 0;
    ss.ss_size = sig_stack_size;
    ss.ss_sp = signal_stack;
    if (sigaltstack(&ss, NULL) < 0) {
        jl_errorf("fatal error: sigaltstack: %s", strerror(errno));
    }

    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = segv_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    if (sigaction(SIGSEGV, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif // defined(_OS_DARWIN_)
    struct sigaction act_die;
    memset(&act_die, 0, sizeof(struct sigaction));
    sigemptyset(&act_die.sa_mask);
    act_die.sa_sigaction = sigdie_handler;
    act_die.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINFO, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGBUS, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGILL, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGTERM, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGABRT, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGQUIT, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGSYS, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
}

