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

void sigdie_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;
    uv_tty_reset_mode();
    sigfillset(&sset);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);
    signal(sig, SIG_DFL);
#ifdef __APPLE__
    jl_critical_error(sig, (bt_context_t)&((ucontext64_t*)context)->uc_mcontext64->__ss, bt_data, &bt_size);
#else
    jl_critical_error(sig, (ucontext_t*)context, bt_data, &bt_size);
#endif
    if (sig != SIGSEGV &&
        sig != SIGBUS &&
        sig != SIGILL) {
        raise(sig);
    }
    // fall-through return to re-execute faulting statement (but without the error handler)
}

void jl_sigsetset(sigset_t *sset)
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
#ifdef SIGPROF
    sigaddset(sset, SIGPROF);
#endif
}

void restore_signals(void)
{
    sigset_t sset;
    jl_sigsetset(&sset);
    sigprocmask(SIG_SETMASK, &sset, 0);
}

static pthread_t root_tid;
static pthread_mutex_t in_signal_lock;
static pthread_cond_t signal_caught_cond;
static pthread_cond_t exit_signal_cond;
static bt_context_t signal_context;
static int remote_sig;
static void *signal_listener(void *arg)
{
    static intptr_t bt_data[MAX_BT_SIZE + 1];
    static size_t bt_size = 0;
    sigset_t sset;
    int sig = 0, critical;
    jl_sigsetset(&sset);
    while (1) {
        remote_sig = 0;
        sigwait(&sset, &sig);


        critical = (sig == SIGINT && exit_on_sigint);
        critical |= (sig == SIGTERM);
        critical |= (sig == SIGABRT);
        critical |= (sig == SIGQUIT);
#ifdef SIGINFO
        critical |= (sig == SIGINFO);
#else
        critical |= (sig == SIGUSR1); // TODO: && si->si_value.sival_ptr != &timerprof)
#endif

        pthread_mutex_lock(&in_signal_lock);
        remote_sig = sig;
        pthread_kill(root_tid, SIGUSR2);
        pthread_cond_wait(&signal_caught_cond, &in_signal_lock);
        // this part must be signal-handler safe
        if (critical)
            bt_size = rec_backtrace_ctx(bt_data, MAX_BT_SIZE, signal_context);
#ifdef SIGPROF
        if (sig == SIGPROF)
#else
        if (sig == SIGUSR1) // TODO: && !critical
#endif
        {
            if (running && bt_size_cur < bt_size_max) {
                // Get backtrace data
                bt_size_cur += rec_backtrace_ctx((ptrint_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, signal_context);
                // Mark the end of this block with 0
                bt_data_prof[bt_size_cur] = 0;
                bt_size_cur++;
            }
            if (bt_size_cur >= bt_size_max) {
                // Buffer full: Delete the  timer
                jl_profile_stop_timer();
            }
        }
        remote_sig = 0;
        pthread_cond_broadcast(&exit_signal_cond);
        pthread_mutex_unlock(&in_signal_lock);

        // this part is async with the running of the rest of the program
        // and must be thread-safe, but not necessarily signal-handler safe
        if (critical) {
            jl_critical_error(sig, NULL, bt_data, &bt_size);
#ifdef SIGINFO
            if (sig != SIGINFO)
#else
            if (sig != SIGUSR1)
#endif
                jl_exit(128 + sig);
        }
    }
}

#ifndef __APPLE__ // Apple handles this from a separate thread (catch_exception_raise)
extern int in_jl_;
static void segv_handler(int sig, siginfo_t *info, void *context)
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

static void usr2_handler(int sig, siginfo_t *info, void *ctx)
{
    sigset_t sset;
    ucontext_t *context = (ucontext_t*)ctx;
    if (remote_sig) {
        int realsig = remote_sig;
#ifdef __APPLE__
            signal_context = (bt_context_t)&context->uc_mcontext->__ss;
#else
            signal_context = context;
#endif
        pthread_mutex_lock(&in_signal_lock);
        pthread_cond_broadcast(&signal_caught_cond);
        pthread_cond_wait(&exit_signal_cond, &in_signal_lock);
        pthread_mutex_unlock(&in_signal_lock);

        if (realsig == SIGINT) {
            if (jl_defer_signal) {
                jl_signal_pending = realsig;
            }
            else {
                jl_signal_pending = 0;
                sigemptyset(&sset);
                sigaddset(&sset, sig);
                sigprocmask(SIG_UNBLOCK, &sset, NULL);
                jl_throw(jl_interrupt_exception);
            }
        }
    }
}

DLLEXPORT void jl_install_sigint_handler(void)
{
    // TODO
}

void jl_install_default_signal_handlers(void)
{
    pthread_t thread;
    pthread_attr_t attr;

    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_handler = fpe_handler;
    actf.sa_flags = 0;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGPIPE");
    }

#if defined(__linux__) && defined(JL_USE_INTEL_JITEVENTS)
    if (jl_using_intel_jitevents)
        // Intel VTune Amplifier needs at least 64k for alternate stack.
        if (SIGSTKSZ < 1<<16)
            sig_stack_size = 1<<16;
#endif
    signal_stack = malloc(sig_stack_size);
    stack_t ss;
    ss.ss_flags = 0;
    ss.ss_size = sig_stack_size;
    ss.ss_sp = signal_stack;
    if (sigaltstack(&ss, NULL) < 0) {
        jl_errorf("fatal error: sigaltstack: %s", strerror(errno));
    }

#ifdef __APPLE__
    {
        kern_return_t ret;
        mach_port_t self = mach_task_self();
        ret = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &segv_port);
        HANDLE_MACH_ERROR("mach_port_allocate",ret);
        ret = mach_port_insert_right(self, segv_port, segv_port, MACH_MSG_TYPE_MAKE_SEND);
        HANDLE_MACH_ERROR("mach_port_insert_right",ret);
        // Alright, create a thread to serve as the listener for exceptions
        if (pthread_attr_init(&attr) != 0) {
            jl_error("pthread_attr_init failed");
        }
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&thread, &attr, mach_segv_listener, NULL) != 0) {
            jl_error("pthread_create failed");
        }
        pthread_attr_destroy(&attr);
        attach_exception_port();
    }
#else // defined(_OS_DARWIN_)
    {
        struct sigaction act;
        memset(&act, 0, sizeof(struct sigaction));
        sigemptyset(&act.sa_mask);
        act.sa_sigaction = segv_handler;
        act.sa_flags = SA_ONSTACK | SA_SIGINFO;
        if (sigaction(SIGSEGV, &act, NULL) < 0) {
            jl_errorf("fatal error: sigaction: %s", strerror(errno));
        }
    }
#endif // defined(_OS_DARWIN_)

    struct sigaction act_die;
    memset(&act_die, 0, sizeof(struct sigaction));
    sigemptyset(&act_die.sa_mask);
    act_die.sa_sigaction = sigdie_handler;
    act_die.sa_flags = SA_SIGINFO;
    if (sigaction(SIGBUS, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGILL, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    if (sigaction(SIGSYS, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
    // need the following signals not to be SIG_IGN, even though they will be blocked
    if (sigaction(SIGUSR1, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#ifdef SIGINFO
    if (sigaction(SIGINFO, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif
#ifdef SIGPROF
    if (sigaction(SIGPROF, &act_die, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif

    root_tid = pthread_self();
    if (pthread_mutex_init(&in_signal_lock, NULL) != 0 ||
        pthread_cond_init(&signal_caught_cond, NULL) != 0 ||
        pthread_cond_init(&exit_signal_cond, NULL) != 0 ||
        pthread_attr_init(&attr) != 0) {
        jl_error("SIGUSR pthread init failed");
    }
    if (pthread_create(&thread, &attr, signal_listener, NULL) != 0) {
        jl_error("pthread_create(signal_listener) failed");
    }
    pthread_attr_destroy(&attr);

    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = usr2_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    if (sigaction(SIGUSR2, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
}
