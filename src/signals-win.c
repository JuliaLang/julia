// This file is a part of Julia. License is MIT: https://julialang.org/license

// Windows
// Note that this file is `#include`d by "signal-handling.c"
#include <mmsystem.h> // hidden by LEAN_AND_MEAN

static const size_t sig_stack_size = 131072; // 128k reserved for backtrace_fiber for stack overflow handling

// Copied from MINGW_FLOAT_H which may not be found due to a collision with the builtin gcc float.h
// eventually we can probably integrate this into OpenLibm.
#if defined(_COMPILER_GCC_)
void __cdecl __MINGW_NOTHROW _fpreset (void);
void __cdecl __MINGW_NOTHROW fpreset (void);
#else
void __cdecl _fpreset (void);
void __cdecl fpreset (void);
#endif
#define _FPE_INVALID        0x81
#define _FPE_DENORMAL       0x82
#define _FPE_ZERODIVIDE     0x83
#define _FPE_OVERFLOW       0x84
#define _FPE_UNDERFLOW      0x85
#define _FPE_INEXACT        0x86
#define _FPE_UNEMULATED     0x87
#define _FPE_SQRTNEG        0x88
#define _FPE_STACKOVERFLOW  0x8a
#define _FPE_STACKUNDERFLOW 0x8b
#define _FPE_EXPLICITGEN    0x8c    /* raise( SIGFPE ); */

void __cdecl crt_sig_handler(int sig, int num)
{
    CONTEXT Context;
    switch (sig) {
    case SIGFPE:
        fpreset();
        signal(SIGFPE, (void (__cdecl *)(int))crt_sig_handler);
        switch(num) {
        case _FPE_INVALID:
        case _FPE_OVERFLOW:
        case _FPE_UNDERFLOW:
        default:
            jl_errorf("Unexpected FPE Error 0x%X", num);
            break;
        case _FPE_ZERODIVIDE:
            jl_throw(jl_diverror_exception);
            break;
        }
        break;
    case SIGINT:
        signal(SIGINT, (void (__cdecl *)(int))crt_sig_handler);
        if (!jl_ignore_sigint()) {
            if (exit_on_sigint)
                jl_exit(130); // 128 + SIGINT
            jl_sigint_request_cancellation();
        }
        break;
    default: // SIGSEGV, SIGTERM, SIGILL, SIGABRT
        if (sig == SIGSEGV) { // restarting jl_ or profile
            jl_jmp_buf *saferestore = jl_get_safe_restore();
            if (saferestore) {
                signal(sig, (void (__cdecl *)(int))crt_sig_handler);
                jl_longjmp(*saferestore, 1);
                return;
            }
        }
        memset(&Context, 0, sizeof(Context));
        RtlCaptureContext(&Context);

        ios_t s;
        ios_mem(&s, 0);
        if (sig == SIGILL)
            jl_fprint_sigill(&s, &Context);
        jl_fprint_critical_error(&s, sig, 0, &Context, jl_get_current_task());

        // First write to stderr
        ios_write_direct(ios_safe_stderr, &s);

        // Then write to Application log
        HANDLE event_source = RegisterEventSourceW(NULL, L"julia");
        if (event_source != INVALID_HANDLE_VALUE) {
            ios_putc('\0', &s);
            const wchar_t *strings[] = { ios_utf8_to_wchar(s.buf) };
            ReportEventW(
                event_source, EVENTLOG_ERROR_TYPE, /* category */ 0, /* event_id */ (DWORD)0xE0000000L,
               /* user_sid */ NULL, /* n_strings */ 1, /* data_size */ 0, strings, /* data */ NULL
            );
            free((void *)strings[0]);

            if (jl_options.alert_on_critical_error) {
                MessageBoxW(NULL, /* message */ L"error: libjulia received a fatal signal.\n\n"
                                                L"See Application log in Event Viewer for more information.",
                            /* title */ L"fatal error in libjulia", MB_OK | MB_ICONEXCLAMATION | MB_SYSTEMMODAL);
            }
        }
        raise(sig);
    }
}

// StackOverflowException needs extra stack space to record the backtrace
// so we keep one around, shared by all threads
static uv_mutex_t backtrace_lock;
static win32_ucontext_t collect_backtrace_fiber;
static win32_ucontext_t error_return_fiber;
static PCONTEXT stkerror_ctx;
static jl_ptls_t stkerror_ptls;
static int have_backtrace_fiber;
static void JL_NORETURN start_backtrace_fiber(void)
{
    // print the warning (this mysteriously needs a lot of stack for the WriteFile syscall)
    stack_overflow_warning();
    // collect the backtrace
    stkerror_ptls->bt_size =
        rec_backtrace_ctx(stkerror_ptls->bt_data, JL_MAX_BT_SIZE, stkerror_ctx,
                          NULL /*current_task?*/);
    // switch back to the execution fiber
    jl_setcontext(&error_return_fiber);
    abort();
}

void restore_signals(void)
{
    // turn on ctrl-c handler
    SetConsoleCtrlHandler(NULL, 0);
}

int jl_simulate_longjmp(jl_jmp_buf mctx, bt_context_t *c, int val) JL_NOTSAFEPOINT;

static void jl_throw_in_ctx(jl_task_t *ct, jl_value_t *excpt, PCONTEXT ctxThread) JL_NOTSAFEPOINT
{
    jl_jmp_buf *saferestore = jl_get_safe_restore();
    if (saferestore) { // restarting jl_ or profile
        if (!jl_simulate_longjmp(*saferestore, ctxThread, 1))
            abort();
        return;
    }
    assert(ct && excpt);
    // This redirect abandons every frame between the interrupted context and
    // the handler. A reset context published in one of those frames would
    // dangle - and may be consumed by an off-thread cancellation sender - so
    // clear it before rewriting the context. The matching
    // jl_eh_restore_state republishes the outer context saved at handler
    // entry. The same applies to a foreign-call cancellation-handler guard
    // published in an abandoned frame.
    jl_atomic_store_release(&ct->reset_ctx, NULL);
    jl_atomic_store_release(&ct->cancel_handler_ctx, NULL);
    jl_ptls_t ptls = ct->ptls;
    ptls->bt_size = 0;
    if (excpt != jl_stackovf_exception) {
        ptls->bt_size = rec_backtrace_ctx(ptls->bt_data, JL_MAX_BT_SIZE, ctxThread,
                                          ct->gcstack);
    }
    else if (have_backtrace_fiber) {
        uv_mutex_lock(&backtrace_lock);
        stkerror_ctx = ctxThread;
        stkerror_ptls = ptls;
        jl_swapcontext(&error_return_fiber, &collect_backtrace_fiber);
        uv_mutex_unlock(&backtrace_lock);
    }
    ptls->sig_exception = excpt;
    ptls->io_wait = 0;
    jl_handler_t *eh = ct->eh;
    if (eh != NULL) {
        asan_unpoison_task_stack(ct, &eh->eh_ctx);
        if (!jl_simulate_longjmp(eh->eh_ctx, ctxThread, 1))
            abort();
    }
    else {
        jl_no_exc_handler(excpt, ct);
    }
}

HANDLE hMainThread = INVALID_HANDLE_VALUE;

// Serializes every path that suspends a thread and rewrites its context
// (jl_send_cancellation_signal and jl_send_abandon_signal on any thread,
// and jl_try_deliver_sigint) for
// its complete suspend/get/set/resume sequence: two rewriters working from
// the same suspended snapshot would install conflicting continuations and
// task chains. (The profiler does not need it: it only reads contexts, and
// suspend counts nest.) It also prevents two threads delivering
// cancellations at each other from freezing both (the suspend-count analog
// of a deadlock): a rewriter always takes this lock before suspending and
// never blocks while holding a suspension, so a suspended thread can never
// hold it.
static SRWLOCK ctx_rewrite_lock = SRWLOCK_INIT;

// === Cancellation-handler delivery ==========================================
// The suspend-based analog of the Unix implementation (see signals-unix.c):
// the sender saves the interrupted thread's full CONTEXT (including FP
// state) onto the interrupted stack, redirects the thread to a trampoline
// that runs fn(state, sev), and rigs the trampoline's return address to
// fault on a dedicated no-access page. A first-chance vectored exception
// handler recognizes that fault and restores the saved CONTEXT wholesale,
// resuming the originally interrupted instruction. (A vectored handler runs
// before any frame-based SEH search, so the synthetic frame needs no unwind
// information.)

static void *jl_win_restore_page = NULL;

static inline int jl_addr_is_win_restore_trigger(uintptr_t addr)
{
    uintptr_t page_addr = (uintptr_t)jl_win_restore_page;
    return page_addr != 0 && addr >= page_addr && addr < page_addr + jl_page_size;
}

// The return target rigged under the trampoline. An asm stub (rather than a
// C function) so that no prologue moves Rsp before the faulting read: at the
// fault, Rsp still points just below the saved CONTEXT.
extern void jl_win_restore_trigger(void);
#if defined(_CPU_X86_64_)
__asm__(
    "  .globl jl_win_restore_trigger\n"
    "jl_win_restore_trigger:\n"
    "  movq jl_win_restore_page(%rip), %r11\n"
    "  movq (%r11), %r11\n" // EXCEPTION_ACCESS_VIOLATION here
    "  ud2\n"               // should never reach here
);
#elif defined(_CPU_X86_)
__asm__(
    "  .globl _jl_win_restore_trigger\n"
    "_jl_win_restore_trigger:\n"
    "  movl _jl_win_restore_page, %eax\n"
    "  movl (%eax), %eax\n" // EXCEPTION_ACCESS_VIOLATION here
    "  ud2\n"               // should never reach here
);
#endif

// Runs on the interrupted thread with the interrupted CONTEXT saved on the
// stack below: invoke the registered cancellation handler with its
// arguments from the per-thread save area. Returning runs into the restore
// trigger.
static void jl_win_cancel_handler_trampoline(jl_ptls_t ptls)
{
    jl_cancel_handler_save_t *save = &ptls->cancel_handler_save;
    save->fn(save->state, save->sev);
}

// Rewrite the (suspended) thread context so that it runs fptr(arg0) on a
// frame carved below the interrupted stack, with the full interrupted
// CONTEXT saved in that frame for the restore trigger.
static void jl_win_call_in_context(CONTEXT *ctx, void (*fptr)(void), uintptr_t arg0)
{
#if defined(_CPU_X86_64_)
    uintptr_t sp = (uintptr_t)ctx->Rsp; // no red zone in the Win64 ABI
    sp = (sp - sizeof(CONTEXT)) & ~(uintptr_t)15;
    CONTEXT *saved = (CONTEXT*)sp;
    memcpy(saved, ctx, sizeof(CONTEXT));
    sp -= 32;            // the callee's register-home space, above the return address
    sp -= sizeof(void*); // return-address slot: entry Rsp == 8 (mod 16), per the ABI
    *(uintptr_t*)sp = (uintptr_t)&jl_win_restore_trigger;
    ctx->Rsp = sp;
    ctx->Rip = (uintptr_t)fptr;
    ctx->Rcx = arg0;
#elif defined(_CPU_X86_)
    uintptr_t sp = (uintptr_t)ctx->Esp;
    sp = (sp - sizeof(CONTEXT)) & ~(uintptr_t)15;
    CONTEXT *saved = (CONTEXT*)sp;
    memcpy(saved, ctx, sizeof(CONTEXT));
    sp -= sizeof(void*); // the trampoline's (cdecl) argument
    *(uintptr_t*)sp = arg0;
    sp -= sizeof(void*); // return-address slot
    *(uintptr_t*)sp = (uintptr_t)&jl_win_restore_trigger;
    ctx->Esp = sp;
    ctx->Eip = (uintptr_t)fptr;
#endif
}

static LONG WINAPI jl_win_restore_veh(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
    if (ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION &&
        ExceptionInfo->ExceptionRecord->ExceptionFlags == 0 &&
        jl_addr_is_win_restore_trigger(ExceptionInfo->ExceptionRecord->ExceptionInformation[1])) {
        // Returning from a cancellation-handler delivery: the trampoline's
        // `ret` left the stack pointer at its home space (x86_64) or its
        // argument slot (x86), with the saved CONTEXT just above. Restore
        // it wholesale and resume the originally interrupted instruction.
#if defined(_CPU_X86_64_)
        CONTEXT *saved = (CONTEXT*)(ExceptionInfo->ContextRecord->Rsp + 32);
#elif defined(_CPU_X86_)
        CONTEXT *saved = (CONTEXT*)(ExceptionInfo->ContextRecord->Esp + sizeof(void*));
#endif
        memcpy(ExceptionInfo->ContextRecord, saved, sizeof(CONTEXT));
        jl_task_t *ct = jl_get_current_task();
        if (ct != NULL && ct->ptls != NULL)
            ct->ptls->cancel_handler_armed = 0;
        return EXCEPTION_CONTINUE_EXECUTION;
    }
    return EXCEPTION_CONTINUE_SEARCH;
}

static void jl_win_init_cancel_handler_delivery(void)
{
    jl_win_restore_page = VirtualAlloc(NULL, jl_page_size, MEM_RESERVE | MEM_COMMIT, PAGE_NOACCESS);
    if (jl_win_restore_page == NULL)
        jl_error("fatal error: could not allocate the cancellation restore-trigger page");
    if (AddVectoredExceptionHandler(1 /* first */, jl_win_restore_veh) == NULL)
        jl_error("fatal error: could not install the cancellation restore handler");
}

// Deliver a cancellation shootdown to the target thread's current task's
// published interruptible-region context, if available: the suspend-based
// analog of the Unix SIGUSR2 request-5 delivery (see signals-unix.c). For a
// compiled reset region, rewrites the suspended thread's context as if the
// reset point's setjmp had returned again (with JL_RESET_CODE_CANCEL as the
// setjmp return), restoring the gcstack and eh saved at establishment; for
// a foreign call carrying a cancellation handler, hijacks the thread to run
// fn(state, sev) on its own stack and resume. Best-effort: any check
// failing simply drops the request (recovery is level-triggered at the
// task's next cancellation point), and the caller retries.
static int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx);
void jl_thread_resume(int tid);

static void jl_send_reset_signal(int16_t tid, int reset_code) JL_NOTSAFEPOINT
{
    jl_value_t *bound;
    jl_reset_ctx_t *reset_ctx;
    jl_cancel_handler_ctx_t *hctx;
    if (tid < 0 || tid >= jl_atomic_load_acquire(&jl_n_threads))
        return;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL)
        return;
    jl_task_t *ct2 = jl_atomic_load_relaxed(&ptls2->current_task);
    if (ct2 == NULL)
        return;
    // Only proceed if the task has an interruptible-region context
    // published - a purely polling victim between cancellation points never
    // has one. Never self-suspend: a thread's own reset region is always
    // cleared here (the ccall reaching this function is itself an unsafe
    // point), but a protected runtime span (e.g. the GMP allocation hooks)
    // can reach this sender - through a finalizer running cancel! - with
    // its own handler context still published.
    if (GetThreadId(ptls2->system_id) == GetCurrentThreadId())
        return;
    // Exception to the published-region requirement: while a ^C dispatch is
    // pending, a cancellation send also reaches a region-less task carrying
    // a token binding - the episode-propagation step below needs no
    // published region.
    if (jl_atomic_load_relaxed(&ct2->reset_ctx) == NULL &&
        jl_atomic_load_relaxed(&ct2->cancel_handler_ctx) == NULL) {
        bound = jl_atomic_load_relaxed(&ct2->bound_cancel_token);
        if (reset_code != JL_RESET_CODE_CANCEL ||
            bound == NULL || bound == jl_nothing ||
            !jl_atomic_load_relaxed(&jl_sigint_dispatch_pending))
            return;
    }
    AcquireSRWLockExclusive(&ctx_rewrite_lock);
    // The freeze must complete before any state is examined: validating (or
    // consuming) the published context while the victim still runs races
    // with it re-executing the establishing cancellation point (rewriting
    // the buffer) or leaving the frame entirely, and a SetThreadContext
    // computed from such a buffer redirects the thread into garbage.
    // (jl_thread_suspend_and_get_state's GetThreadContext is what completes
    // the asynchronous SuspendThread.) The profile (read) lock guarantees
    // the thread is not frozen while holding the debuginfo write lock; it
    // is only needed around the suspend itself.
    CONTEXT ctxThread;
    jl_lock_profile();
    int suspended = jl_thread_suspend_and_get_state(tid, 0, &ctxThread);
    jl_unlock_profile();
    if (!suspended) {
        ReleaseSRWLockExclusive(&ctx_rewrite_lock);
        return;
    }
    // Re-check now that the thread cannot run (the current task may have
    // switched before the freeze). Delivery is gated on an actual
    // cancellation of the task's bound token source - coherent with the
    // published regions, since everything that may rebind it while a region
    // is live (exception handlers, the finalizer bracket) restores the pair
    // together.
    ct2 = jl_atomic_load_relaxed(&ptls2->current_task);
    if (ct2 == NULL)
        goto resume;
    // A pending ^C episode reaches scoped descendant sources through the
    // julia-side listener's walk; when the listener is starved, carry it
    // into the frozen task's own bound source here so its next cancellation
    // point sees it (the flavor gates below re-read the source state).
    if (reset_code == JL_RESET_CODE_CANCEL) {
        bound = jl_atomic_load_relaxed(&ct2->bound_cancel_token);
        if (bound != NULL && bound != jl_nothing &&
            jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state) == 0)
            jl_sigint_propagate_to_bound(bound);
    }
    hctx = jl_atomic_load_acquire(&ct2->cancel_handler_ctx);
    if (hctx != NULL) {
        // Handler flavor: a published foreign-call cancellation-handler
        // guard takes priority over (and suppresses) the reset - its span
        // (e.g. a protected allocator) is exactly where a longjmp must not
        // land - for cancellations AND preemptions alike (a preemption
        // stays pending in the polled request byte). For an actual
        // cancellation of the bound token, hijack the thread to run
        // fn(state, sev) on its own stack - at most one delivery at a time
        // per thread (the save area holds one; skips recover
        // level-triggered).
        if (reset_code == JL_RESET_CODE_CANCEL && !ptls2->cancel_handler_armed) {
            bound = jl_atomic_load_relaxed(&ct2->bound_cancel_token);
            if (bound != NULL && bound != jl_nothing &&
                jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state) != 0) {
                jl_cancel_handler_save_t *save = &ptls2->cancel_handler_save;
                save->fn = hctx->fn;
                save->state = hctx->state;
                save->sev = jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state);
                ptls2->cancel_handler_armed = 1;
                jl_win_call_in_context(&ctxThread, (void (*)(void))&jl_win_cancel_handler_trampoline,
                                       (uintptr_t)ptls2);
                ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
                if (!SetThreadContext(ptls2->system_id, &ctxThread))
                    ptls2->cancel_handler_armed = 0;
            }
        }
        goto resume;
    }
    // Reset flavor, additionally gated on the thread running Julia code
    // (gc_state == 0): a thread inside a GC-safe region may be raced by a
    // concurrent stop-the-world, and a redirect back into Julia code would
    // break that protocol.
    if (jl_atomic_load_relaxed(&ptls2->gc_state) != JL_GC_STATE_UNSAFE)
        goto resume;
    reset_ctx = jl_atomic_load_acquire(&ct2->reset_ctx);
    if (reset_ctx == NULL || reset_ctx->sp == 0)
        goto resume;
    if (reset_code == JL_RESET_CODE_CANCEL) {
        // A preempt shootdown checks no source; the re-executed point
        // observes the setjmp return code and yields.
        bound = jl_atomic_load_relaxed(&ct2->bound_cancel_token);
        if (bound == NULL || bound == jl_nothing ||
            jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state) == 0)
            goto resume;
    }
    // Consume the reset point (prevents a double reset; unlike on Unix the
    // consumer here is not the victim thread itself, so use an exchange to
    // arbitrate against concurrent senders) and redirect the thread there,
    // restoring the gcstack and eh recorded at establishment: the interrupt
    // may have landed inside a reset-safe callee whose pushes onto either
    // chain die with the abandoned stack region.
    reset_ctx = jl_atomic_exchange(&ct2->reset_ctx, NULL);
    if (reset_ctx == NULL || reset_ctx->sp == 0)
        goto resume;
    // Compute and install the redirected thread context first, and rewind
    // the task's gcstack/eh chains only once the redirect is committed: if
    // either step fails (e.g. jl_simulate_longjmp is unavailable under
    // sanitizers), the thread must resume exactly as it was, with its
    // deeper chains intact and the (unconsumed) region republished.
    if (!jl_simulate_longjmp(reset_ctx->mctx, &ctxThread, reset_code))
        goto republish;
    ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_FLOATING_POINT;
    if (!SetThreadContext(ptls2->system_id, &ctxThread))
        goto republish;
    // Committed: restore the GC-frame chain head and innermost exception
    // handler recorded at establishment - the interrupt may have landed
    // inside a callee whose pushes onto either chain die with the abandoned
    // stack region.
    ct2->gcstack = reset_ctx->gcstack;
    ct2->eh = reset_ctx->eh;
    goto resume;
republish:
    jl_atomic_store_release(&ct2->reset_ctx, reset_ctx);
resume:
    jl_thread_resume(tid);
    ReleaseSRWLockExclusive(&ctx_rewrite_lock);
}


// Try to throw the exception in the master thread.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    // Hold the rewrite lock until the thread is resumed: it serializes the
    // complete suspend/get/set/resume sequence against other context
    // rewriters (jl_send_cancellation_signal), which would otherwise install
    // a conflicting continuation computed from the same suspended snapshot.
    AcquireSRWLockExclusive(&ctx_rewrite_lock);
    jl_lock_profile();
    jl_safepoint_enable_sigint();
    jl_wake_libuv();
    if ((DWORD)-1 == SuspendThread(hMainThread)) {
        // error
        jl_safe_printf("error: SuspendThread failed\n");
        jl_unlock_profile();
        ReleaseSRWLockExclusive(&ctx_rewrite_lock);
        return;
    }
    jl_unlock_profile();
    int force = jl_check_force_sigint();
    if (force || (!ptls2->defer_signal && ptls2->io_wait)) {
        jl_safepoint_consume_sigint();
        if (force)
            jl_safe_printf("WARNING: Force throwing a SIGINT\n");
        // Force a throw
        jl_clear_force_sigint();
        CONTEXT ctxThread;
        memset(&ctxThread, 0, sizeof(CONTEXT));
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_FLOATING_POINT;
        if (!GetThreadContext(hMainThread, &ctxThread)) {
            // error
            jl_safe_printf("error: GetThreadContext failed\n");
            ReleaseSRWLockExclusive(&ctx_rewrite_lock);
            return;
        }
        jl_task_t *ct = jl_atomic_load_relaxed(&ptls2->current_task);
        jl_throw_in_ctx(ct, jl_interrupt_exception, &ctxThread);
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_FLOATING_POINT;
        if (!SetThreadContext(hMainThread, &ctxThread)) {
            jl_safe_printf("error: SetThreadContext failed\n");
            // error
            ReleaseSRWLockExclusive(&ctx_rewrite_lock);
            return;
        }
    }
    if ((DWORD)-1 == ResumeThread(hMainThread)) {
        jl_safe_printf("error: ResumeThread failed\n");
        // error
        ReleaseSRWLockExclusive(&ctx_rewrite_lock);
        return;
    }
    ReleaseSRWLockExclusive(&ctx_rewrite_lock);
}

// Switch the target thread's current (already committed) task to
// ptls->abandon_to (see jl_abandon_task_request): freeze the thread, validate the
// pending request against its frozen state, and on commit redirect it into
// the abandon callback (which never returns). Holds the rewrite lock like
// every other suspend-and-rewrite path; the profile read lock covers the
// suspension itself (see jl_send_reset_signal above).
void jl_send_abandon_signal(int16_t tid) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL)
        return;
    AcquireSRWLockExclusive(&ctx_rewrite_lock);
    CONTEXT ctxThread;
    jl_lock_profile();
    int suspended = jl_thread_suspend_and_get_state(tid, 0, &ctxThread);
    jl_unlock_profile();
    if (!suspended) {
        ReleaseSRWLockExclusive(&ctx_rewrite_lock);
        return;
    }
    // The victim thread is frozen: validate the pending request against its
    // state and, on commit, redirect it into the abandon callback. A commit
    // is rolled back to a refusal if the redirect cannot be completed - the
    // callback is what publishes the abandoned task state, so an
    // unredirected victim resumes untouched. On refusal the requester
    // observes the verdict and withdraws.
    if (jl_abandon_try_commit(ptls2)) {
        // Redirect the thread to call jl_abandon_task_cb (which never
        // returns) on a minimal fake frame.
#if defined(_CPU_X86_64_)
        uintptr_t sp = (uintptr_t)ctxThread.Rsp;
        sp = (sp - 256) & ~(uintptr_t)15; // skip resume data, realign
        sp -= sizeof(uintptr_t); // fake return address slot
        *(uintptr_t*)sp = 0;
        ctxThread.Rsp = (DWORD64)sp;
        ctxThread.Rip = (DWORD64)&jl_abandon_task_cb;
#elif defined(_CPU_X86_)
        uintptr_t sp = (uintptr_t)ctxThread.Esp;
        sp = (sp - 64) & ~(uintptr_t)15;
        sp -= sizeof(uintptr_t); // fake return address slot
        *(uintptr_t*)sp = 0;
        ctxThread.Esp = (DWORD)sp;
        ctxThread.Eip = (DWORD)&jl_abandon_task_cb;
#endif
        ctxThread.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (!SetThreadContext(ptls2->system_id, &ctxThread)) {
            // Roll the commit back: nothing observable was published yet
            // (the task state is written by the callback).
            jl_atomic_store_release(&ptls2->abandon_state, JL_ABANDON_REFUSED);
        }
    }
    jl_thread_resume(tid);
    ReleaseSRWLockExclusive(&ctx_rewrite_lock);
}

// Direct-abandonment request in flight from this console handler (the
// victim's thread id), or -1 (see the unix listener's state machine).
static int16_t direct_abandon_tid = -1;

static BOOL WINAPI sigint_handler(DWORD wsig) //This needs winapi types to guarantee __stdcall
{
    int sig;
    //windows signals use different numbers from unix (raise)
    switch(wsig) {
        case CTRL_C_EVENT: sig = SIGINT; break;
        //case CTRL_BREAK_EVENT: sig = SIGTERM; break;
        // etc.
        default: sig = SIGTERM; break;
    }
    if (direct_abandon_tid >= 0) {
        int verdict = jl_abandon_task_poll(direct_abandon_tid);
        if (verdict == 1) {
            direct_abandon_tid = -1;
            // Let the sigint listener task perform the Julia-side cleanup.
            deliver_sigint_notification();
        }
        else if (verdict == -1) {
            direct_abandon_tid = -1;
            jl_safe_printf("\nWARNING: Could not abandon the current task (it holds runtime resources); still trying.\n");
        }
    }
    if (!jl_ignore_sigint()) {
        if (exit_on_sigint)
            jl_exit(128 + sig); // 128 + SIGINT
        if (sig == SIGINT) {
            // If the escalation timer has expired, this repeated ^C abandons
            // the stuck task, unless the julia-side escalation
            // (SAFE -> ABANDON_EXTERNAL -> ABANDON_ALL) can still make
            // progress on it.
            jl_task_t *rescue_task = NULL;
            jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
            // See signals-unix.c: only abandon a thread busy in managed
            // compute - a GC-safe thread is parked, not the stuck victim -
            // but give a thread transiently inside the allocator or GC a
            // brief window rather than consuming the press.
            if (jl_sigint_rescue_timer_expired_peek() && jl_sigint_direct_abandon_allowed()) {
                for (int tries = 0; tries < 100; tries++) {
                    if (jl_atomic_load_relaxed(&ptls2->gc_state) == 0 &&
                        ptls2->locks.len == 0) {
                        // consumes the expiry; NULL if the episode completed
                        // (or was reset) while we waited
                        rescue_task = jl_check_sigint_rescue_abandon();
                        break;
                    }
                    uv_sleep(1);
                }
            }
            if (rescue_task != NULL) {
                jl_task_t *ct = jl_atomic_load_relaxed(&ptls2->current_task);
                jl_value_t *bound = ct == NULL ? NULL :
                    jl_atomic_load_acquire(&ct->bound_cancel_token);
                jl_value_t *sigsrc = (jl_value_t*)jl_get_sigint_source();
                // See signals-unix.c: only abandon work governed by the ^C
                // source - an unbound task may be runtime infrastructure -
                // but the episode's registered foreground task qualifies
                // even without a binding (a never-blocked victim passed no
                // publishing cancellation point).
                int governed = bound != NULL && bound != jl_nothing &&
                    sigsrc != NULL && sigsrc != jl_nothing &&
                    jl_cancel_source_subtree_member(bound, sigsrc);
                if (!governed && ct != NULL && sigsrc != NULL && sigsrc != jl_nothing &&
                    ct == jl_get_sigint_foreground_task())
                    governed = 1;
                if (ct != NULL && ct != rescue_task && governed) {
                    // Announce BEFORE publishing (see signals-unix.c): the
                    // session cleanup that follows the switch can exit the
                    // process before a message printed afterwards makes it
                    // out.
                    jl_safe_printf("\nWARNING: Abandoning the current task and switching to a rescue task.\n"
                                     "         This may leave the process in an inconsistent state.\n");
                    // Publish the request and return: the verdict is polled
                    // at the next handler invocation (press or timer tick) -
                    // no waiting here (see the unix listener's state
                    // machine).
                    int reqtid = jl_abandon_task_request(ct, rescue_task, NULL, NULL);
                    if (reqtid >= 0)
                        direct_abandon_tid = (int16_t)reqtid;
                    else
                        jl_safe_printf("\nWARNING: Could not abandon the current task; still trying.\n");
                    return 1;
                }
            }
            // Request cancellation of the root task and notify the sigint
            // listener, which drives the cancellation state machine.
            jl_sigint_request_cancellation();
        }
        else {
            jl_try_deliver_sigint();
        }
    }
    return 1;
}

LONG WINAPI jl_exception_handler(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
    if (ExceptionInfo->ExceptionRecord->ExceptionFlags != 0)
        return EXCEPTION_CONTINUE_SEARCH;
    jl_task_t *ct = jl_get_current_task();
    if (ct != NULL && ct->ptls != NULL && ct->ptls->gc_state != JL_GC_STATE_WAITING) {
        jl_ptls_t ptls = ct->ptls;
        switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
        case EXCEPTION_INT_DIVIDE_BY_ZERO:
            if (ct->eh != NULL) {
                fpreset();
                jl_throw_in_ctx(ct, jl_diverror_exception, ExceptionInfo->ContextRecord);
                return EXCEPTION_CONTINUE_EXECUTION;
            }
            break;
        case EXCEPTION_STACK_OVERFLOW:
            if (ct->eh != NULL) {
                ptls->needs_resetstkoflw = 1;
                jl_throw_in_ctx(ct, jl_stackovf_exception, ExceptionInfo->ContextRecord);
                return EXCEPTION_CONTINUE_EXECUTION;
            }
            break;
        case EXCEPTION_ACCESS_VIOLATION:
            if (jl_addr_is_safepoint(ExceptionInfo->ExceptionRecord->ExceptionInformation[1])) {
                jl_set_gc_and_wait(ct);
                // Do not raise sigint on worker thread
                if (ptls->tid != 0)
                    return EXCEPTION_CONTINUE_EXECUTION;
                if (ptls->defer_signal) {
                    jl_safepoint_defer_sigint();
                }
                else if (jl_safepoint_consume_sigint()) {
                    jl_clear_force_sigint();
                    jl_throw_in_ctx(ct, jl_interrupt_exception, ExceptionInfo->ContextRecord);
                }
                return EXCEPTION_CONTINUE_EXECUTION;
            }
            if (jl_get_safe_restore()) {
                jl_throw_in_ctx(NULL, NULL, ExceptionInfo->ContextRecord);
                return EXCEPTION_CONTINUE_EXECUTION;
            }
            if (ct->eh != NULL) {
                if (ExceptionInfo->ExceptionRecord->ExceptionInformation[0] == 1) { // writing to read-only memory (e.g. mmap)
                    jl_throw_in_ctx(ct, jl_readonlymemory_exception, ExceptionInfo->ContextRecord);
                    return EXCEPTION_CONTINUE_EXECUTION;
                }
            }
        default:
            break;
        }
    }
    ios_t full_error, summary;
    ios_mem(&full_error, 0);
    if (ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ILLEGAL_INSTRUCTION) {
        jl_safe_fprintf(&full_error, "\n");
        jl_fprint_sigill(&full_error, ExceptionInfo->ContextRecord);
    }
    jl_safe_fprintf(&full_error, "\nPlease submit a bug report with steps to reproduce this fault, and any error messages that follow (in their entirety). Thanks.\n");
    ios_mem(&summary, 128);
    jl_safe_fprintf(&summary, "Exception: ");
    switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
    case EXCEPTION_ACCESS_VIOLATION:
        jl_safe_fprintf(&summary, "EXCEPTION_ACCESS_VIOLATION"); break;
    case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
        jl_safe_fprintf(&summary, "EXCEPTION_ARRAY_BOUNDS_EXCEEDED"); break;
    case EXCEPTION_BREAKPOINT:
        jl_safe_fprintf(&summary, "EXCEPTION_BREAKPOINT"); break;
    case EXCEPTION_DATATYPE_MISALIGNMENT:
        jl_safe_fprintf(&summary, "EXCEPTION_DATATYPE_MISALIGNMENT"); break;
    case EXCEPTION_FLT_DENORMAL_OPERAND:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_DENORMAL_OPERAND"); break;
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_DIVIDE_BY_ZERO"); break;
    case EXCEPTION_FLT_INEXACT_RESULT:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_INEXACT_RESULT"); break;
    case EXCEPTION_FLT_INVALID_OPERATION:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_INVALID_OPERATION"); break;
    case EXCEPTION_FLT_OVERFLOW:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_OVERFLOW"); break;
    case EXCEPTION_FLT_STACK_CHECK:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_STACK_CHECK"); break;
    case EXCEPTION_FLT_UNDERFLOW:
        jl_safe_fprintf(&summary, "EXCEPTION_FLT_UNDERFLOW"); break;
    case EXCEPTION_ILLEGAL_INSTRUCTION:
        jl_safe_fprintf(&summary, "EXCEPTION_ILLEGAL_INSTRUCTION"); break;
    case EXCEPTION_IN_PAGE_ERROR:
        jl_safe_fprintf(&summary, "EXCEPTION_IN_PAGE_ERROR"); break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
        jl_safe_fprintf(&summary, "EXCEPTION_INT_DIVIDE_BY_ZERO"); break;
    case EXCEPTION_INT_OVERFLOW:
        jl_safe_fprintf(&summary, "EXCEPTION_INT_OVERFLOW"); break;
    case EXCEPTION_INVALID_DISPOSITION:
        jl_safe_fprintf(&summary, "EXCEPTION_INVALID_DISPOSITION"); break;
    case EXCEPTION_NONCONTINUABLE_EXCEPTION:
        jl_safe_fprintf(&summary, "EXCEPTION_NONCONTINUABLE_EXCEPTION"); break;
    case EXCEPTION_PRIV_INSTRUCTION:
        jl_safe_fprintf(&summary, "EXCEPTION_PRIV_INSTRUCTION"); break;
    case EXCEPTION_SINGLE_STEP:
        jl_safe_fprintf(&summary, "EXCEPTION_SINGLE_STEP"); break;
    case EXCEPTION_STACK_OVERFLOW:
        jl_safe_fprintf(&summary, "EXCEPTION_STACK_OVERFLOW"); break;
    default:
        jl_safe_fprintf(&summary, "UNKNOWN"); break;
    }
    jl_safe_fprintf(&summary, " at 0x%zx", (size_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);
    if (ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION ||
        ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_IN_PAGE_ERROR) {
        jl_safe_fprintf(&summary, " (%s 0x%zx)",
                        ExceptionInfo->ExceptionRecord->ExceptionInformation[0] == 1 ? "writing" :
                        ExceptionInfo->ExceptionRecord->ExceptionInformation[0] == 8 ? "executing" : "reading",
                        (size_t)ExceptionInfo->ExceptionRecord->ExceptionInformation[1]);
    }
    jl_safe_fprintf(&summary, " -- ");
    jl_fprint_native_codeloc(&summary, (uintptr_t)ExceptionInfo->ExceptionRecord->ExceptionAddress);
    // runtime state that distinguishes crash classes (e.g. a NULL task->ptls
    // from a corrupted pointer) without needing a debugger on the machine
    jl_safe_fprintf(&summary, "current task: 0x%zx (ptls 0x%zx, eh 0x%zx)\n", (size_t)ct,
                    (size_t)(ct == NULL ? NULL : (void*)ct->ptls),
                    (size_t)(ct == NULL ? NULL : (void*)ct->eh));
    ios_write(&full_error, summary.buf, ios_pos(&summary));
    ios_puts("\nSee Application log in Event Viewer for more information.\n", &summary);

    jl_fprint_critical_error(&full_error, 0, 0, ExceptionInfo->ContextRecord, ct);

    // First print to STDERR
    ios_write_direct(ios_safe_stderr, &full_error);

    // Secondly print to Application log
    HANDLE event_source = RegisterEventSourceW(NULL, L"julia");
    if (event_source != INVALID_HANDLE_VALUE) {
        ios_putc('\0', &full_error);
        const wchar_t *strings[] = { ios_utf8_to_wchar(full_error.buf) };
        ReportEventW(
            event_source, EVENTLOG_ERROR_TYPE, /* category */ 0, /* event_id */ (DWORD)0xE0000000L,
           /* user_sid */ NULL, /* n_strings */ 1, /* data_size */ 0, strings, /* data */ NULL
        );
        free((void *)strings[0]);

        if (jl_options.alert_on_critical_error) {
            ios_putc('\0', &summary);
            const wchar_t *message = ios_utf8_to_wchar(summary.buf);
            MessageBoxW(NULL, message, /* title */ L"fatal error in libjulia",
                        MB_OK | MB_ICONEXCLAMATION | MB_SYSTEMMODAL);
            free((void *)message);
        }
    }

    ios_close(&summary);
    ios_close(&full_error);
    static int recursion = 0;
    if (recursion++)
        exit(1);
    else
        jl_exit(1);
}

JL_DLLEXPORT void jl_install_sigint_handler(void)
{
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)sigint_handler,1);
}

static TIMECAPS timecaps;
static HANDLE hBtThread = 0;
static uv_cond_t bt_data_prof_cond = CONDITION_VARIABLE_INIT;

#ifdef _CPU_X86_64_
// Callback data structure for profile timeout
typedef struct {
    _Atomic(int) *abort_ptr;
    int tid;
} profile_timeout_data_t;

static void CALLBACK profile_timeout_cb(PVOID lpParam, BOOLEAN TimerOrWaitFired)
{
    profile_timeout_data_t *data = (profile_timeout_data_t*)lpParam;
    if (TimerOrWaitFired && data != NULL && data->abort_ptr != NULL) {
        // Timeout reached, signal an abort should occur
        if (jl_atomic_exchange(data->abort_ptr, 2) == 1) {
            jl_thread_resume(data->tid);
            data->tid = -1;
        }
    }
}
#endif

static int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx)
{
    (void)timeout;
    if (tid < 0 || tid >= jl_atomic_load_acquire(&jl_n_threads))
        return 0;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL) // this thread is not alive
        return 0;
    jl_task_t *ct2 = jl_atomic_load_relaxed(&ptls2->current_task);
    if (ct2 == NULL) // this thread is already dead
        return 0;
    HANDLE hThread = ptls2->system_id;
    assert(GetCurrentThreadId() != GetThreadId(hThread));
    if ((DWORD)-1 == SuspendThread(hThread)) {
        // jl_safe_fprintf(ios_safe_stderr, "failed to suspend thread %d: %lu\n", tid, GetLastError());
        return 0;
    }
    assert(sizeof(*ctx) == sizeof(CONTEXT));
    memset(ctx, 0, sizeof(CONTEXT));
    ctx->ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_FLOATING_POINT;
    if (!GetThreadContext(hThread, ctx)) {
        if ((DWORD)-1 == ResumeThread(hThread))
            abort();
        return 0;
    }
    return 1;
}

void jl_thread_resume(int tid)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    HANDLE hThread = ptls2->system_id;
    if ((DWORD)-1 == ResumeThread(hThread)) {
        jl_safe_fprintf(ios_safe_stderr, "failed to resume main thread! aborting.\n");
        abort();
    }
}

int jl_thread_suspend(int16_t tid, bt_context_t *ctx)
{
    jl_lock_profile(); // prevent concurrent mutation
    uv_mutex_lock(&jl_in_stackwalk); // prevent multi-threaded dbghelp calls
    uv_mutex_lock(&jl_dll_notify_lock);
    jl_profile_process_dll_events();
    int success = jl_thread_suspend_and_get_state(tid, 0, ctx);
    uv_mutex_unlock(&jl_dll_notify_lock);
    uv_mutex_unlock(&jl_in_stackwalk);
    jl_unlock_profile();
    return success;
}

static DWORD WINAPI profile_bt( LPVOID lparam )
{
    // Note: illegal to use jl_* functions from this thread except for profiling-specific functions
    HANDLE hTimerQueue = CreateTimerQueue();
    if (hTimerQueue == NULL) {
        jl_safe_fprintf(ios_safe_stderr, "failed to create profile watchdog timer queue.\n");
        abort();
    }
    while (1) {
        DWORD timeout_ms = nsecprof / (GIGA / 1000);
        Sleep(timeout_ms > 0 ? timeout_ms : 1);
        if (jl_profile_is_buffer_full())
            jl_profile_stop_timer(); // does not change the thread state
        if (!profile_running) {
            uv_mutex_lock(&bt_data_prof_lock);
            while (!profile_running)
                uv_cond_wait(&bt_data_prof_cond, &bt_data_prof_lock);
            uv_mutex_unlock(&bt_data_prof_lock);
        }
        else if (profile_all_tasks) {
            // Don't take the stackwalk lock here since it's already taken in `jl_rec_backtrace`
            jl_profile_task();
        }
        else {
            // Profile all threads, similar to Unix implementation
            bt_context_t c;
            int nthreads = jl_atomic_load_acquire(&jl_n_threads);
            int *randperm = profile_get_randperm(nthreads);
            for (int idx = nthreads; idx-- > 0; ) {
                int tid = randperm[idx];
                if (!profile_running)
                    break;
                if (jl_profile_is_buffer_full()) {
                    jl_profile_stop_timer();
                    break;
                }

                // Set up timeout handler for stackwalk
#ifdef _CPU_X86_64_
                _Atomic(int) abort_profiling = 0;
                profile_timeout_data_t timeout_data;
                timeout_data.abort_ptr = &abort_profiling;
                timeout_data.tid = tid;
                jl_set_profile_abort_ptr(&abort_profiling);
                HANDLE hTimer = NULL;
                if (!CreateTimerQueueTimer(&hTimer, hTimerQueue, profile_timeout_cb,
                                           &timeout_data, 1000 /* milliseconds */, 0,
                                           WT_EXECUTEONLYONCE | WT_EXECUTEINWAITTHREAD)) {
                    // Failed to register wait, proceed without timeout protection
                    hTimer = NULL;
                }
#endif

                if (!jl_thread_suspend(tid, &c))
                    continue;

                jl_ptls_t ptls = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
                jl_task_t *t2 = jl_atomic_load_relaxed(&ptls->current_task);
                int state = jl_atomic_load_relaxed(&ptls->sleep_check_state) == 0 ? PROFILE_STATE_THREAD_NOT_SLEEPING : PROFILE_STATE_THREAD_SLEEPING;

                // Get backtrace data
                profile_bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)profile_bt_data_prof + profile_bt_size_cur,
                        profile_bt_size_max - profile_bt_size_cur - 1, &c, NULL);

#ifdef _CPU_X86_64_
                // Clear abort pointer from TLS
                jl_set_profile_abort_ptr(NULL);
                if (timeout_data.tid != -1)
                    jl_thread_resume(tid);
                // Wait for callback to complete or cancel before continuing
                if (hTimer != NULL)
                    DeleteTimerQueueTimer(hTimerQueue, hTimer, INVALID_HANDLE_VALUE);
#else
                jl_thread_resume(tid);
#endif

                // META_OFFSET_THREADID store threadid but add 1 as 0 is preserved to indicate end of block
                profile_bt_data_prof[profile_bt_size_cur++].uintptr = tid + 1;

                // META_OFFSET_TASKID store task id (never null)
                profile_bt_data_prof[profile_bt_size_cur++].jlvalue = (jl_value_t*)t2;

                // META_OFFSET_CPUCYCLECLOCK store cpu cycle clock
                profile_bt_data_prof[profile_bt_size_cur++].uintptr = cycleclock();

                // store whether thread is sleeping (don't ever encode a state as `0` since is preserved to indicate end of block)
                profile_bt_data_prof[profile_bt_size_cur++].uintptr = state;

                // Mark the end of this block with two 0's
                profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;
                profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;
            }
            jl_check_profile_autostop();
        }
    }
    // this is unreachable, but would be the relevant cleanup
    uv_mutex_lock(&bt_data_prof_lock);
    hBtThread = NULL;
    uv_mutex_unlock(&bt_data_prof_lock);
    jl_profile_stop_timer();
    DeleteTimerQueue(hTimerQueue);
    return 0;
}

JL_DLLEXPORT int jl_profile_start_timer(uint8_t all_tasks)
{
    uv_mutex_lock(&bt_data_prof_lock);
    if (hBtThread == NULL) {
        TIMECAPS _timecaps;
        if (MMSYSERR_NOERROR != timeGetDevCaps(&_timecaps, sizeof(_timecaps))) {
            uv_mutex_unlock(&bt_data_prof_lock);
            jl_safe_fprintf(ios_safe_stderr, "failed to get timer resolution.\n");
            return -2;
        }
        timecaps = _timecaps;

        hBtThread = CreateThread(
            NULL,                   // default security attributes
            0,                      // use default stack size
            profile_bt,             // thread function name
            0,                      // argument to thread function
            0,                      // use default creation flags
            0);                     // returns the thread identifier
        if (hBtThread == NULL) {
            uv_mutex_unlock(&bt_data_prof_lock);
            jl_safe_fprintf(ios_safe_stderr, "failed to allocate profile thread.\n");
            return -1;
        }
        (void)SetThreadPriority(hBtThread, THREAD_PRIORITY_ABOVE_NORMAL);
    }
    if (profile_running == 0) {
        // Failure to change the timer resolution is not fatal. However, it is important to
        // ensure that the timeBeginPeriod/timeEndPeriod is paired.
        if (TIMERR_NOERROR != timeBeginPeriod(timecaps.wPeriodMin))
            timecaps.wPeriodMin = 0;
    }
    profile_all_tasks = all_tasks;
    profile_running = 1; // set `profile_running` finally
    uv_cond_broadcast(&bt_data_prof_cond);
    uv_mutex_unlock(&bt_data_prof_lock);
    return 0;
}
JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    uv_mutex_lock(&bt_data_prof_lock);
    if (profile_running && timecaps.wPeriodMin)
        timeEndPeriod(timecaps.wPeriodMin);
    profile_running = 0;
    profile_all_tasks = 0;
    uv_mutex_unlock(&bt_data_prof_lock);
}

void jl_install_default_signal_handlers(void)
{
    if (signal(SIGFPE, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGFPE");
    }
    if (signal(SIGILL, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGILL");
    }
    if (signal(SIGINT, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGINT");
    }
    if (signal(SIGSEGV, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGSEGV");
    }
    if (signal(SIGTERM, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGTERM");
    }
    if (signal(SIGABRT, (void (__cdecl *)(int))crt_sig_handler) == SIG_ERR) {
        jl_error("fatal error: Couldn't set SIGABRT");
    }
    SetUnhandledExceptionFilter(jl_exception_handler);
    jl_win_init_cancel_handler_delivery();
}

void jl_install_thread_signal_handler(jl_ptls_t ptls)
{
    if (!have_backtrace_fiber) {
        size_t ssize = sig_stack_size;
        void *stk = jl_malloc_stack(&ssize, NULL);
        if (stk == NULL)
            jl_errorf("fatal error allocating signal stack: mmap: %s", strerror(errno));
        collect_backtrace_fiber.uc_stack.ss_sp = (void*)stk;
        collect_backtrace_fiber.uc_stack.ss_size = ssize;
        jl_makecontext(&collect_backtrace_fiber, start_backtrace_fiber);
        uv_mutex_init(&backtrace_lock);
        have_backtrace_fiber = 1;
    }
}

JL_DLLEXPORT void jl_membarrier(void) {
    FlushProcessWriteBuffers();
}
