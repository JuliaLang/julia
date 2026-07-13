// This file is a part of Julia. License is MIT: https://julialang.org/license

// Note that this file is `#include`d by "signal-handling.c"

#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <pthread.h>
#include <time.h>
#include <errno.h>

#include "julia.h"
#include "julia_internal.h"

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

// sigwaitinfo (and the siginfo_t it fills) lets the signal listener
// distinguish timer-raised signals from user-sent ones (SI_TIMER + sigev
// value). glibc advertises it via _POSIX_C_SOURCE (defined through
// _GNU_SOURCE); FreeBSD supports it without defining that macro, and
// without the discrimination a rescue-timer SIGINT is indistinguishable
// from a user press (breaking the whole ^C escalation ladder there).
#if (defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L) || defined(__FreeBSD__)
#define HAVE_SIGWAITINFO
#endif

// 8M signal stack, same as default stack size (though we barely use this)
static const size_t sig_stack_size = 8 * 1024 * 1024;

#include "julia_assert.h"

// helper function for returning the unw_context_t inside a ucontext_t
static bt_context_t *jl_to_bt_context(void *sigctx) JL_NOTSAFEPOINT
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
static void jl_longjmp_in_ctx(int sig, void *_ctx, jl_jmp_buf jmpbuf);

#if !defined(_OS_DARWIN_)
extern void jl_fake_signal_return(void);
// Create a trampoline function that does the stack manipulations for jl_call_in_ctx/jl_call_in_state
// The callee-saved registers still may get smashed (by the cdecl fptr), since we didn't explicitly copy all of the
// state to the stack (to build a real sigreturn frame).
#if (defined(_OS_LINUX_) || defined(_OS_FREEBSD_) || defined(_OS_OPENBSD_)) && defined(_CPU_X86_64_)
__asm__(
    "  .type jl_fake_signal_return, @function\n"
    "jl_fake_signal_return:\n"
    "  .cfi_startproc\n"
    "  .cfi_signal_frame\n"
    // Mark as end of stack until frame is set up
    "  .cfi_undefined %rip\n"
    "  .cfi_undefined %rsp\n"
    // rdi points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg)
    "  pushq (%rdi)\n"        // push pc (signal_ctx_pc)
    "  pushq 8(%rdi)\n"       // push sp (signal_ctx_sp)
    // stack layout: [sp, pc] (pc at higher address, like return address after call)
    "  .cfi_def_cfa %rsp, 8\n"
    "  .cfi_offset %rip, 0\n"  // previous %rip at CFA+0 (pc slot at rsp+8)
    "  .cfi_offset %rsp, -8\n" // previous %rsp at CFA-8 (sp slot at rsp+0)
    "  pushq 16(%rdi)\n"      // push fptr (signal_ctx_fptr)
    "  .cfi_def_cfa %rsp, 16\n"
    "  movq 24(%rdi), %rdi\n" // restore original rdi from signal_ctx_arg
    "  subq $8, %rsp\n"       // align stack to 16 bytes
    "  .cfi_def_cfa %rsp, 24\n"
    "  callq *8(%rsp)\n"      // call fptr
    "  ud2\n"                 // unreachable
    "  .cfi_endproc\n"
    "  .size jl_fake_signal_return, .-jl_fake_signal_return\n"
);

#elif (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_X86_)
__asm__(
    "  .type jl_fake_signal_return, @function\n"
    "jl_fake_signal_return:\n"
    "  .cfi_startproc\n"
    "  .cfi_signal_frame\n"
    // Mark as end of stack until frame is set up
    "  .cfi_undefined 1\n"
    // eax points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg)
    "  pushl (%eax)\n"        // push pc (signal_ctx_pc)
    "  pushl 4(%eax)\n"       // push sp (signal_ctx_sp)
    // stack layout: [sp, pc] (pc at higher address, like return address after call)
    "  .cfi_def_cfa %esp, 4\n"
    "  .cfi_offset %eip, 0\n"  // previous %eip at CFA+0 (pc slot at esp+4)
    "  .cfi_offset %esp, -4\n" // previous %esp at CFA-4 (sp slot at esp+0)
    "  pushl 8(%eax)\n"       // push fptr (signal_ctx_fptr)
    "  .cfi_def_cfa %esp, 8\n"
    "  movl 12(%eax), %eax\n" // restore original eax from signal_ctx_arg
    "  subl $4, %esp\n"       // align stack to 16 bytes
    "  .cfi_def_cfa %esp, 12\n"
    "  calll *4(%esp)\n"      // call fptr
    "  ud2\n"                 // unreachable
    "  .cfi_endproc\n"
    "  .size jl_fake_signal_return, .-jl_fake_signal_return\n"
);
#elif (defined(_OS_LINUX_) || defined(_OS_FREEBSD_)) && defined(_CPU_AARCH64_)
__asm__(
    "  .type jl_fake_signal_return, @function\n"
    "jl_fake_signal_return:\n"
    "  .cfi_startproc\n"
    "  .cfi_signal_frame\n"
    // Mark as end of stack until frame is set up
    "  .cfi_undefined 1\n"
    // x0 points to signal_ctx_pc in ptls (followed by signal_ctx_sp, signal_ctx_fptr, signal_ctx_arg)
    "  ldp x1, x2, [x0]\n"      // load pc (x1) and sp (x2)
    "  stp x2, x1, [sp, #-16]!\n" // push sp and pc (sp at lower addr, pc at higher addr)
    // stack layout: [sp, pc] (pc at higher address, like return address after call)
    "  .cfi_def_cfa sp, 16\n"
    "  .cfi_offset lr, -8\n"   // previous lr (pc) at CFA-8 (pc slot at sp+8)
    "  .cfi_offset sp, -16\n"  // previous sp at CFA-16 (sp slot at sp+0)
    // This is not quite valid, since the AArch64 DWARF spec lacks the ability to define how to restore the LR register correctly,
    // so normally libunwind implementations on linux detect this function specially and hack around the invalid info:
    // https://github.com/llvm/llvm-project/commit/c82deed6764cbc63966374baf9721331901ca958
    "  ldp x1, x2, [x0, #16]\n" // load fptr (x1) and saved x0 (x2)
    "  mov x0, x2\n"           // restore original x0
    "  blr x1\n"               // call fptr
    "  brk #1\n"               // unreachable
    "  .cfi_endproc\n"
    "  .size jl_fake_signal_return, .-jl_fake_signal_return\n"
);
#else
extern void JL_NORETURN jl_fake_signal_return(void)
{
    CFI_NORETURN
    abort();
}
#endif

static inline uintptr_t jl_get_rsp_from_ctx(const void *_ctx)
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
#elif defined(_OS_LINUX_) && (defined(_CPU_RISCV64_))
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.__gregs[REG_SP];
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_64_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.mc_rsp;
#elif defined(_OS_FREEBSD_) && defined(_CPU_AARCH64_)
    const ucontext_t *ctx = (const ucontext_t*)_ctx;
    return ctx->uc_mcontext.mc_gpregs.gp_sp;
#elif defined(_OS_OPENBSD_) && defined(_CPU_X86_64_)
    const struct sigcontext *ctx = (const struct sigcontext *)_ctx;
    return ctx->sc_rsp;
#else
    // TODO Add support for PowerPC(64)?
    return 0;
#endif
}

static int is_addr_on_sigstack(jl_ptls_t ptls, void *ptr) JL_NOTSAFEPOINT
{
    // One guard page for signal_stack.
    return ptls->signal_stack == NULL ||
           ((char*)ptr >= (char*)ptls->signal_stack - jl_page_size &&
            (char*)ptr <= (char*)ptls->signal_stack + (ptls->signal_stack_size ? ptls->signal_stack_size : sig_stack_size));
}

// Modify signal context `_ctx` so that `fptr` will execute when the signal returns
// The function `fptr` itself must not return.
JL_NO_ASAN static void jl_call_in_ctx(jl_ptls_t ptls, void (*fptr)(void) JL_CANSAFEPOINT, int sig, void *_ctx)
{
    // Modifying the ucontext should work but there is concern that
    // sigreturn oriented programming mitigation can work against us
    // by rejecting ucontext that is modified.
    // The current (staged) implementation in the Linux Kernel only
    // checks that the syscall is made in the signal handler and that
    // the ucontext address is valid. Hopefully the value of the ucontext
    // will not be part of the validation...
    uintptr_t rsp = jl_get_rsp_from_ctx(_ctx);
    rsp = (rsp - 256) & ~(uintptr_t)15; // redzone and re-alignment
    assert(rsp % 16 == 0);
#if defined(_OS_LINUX_) && defined(_CPU_X86_64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = ctx->uc_mcontext.gregs[REG_RIP];
    ptls->signal_ctx_sp = ctx->uc_mcontext.gregs[REG_RSP];
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->uc_mcontext.gregs[REG_RDI];
    ctx->uc_mcontext.gregs[REG_RSP] = rsp; // set stack pointer
    ctx->uc_mcontext.gregs[REG_RDI] = (uintptr_t)&ptls->signal_ctx_pc; // first arg points to signal_ctx
    ctx->uc_mcontext.gregs[REG_RIP] = (uintptr_t)&jl_fake_signal_return; // "call" jl_fake_signal_return
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = ctx->uc_mcontext.mc_rip;
    ptls->signal_ctx_sp = ctx->uc_mcontext.mc_rsp;
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->uc_mcontext.mc_rdi;
    ctx->uc_mcontext.mc_rsp = rsp; // set stack pointer
    ctx->uc_mcontext.mc_rdi = (uintptr_t)&ptls->signal_ctx_pc; // first arg points to signal_ctx
    ctx->uc_mcontext.mc_rip = (uintptr_t)&jl_fake_signal_return; // "call" jl_fake_signal_return
#elif defined(_OS_LINUX_) && defined(_CPU_X86_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = ctx->uc_mcontext.gregs[REG_EIP];
    ptls->signal_ctx_sp = ctx->uc_mcontext.gregs[REG_ESP];
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->uc_mcontext.gregs[REG_EAX];
    ctx->uc_mcontext.gregs[REG_ESP] = rsp; // set stack pointer
    ctx->uc_mcontext.gregs[REG_EAX] = (uintptr_t)&ptls->signal_ctx_pc; // set eax to point to signal_ctx
    ctx->uc_mcontext.gregs[REG_EIP] = (uintptr_t)&jl_fake_signal_return; // "call" jl_fake_signal_return
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = ctx->uc_mcontext.mc_eip;
    ptls->signal_ctx_sp = ctx->uc_mcontext.mc_esp;
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->uc_mcontext.mc_eax;
    ctx->uc_mcontext.mc_esp = rsp; // set stack pointer
    ctx->uc_mcontext.mc_eax = (uintptr_t)&ptls->signal_ctx_pc; // set eax to point to signal_ctx
    ctx->uc_mcontext.mc_eip = (uintptr_t)&jl_fake_signal_return; // "call" jl_fake_signal_return
#elif defined(_OS_OPENBSD_) && defined(_CPU_X86_64_)
    struct sigcontext *ctx = (struct sigcontext *)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = ctx->sc_rip;
    ptls->signal_ctx_sp = ctx->sc_rsp;
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->sc_rdi;
    ctx->sc_rsp = rsp; // set stack pointer
    ctx->sc_rdi = (uintptr_t)&ptls->signal_ctx_pc; // first arg points to signal_ctx
    ctx->sc_rip = (uintptr_t)&jl_fake_signal_return; // "call" jl_fake_signal_return
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = (uintptr_t)ctx->uc_mcontext.pc;
    ptls->signal_ctx_sp = ctx->uc_mcontext.sp;
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->uc_mcontext.regs[0];
    ctx->uc_mcontext.sp = rsp; // sp
    ctx->uc_mcontext.regs[0] = (uintptr_t)&ptls->signal_ctx_pc; // first arg points to signal_ctx
    ctx->uc_mcontext.pc = (uint64_t)&jl_fake_signal_return; // pc
    ctx->uc_mcontext.regs[30] = 0; // clear lr (x30)
#elif defined(_OS_FREEBSD_) && defined(_CPU_AARCH64_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    // Save context in ptls for stack unwinding
    ptls->signal_ctx_pc = ctx->uc_mcontext.mc_gpregs.gp_elr;
    ptls->signal_ctx_sp = ctx->uc_mcontext.mc_gpregs.gp_sp;
    ptls->signal_ctx_fptr = fptr;
    ptls->signal_ctx_arg = ctx->uc_mcontext.mc_gpregs.gp_x[0];
    ctx->uc_mcontext.mc_gpregs.gp_sp = rsp; // set stack pointer
    ctx->uc_mcontext.mc_gpregs.gp_x[0] = (uintptr_t)&ptls->signal_ctx_pc; // first arg points to signal_ctx
    ctx->uc_mcontext.mc_gpregs.gp_elr = (uintptr_t)&jl_fake_signal_return; // pc
    ctx->uc_mcontext.mc_gpregs.gp_lr = 0; // clear lr (x30)
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
#elif defined(_OS_LINUX_) && (defined(_CPU_RISCV64_))
    ucontext_t *ctx = (ucontext_t*)_ctx;
    ctx->uc_mcontext.__gregs[REG_SP] = rsp;
    ctx->uc_mcontext.__gregs[REG_RA] = 0; // Clear return address address (ra)
    ctx->uc_mcontext.__gregs[REG_PC] = (uintptr_t)fptr;
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
#endif

static void jl_throw_in_ctx(jl_task_t *ct, jl_value_t *e, int sig, void *sigctx)
{
    jl_ptls_t ptls = ct->ptls;
    assert(!jl_get_safe_restore());
    ptls->bt_size =
        rec_backtrace_ctx(ptls->bt_data, JL_MAX_BT_SIZE, jl_to_bt_context(sigctx),
                            ct->gcstack);
    ptls->sig_exception = e;
    ptls->io_wait = 0;
    jl_handler_t *eh = ct->eh;
    if (eh != NULL) {
        asan_unpoison_task_stack(ct, &eh->eh_ctx);
        jl_longjmp_in_ctx(sig, sigctx, eh->eh_ctx);
    }
    else {
        jl_no_exc_handler(e, ct);
    }
}

// === Cancellation-handler delivery ==========================================
// A foreign call annotated with a cancellation handler publishes an sp == 0
// jl_reset_ctx_t in task->cancel_handler_ctx (see emit_ccall) - a slot
// separate from a compiled reset region's task->reset_ctx, so both can be
// active at once (the handler takes delivery priority while published).
// Delivering the cancellation signal to such a region runs the handler *on
// the interrupted thread*, like a signal handler, and then resumes the
// interrupted computation:
//  1. The signal handler copies the interrupted general-purpose registers
//     from the signal context into ptls->cancel_handler_save and redirects
//     the context to jl_cancel_handler_shim, dropping sp just past the
//     interrupted frame's red zone but touching no other register. Nothing
//     is written to the stack: the memory below the red zone holds only
//     this signal's own frame, which is dead the moment sigreturn launches
//     the shim.
//  2. The shim - running in ordinary, unmasked context once the signal
//     handler returns - calls jl_cancel_handler_trampoline, which invokes
//     fn(state, severity) from the save area. GPRs need no care anywhere on
//     this path: the resume below replays every one of them. FP/vector
//     state is the *handler's* responsibility: its contract is the LLVM
//     `preserve_all` calling convention (clang
//     `__attribute__((preserve_all))`, and reachable from Julia's own LLVM
//     pipeline), i.e. it generates whatever stack saves it needs - a
//     handler that touches no FP/vector registers satisfies this trivially.
//     The runtime pieces on this path are compiled general-regs-only so
//     they cannot clobber FP state themselves.
//  3. The shim then calls jl_cancel_handler_resume, which arbitrates the
//     shared signal_request slot (request 7) and re-raises the signal; the
//     restore branch copies the saved GPR state back over its own signal
//     context, so that returning from it resumes the originally interrupted
//     instruction (the self-signal's frame round-trips the still-live
//     interrupted FP state through the kernel untouched).
// The save area holds at most one delivery per thread: while one is in
// flight (armed), further deliveries are skipped and recovered
// level-triggered. The region context stays published throughout, so an
// escalation or redelivery runs the (idempotent) handler again once the
// current delivery completes.

// (On Darwin this file's delivery machinery is replaced by the mach-based
// implementation in signals-mach.c, which rides the resumable
// jl_call_in_state + restore-trigger machinery instead of a self-signal.)
#if defined(JL_HAVE_CANCEL_HANDLER_DELIVERY) && defined(_OS_LINUX_)

extern void jl_cancel_handler_shim(void);
void jl_cancel_handler_trampoline(void);
JL_NORETURN void jl_cancel_handler_resume(void);

#if defined(_CPU_X86_64_)
_Static_assert(sizeof(((mcontext_t*)0)->gregs) <= sizeof(((jl_cancel_handler_save_t*)0)->gregs),
               "jl_cancel_handler_save_t.gregs must hold mcontext_t.gregs");
__asm__(
    "  .type jl_cancel_handler_shim, @function\n"
    "jl_cancel_handler_shim:\n"
    "  .cfi_startproc\n"
    "  .cfi_signal_frame\n"
    // Mark as end of stack; the interrupted pc/sp live in the per-thread
    // save area, not anywhere an unwinder could recover them.
    "  .cfi_undefined %rip\n"
    "  .cfi_undefined %rsp\n"
    // Entered with sp just past the interrupted frame's red zone (16-byte
    // aligned) and every other register still holding the interrupted
    // computation's value. GPRs are free to clobber (the resume replays
    // them from the save area); FP/vector state is preserved by the
    // handler's preserve_all contract.
    "  cld\n" // C ABI needs DF clear; the interrupted flags are replayed on resume
    "  callq jl_cancel_handler_trampoline@PLT\n"
    "  callq jl_cancel_handler_resume@PLT\n" // does not return
    "  ud2\n"
    "  .cfi_endproc\n"
    "  .size jl_cancel_handler_shim, .-jl_cancel_handler_shim\n"
);
#elif defined(_CPU_AARCH64_)
__asm__(
    "  .type jl_cancel_handler_shim, @function\n"
    "jl_cancel_handler_shim:\n"
    "  .cfi_startproc\n"
    "  .cfi_signal_frame\n"
    "  .cfi_undefined 30\n" // end of stack (lr was cleared by the deliverer)
    // Entered with sp at the (16-byte aligned) interrupted sp - no red zone
    // on aarch64 - and every other register still holding the interrupted
    // computation's value. GPRs are free to clobber (the resume replays
    // them); FP/vector state is preserved by the handler's preserve_all
    // contract.
    "  bl jl_cancel_handler_trampoline\n"
    "  bl jl_cancel_handler_resume\n" // does not return
    "  brk #1\n"
    "  .cfi_endproc\n"
    "  .size jl_cancel_handler_shim, .-jl_cancel_handler_shim\n"
);
#endif

// Save the interrupted GPR state into the per-thread save area and rewrite
// the (suspended) signal context so that sigreturn runs fn(state, sev) on
// the interrupted thread. Only sp and pc are redirected.
JL_NO_ASAN static void jl_deliver_cancel_handler(jl_ptls_t ptls, jl_task_t *ct, jl_reset_ctx_t *rctx, void *_ctx) JL_NOTSAFEPOINT
{
    // At most one delivery in flight per thread (its state occupies the one
    // save area); skips are recovered level-triggered.
    if (ptls->cancel_handler_armed)
        return;
    jl_cancel_handler_save_t *save = &ptls->cancel_handler_save;
    // Severity: the state of the task's bound token source (relaxed; a
    // spurious delivery passes whatever is current - handlers tolerate it).
    uint8_t sev = 0;
    jl_value_t *bound = jl_atomic_load_relaxed(&ct->bound_cancel_token);
    if (bound != NULL && bound != jl_nothing)
        sev = jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state) & 0x3f;
    ucontext_t *ctx = (ucontext_t*)_ctx;
    save->fn = rctx->handler.fn;
    save->state = rctx->handler.state;
    save->sev = sev;
#if defined(_CPU_X86_64_)
    memcpy(save->gregs, ctx->uc_mcontext.gregs, sizeof(ctx->uc_mcontext.gregs));
    ptls->cancel_handler_armed = 1;
    // Drop sp past the interrupted frame's red zone (the signal frame below
    // it is dead once sigreturn fires) and redirect to the shim; at the
    // shim's `call`, sp is 16-aligned as the C ABI requires.
    uintptr_t sp = ((uintptr_t)ctx->uc_mcontext.gregs[REG_RSP] - 128) & ~(uintptr_t)15;
    ctx->uc_mcontext.gregs[REG_RSP] = (greg_t)sp;
    ctx->uc_mcontext.gregs[REG_RIP] = (greg_t)(uintptr_t)&jl_cancel_handler_shim;
#elif defined(_CPU_AARCH64_)
    memcpy(save->regs, ctx->uc_mcontext.regs, sizeof(save->regs));
    save->sp = ctx->uc_mcontext.sp;
    save->pc = ctx->uc_mcontext.pc;
    save->pstate = ctx->uc_mcontext.pstate;
    ptls->cancel_handler_armed = 1;
    // No red zone on aarch64: the interrupted sp (16-aligned per the ABI) is
    // directly usable; the dead signal frame lies below it.
    ctx->uc_mcontext.sp = ctx->uc_mcontext.sp & ~(uintptr_t)15;
    ctx->uc_mcontext.regs[30] = 0; // clear lr: stop unwinding at the shim
    ctx->uc_mcontext.pc = (uintptr_t)&jl_cancel_handler_shim;
#endif
}

// Copy the saved GPR state back over this signal's own context, so that
// sigreturn resumes the originally interrupted instruction. (The shim
// already restored the FP state before raising this signal.)
JL_NO_ASAN static void jl_cancel_handler_restore(jl_ptls_t ptls, void *_ctx) JL_NOTSAFEPOINT
{
    if (!ptls->cancel_handler_armed)
        return;
    jl_cancel_handler_save_t *save = &ptls->cancel_handler_save;
    ucontext_t *ctx = (ucontext_t*)_ctx;
#if defined(_CPU_X86_64_)
    memcpy(ctx->uc_mcontext.gregs, save->gregs, sizeof(ctx->uc_mcontext.gregs));
#elif defined(_CPU_AARCH64_)
    memcpy(ctx->uc_mcontext.regs, save->regs, sizeof(save->regs));
    ctx->uc_mcontext.sp = save->sp;
    ctx->uc_mcontext.pc = save->pc;
    ctx->uc_mcontext.pstate = save->pstate;
#endif
    ptls->cancel_handler_armed = 0;
}

// Between the (still-FP-live) interrupted context and the final sigreturn,
// the runtime's own code must not clobber FP/vector registers: the kernel
// captures the live FP state into the self-signal's frame and sigreturn
// replays it verbatim. JL_GENERAL_REGS_ONLY (support/platform.h) has the
// compiler enforce that.

// Called by the shim: invoke the registered handler with its arguments from
// the save area. The handler itself preserves all register state it touches
// (the preserve_all contract).
JL_GENERAL_REGS_ONLY void jl_cancel_handler_trampoline(void)
{
    jl_ptls_t ptls = jl_get_current_task()->ptls;
    jl_cancel_handler_save_t *save = &ptls->cancel_handler_save;
    save->fn(save->state, save->sev);
}

// Called by the shim after the handler returned: hand the saved GPR state
// to the restore branch (request 7) via a self-signal. Free to clobber any
// GPR (all of them are replayed).
JL_GENERAL_REGS_ONLY JL_NORETURN JL_NO_ASAN void jl_cancel_handler_resume(void)
{
    jl_ptls_t ptls = jl_get_current_task()->ptls;
    // Arbitrate the shared signal_request slot: a concurrent suspend/abandon
    // request holds it transiently, and its delivery interrupts this spin
    // (we run with the normal signal mask) and releases the slot.
    for (;;) {
        sig_atomic_t expected = 0;
        if (jl_atomic_cmpswap(&ptls->signal_request, &expected, 7))
            break;
        // jl_cpu_pause() is spelled out: _mm_pause is an SSE-header intrinsic
        // that general-regs-only rejects (the instruction itself is fine).
#if defined(_CPU_X86_64_)
        __asm__ volatile ("pause" ::: "memory");
#elif defined(_CPU_AARCH64_)
        __asm__ volatile ("isb" ::: "memory");
#endif
    }
    pthread_kill(pthread_self(), SIGUSR2);
    // The restore branch rewrites the self-signal's context to the saved
    // state, so its handler returns straight to the originally interrupted
    // instruction; control never reaches this point.
    abort();
}
#endif // JL_HAVE_CANCEL_HANDLER_DELIVERY && _OS_LINUX_

static pthread_t signals_thread;

static int is_addr_on_stack(jl_task_t *ct, void *addr) JL_NOTSAFEPOINT
{
    if (ct->ctx.copy_stack) {
        jl_ptls_t ptls = ct->ptls;
        return ((char*)addr > (char*)ptls->stackbase - ptls->stacksize &&
                (char*)addr < (char*)ptls->stackbase);
    }
    return ((char*)addr > (char*)ct->ctx.stkbuf &&
            (char*)addr < (char*)ct->ctx.stkbuf + ct->ctx.bufsz);
}

static void sigdie_handler(int sig, siginfo_t *info, void *context) JL_CANSAFEPOINT
{
    signal(sig, SIG_DFL);
    uv_tty_reset_mode();
    if (sig == SIGILL)
        jl_fprint_sigill(ios_safe_stderr, context);
    jl_task_t *ct = jl_get_current_task();
    jl_fprint_critical_error(ios_safe_stderr, sig, info->si_code, jl_to_bt_context(context), ct);
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

static int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx) JL_NOTSAFEPOINT_ENTER_CONDITIONAL(1);

#if defined(HAVE_MACH)
#include "signals-mach.c"
#else
#include <poll.h>
#include <sys/eventfd.h>
#include <link.h>

typedef struct {
    int16_t tid;
    bt_context_t *ctx;
    int success;
} callback_data_t;
static int with_dl_iterate_phdr_lock(struct dl_phdr_info *info, size_t size, void *data)
{
    jl_lock_profile();
    callback_data_t *cb_data = (callback_data_t*)data;
    cb_data->success = jl_thread_suspend_and_get_state(cb_data->tid, 1, cb_data->ctx);
    jl_unlock_profile();
    return 1; // only call this once
}

int jl_thread_suspend(int16_t tid, bt_context_t *ctx)
{
    callback_data_t cb_data = {tid, ctx, 0};
    dl_iterate_phdr(with_dl_iterate_phdr_lock, &cb_data);
    return cb_data.success;
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
#elif defined(_OS_FREEBSD_) && defined(_CPU_AARCH64_)
// FreeBSD seems not to expose a means of accessing ESR via `ucontext_t` on AArch64.
// TODO: Is there an alternative approach that can be taken? ESR may become accessible
// in a future release though.
int is_write_fault(void *context) {
    return 0;
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

static int jl_is_on_sigstack(jl_ptls_t ptls, void *ptr, void *context) JL_NOTSAFEPOINT
{
    return (ptls->signal_stack != NULL &&
            is_addr_on_sigstack(ptls, ptr) &&
            is_addr_on_sigstack(ptls, (void*)jl_get_rsp_from_ctx(context)));
}

JL_NO_ASAN static void segv_handler(int sig, siginfo_t *info, void *context) JL_CANSAFEPOINT
{
    assert(sig == SIGSEGV || sig == SIGBUS);
    jl_jmp_buf *saferestore = jl_get_safe_restore();
    if (saferestore) { // restarting jl_ or profile
        jl_longjmp_in_ctx(sig, context, *saferestore);
        return;
    }
    jl_task_t *ct = jl_get_current_task();
    if (ct == NULL || ct->ptls == NULL || jl_atomic_load_relaxed(&ct->ptls->gc_state) == JL_GC_STATE_WAITING) {
        sigdie_handler(sig, info, context);
        return;
    }
    if (sig == SIGSEGV && info->si_code == SEGV_ACCERR && jl_addr_is_safepoint((uintptr_t)info->si_addr) && !is_write_fault(context)) {
        jl_set_gc_and_wait(ct);
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
        }
        return;
    }
    if (ct->eh == NULL)
        sigdie_handler(sig, info, context);
    if ((sig != SIGBUS || info->si_code == BUS_ADRERR) && is_addr_on_stack(ct, info->si_addr)) { // stack overflow and not a BUS_ADRALN (alignment error)
        stack_overflow_warning();
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

pthread_mutex_t in_signal_lock; // shared with jl_delete_thread
static bt_context_t *usr2_signal_context; // protected by in_signal_lock
static int exit_signal_cond = -1;
static int signal_caught_cond = -1;
static int signals_inflight = 0;

static int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx) JL_NOTSAFEPOINT_ENTER_CONDITIONAL(1)
{
    if (tid < 0 || tid >= jl_atomic_load_acquire(&jl_n_threads))
        return 0;
    int err;
    pthread_mutex_lock(&in_signal_lock);
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    jl_task_t *ct2 = ptls2 ? jl_atomic_load_relaxed(&ptls2->current_task) : NULL;
    if (ct2 == NULL) {
        // this thread is not alive or already dead
        pthread_mutex_unlock(&in_signal_lock);
        return 0;
    }
    while (signals_inflight) {
        // something is wrong, or there is already a usr2 in flight elsewhere
        // try to wait for it to finish or wait for timeout
        struct pollfd event = {signal_caught_cond, POLLIN, 0};
        do {
            err = poll(&event, 1, timeout * 1000);
        } while (err == -1 && errno == EINTR);
        if (err == -1 || (event.revents & POLLIN) == 0) {
            // not ready after timeout: cancel this request
            pthread_mutex_unlock(&in_signal_lock);
            return 0;
        }
        // consume it before continuing
        eventfd_t got;
        do {
            err = read(signal_caught_cond, &got, sizeof(eventfd_t));
        } while (err == -1 && errno == EINTR);
        if (err != sizeof(eventfd_t)) abort();
        assert(signals_inflight >= got);
        signals_inflight -= got;
    }
    signals_inflight++;
    sig_atomic_t request = jl_atomic_exchange(&ptls2->signal_request, 1);
    // A parked best-effort cancellation (5) or abandon (6) request may
    // occupy the slot indefinitely - its victim may never consume it (e.g.
    // a thread with SIGUSR2 blocked) - and both senders tolerate a lost
    // delivery (they re-send or time out and withdraw), so the suspend
    // handshake may displace them. The handshake states 1-4/-1 cannot
    // appear here: those settle under in_signal_lock, which we hold.
    assert(request == 0 || request == -1 || request == 5 || request == 6);
    request = 1;
    err = pthread_kill(ptls2->system_id, SIGUSR2);
    if (err == 0) {
        // wait for thread to acknowledge or timeout
        struct pollfd event = {signal_caught_cond, POLLIN, 0};
        do {
            err = poll(&event, 1, timeout * 1000);
        } while (err == -1 && errno == EINTR);
        if (err != 1 || (event.revents & POLLIN) == 0)
            err = -1;
    }
    if (err == -1) {
        // not ready after timeout: try to cancel this request
        if (jl_atomic_cmpswap(&ptls2->signal_request, &request, 0)) {
            signals_inflight--;
            pthread_mutex_unlock(&in_signal_lock);
            return 0;
        }
    }
    eventfd_t got;
    do {
        err = read(signal_caught_cond, &got, sizeof(eventfd_t));
    } while (err == -1 && errno == EINTR);
    if (err != sizeof(eventfd_t)) abort();
    assert(signals_inflight >= got);
    signals_inflight -= got;
    signals_inflight++;
    // Now the other thread is waiting on exit_signal_cond (verify that here by
    // checking it is 0, and add an acquire barrier for good measure)
    request = jl_atomic_load_acquire(&ptls2->signal_request);
    assert(request == 0 || request == -1); (void) request;
    jl_atomic_store_release(&ptls2->signal_request, 4); // prepare to resume normally, but later code may change this
    *ctx = *usr2_signal_context;
    return 1;
}

void jl_thread_resume(int tid)
{
    int err;
    eventfd_t got = 1;
    err = write(exit_signal_cond, &got, sizeof(eventfd_t));
    if (err != sizeof(eventfd_t)) abort();
    pthread_mutex_unlock(&in_signal_lock);
}

// Send a signal to the specified thread to deliver to its current task's
// published reset_ctx, if available: longjmp to a compiled reset point, or
// run a foreign call's cancellation handler (see usr2_handler request 5).
JL_DLLEXPORT void jl_send_cancellation_signal(int16_t tid) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL)
        return;
    jl_task_t *ct = jl_atomic_load_relaxed(&ptls2->current_task);
    if (ct == NULL)
        return;
    // Only send if the task has an interruptible-region context published
    // (a compiled reset point, or a foreign call with a cancellation
    // handler) - unless a ^C dispatch is pending and the task carries a
    // token binding: the handler's episode-propagation step (see
    // jl_sigint_propagate_to_bound) needs no published region, and a
    // purely polling victim between cancellation points never has one.
    if (jl_atomic_load_relaxed(&ct->reset_ctx) == NULL &&
        jl_atomic_load_relaxed(&ct->cancel_handler_ctx) == NULL) {
        jl_value_t *bound = jl_atomic_load_relaxed(&ct->bound_cancel_token);
        if (bound == NULL || bound == jl_nothing ||
            !jl_atomic_load_relaxed(&jl_sigint_dispatch_pending))
            return;
    }
    pthread_mutex_lock(&in_signal_lock);
    // This request is best-effort and produces no acknowledgment token (see
    // the handler): do not count it in signals_inflight, and never clobber a
    // request that is already pending or being processed - blindly storing
    // over an in-flight suspend handshake (or an abandon request) would lose
    // it; our caller retries delivery anyway.
    sig_atomic_t expected = 0;
    if (jl_atomic_cmpswap(&ptls2->signal_request, &expected, 5))
        pthread_kill(ptls2->system_id, SIGUSR2);
    pthread_mutex_unlock(&in_signal_lock);
}

// Send a signal to the specified thread to abandon the current task.
// The target task to switch to must already be set in ptls2->abandon_to,
// and the task's state must already be set to JL_TASK_STATE_ABANDONED.
void jl_send_abandon_signal(int16_t tid) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL)
        return;
    pthread_mutex_lock(&in_signal_lock);
    // Like the cancellation signal, this produces no acknowledgment token.
    // Abandonment overrides a pending best-effort cancellation signal (5),
    // but must not disturb a suspend handshake in progress (requests 1-4 /
    // processing) - jl_abandon_task re-sends until the switch is observed.
    sig_atomic_t expected = 0;
    if (jl_atomic_cmpswap(&ptls2->signal_request, &expected, 6) ||
        (expected == 5 && jl_atomic_cmpswap(&ptls2->signal_request, &expected, 6)))
        pthread_kill(ptls2->system_id, SIGUSR2);
    pthread_mutex_unlock(&in_signal_lock);
}

// Write only by signal handling thread, read only by main thread
// no sync necessary.
static int thread0_exit_signo = 0;
static void jl_exit_thread0_cb(void) JL_CANSAFEPOINT
{
    jl_atomic_fetch_add(&jl_gc_disable_counter, -1);
    jl_fprint_critical_error(ios_safe_stderr, thread0_exit_signo, 0, NULL, jl_current_task);
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
// -1: processing
//  0: nothing [not from here]
//  1: get state & wait for request
//  2: throw sigint if `!defer_signal && io_wait` or if force throw threshold
//     is reached
//  3: raise `thread0_exit_signo` and try to exit
//  4: no-op
//  5: deliver to the current task's published reset_ctx if available (for
//     task cancellation): longjmp to a reset point, or run a foreign call's
//     cancellation handler
//  6: abandon the current task and switch to ptls->abandon_to
//  7: restore the context saved by a cancellation-handler delivery
//     (self-sent by jl_cancel_handler_resume)
void usr2_handler(int sig, siginfo_t *info, void *ctx) JL_CANSAFEPOINT
{
    jl_task_t *ct = jl_get_current_task();
    if (ct == NULL)
        return;
    jl_ptls_t ptls = ct->ptls;
    if (ptls == NULL)
        return;
    int errno_save = errno;
    sig_atomic_t request = jl_atomic_load(&ptls->signal_request);
    if (request == 0)
        return;
    if (!jl_atomic_cmpswap(&ptls->signal_request, &request, -1))
        return;
    if (request == 1) {
        usr2_signal_context = jl_to_bt_context(ctx);
        // acknowledge that we saw the signal_request and set usr2_signal_context
        int err;
        eventfd_t got = 1;
        err = write(signal_caught_cond, &got, sizeof(eventfd_t));
        if (err != sizeof(eventfd_t)) abort();
        sig_atomic_t processing = -1;
        jl_atomic_cmpswap(&ptls->signal_request, &processing, 0);
        // wait for exit signal
        do {
            err = read(exit_signal_cond, &got, sizeof(eventfd_t));
        } while (err == -1 && errno == EINTR);
        if (err != sizeof(eventfd_t)) abort();
        assert(got == 1);
        request = jl_atomic_exchange(&ptls->signal_request, -1);
        usr2_signal_context = NULL;
        assert(request == 2 || request == 3 || request == 4);
    }
    if (request != 5 && request != 6 && request != 7) {
        // Acknowledge the request to its synchronously waiting sender. The
        // cancellation, abandon and restore senders (requests 5-7) are
        // fire-and-forget and never consume the token; writing it would
        // poison the next suspend handshake with a stale acknowledgment.
        int err;
        eventfd_t got = 1;
        err = write(signal_caught_cond, &got, sizeof(eventfd_t));
        if (err != sizeof(eventfd_t)) abort();
    }
    sig_atomic_t processing = -1;
    jl_atomic_cmpswap(&ptls->signal_request, &processing, 0);
    if (request == 2) {
        int force = jl_check_force_sigint();
        if (force || (!ptls->defer_signal && ptls->io_wait)) {
            jl_safepoint_consume_sigint();
            if (force)
                jl_safe_printf("WARNING: Force throwing a SIGINT\n");
            // Force a throw
            jl_clear_force_sigint();
            jl_jmp_buf *saferestore = jl_get_safe_restore();
            if (saferestore) // restarting jl_ or profile
                jl_longjmp_in_ctx(sig, ctx, *saferestore);
            else
                jl_throw_in_ctx(ct, jl_interrupt_exception, sig, ctx);
        }
    }
    else if (request == 3) {
        jl_call_in_ctx(ct->ptls, jl_exit_thread0_cb, sig, ctx);
    }
    else if (request == 5) {
        // Deliver to the published context(s) of the current task's
        // asynchronously interruptible regions, if any. N.B.: these are only
        // ever consumed for the thread's *current* task, whose stack is live
        // at its canonical address (copied stacks are swapped in before a
        // task becomes current), so the buffer addresses are valid here.
        // A handler region and a reset region may be active at the same
        // time; the handler takes priority while published - its span (e.g.
        // a protected allocator) is exactly where a longjmp must not land,
        // and the handler can defer the cancellation and chain into the
        // reset on region exit.
        // Deliveries are gated on an actual cancellation of the task's bound
        // token: a request-5 signal is also sent for cooperative preemption
        // (see jl_preempt_thread_task), which cannot be honored inside an
        // asynchronously interruptible region - aborting a foreign call (or
        // unwinding a protected span just to restart it) for a mere yield
        // request would discard its work for nothing. Preemption is instead
        // polled at every cancellation point.
        jl_value_t *bound = jl_atomic_load_relaxed(&ct->bound_cancel_token);
        int bound_cancelled = bound != NULL && bound != jl_nothing &&
            (jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state) & 0x80);
        // A pending ^C episode reaches scoped descendant sources through the
        // julia-side listener's walk; when the listener is starved (e.g. a
        // single-threaded process spinning in this task), carry it into the
        // task's own bound source here so the next cancellation point sees it.
        if (!bound_cancelled)
            bound_cancelled = jl_sigint_propagate_to_bound(bound);
        jl_reset_ctx_t *hctx = jl_atomic_load_acquire(&ct->cancel_handler_ctx);
        if (hctx != NULL) {
            // Handler flavor: run the registered cancellation handler on
            // this thread, signal-handler-style, and resume. The context
            // stays published - escalation or redelivery runs the
            // (idempotent) handler again once this delivery completes. (And
            // never fall through to the reset while the handler region is
            // published.)
#ifdef JL_HAVE_CANCEL_HANDLER_DELIVERY
            if (hctx->sp == 0 && bound_cancelled)
                jl_deliver_cancel_handler(ptls, ct, hctx, ctx);
#endif
        }
        else {
            jl_reset_ctx_t *reset_ctx = jl_atomic_load_acquire(&ct->reset_ctx);
            if (reset_ctx != NULL && reset_ctx->sp != 0 && bound_cancelled) {
                // Reset flavor: abandon the interrupted register state and
                // longjmp to the reset point, whose re-executed check
                // observes the cancellation and throws. Clear reset_ctx
                // before the longjmp to prevent a double reset.
                jl_atomic_store_relaxed(&ct->reset_ctx, NULL);
                jl_longjmp_in_ctx(sig, ctx, reset_ctx->ctx.uc_mcontext);
            }
        }
    }
    else if (request == 7) {
        // Restore the interrupted context saved by a cancellation-handler
        // delivery on this thread (see jl_cancel_handler_trampoline):
        // returning from this handler then resumes the originally
        // interrupted instruction.
#ifdef JL_HAVE_CANCEL_HANDLER_DELIVERY
        jl_cancel_handler_restore(ptls, ctx);
#endif
    }
    else if (request == 6) {
        // Task abandonment: validate the pending request against this
        // thread's actual state (we ARE the victim thread, stopped in this
        // handler, so nothing can change under the check) and, on commit,
        // redirect into the abandon callback (which must not return) to
        // switch to ptls->abandon_to. On refusal the requester observes the
        // verdict and withdraws; the current task continues untouched.
        if (jl_abandon_try_commit(ct->ptls)) {
            jl_call_in_ctx(ct->ptls, jl_abandon_task_cb, sig, ctx);
        }
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

JL_DLLEXPORT int jl_profile_start_timer(uint8_t all_tasks)
{
    struct sigevent sigprof;

    // Establish the signal event
    memset(&sigprof, 0, sizeof(struct sigevent));
    sigprof.sigev_notify = SIGEV_SIGNAL;
    sigprof.sigev_signo = SIGUSR1;
    sigprof.sigev_value.sival_ptr = &timerprof;
    // Because SIGUSR1 is multipurpose, set `profile_running` before so that we know that the first SIGUSR1 came from the timer
    profile_running = 1;
    profile_all_tasks = all_tasks;
    if (timer_create(CLOCK_REALTIME, &sigprof, &timerprof) == -1) {
        profile_running = 0;
        profile_all_tasks = 0;
        return -2;
    }

    // Start the timer
    itsprof.it_interval.tv_sec = 0;
    itsprof.it_interval.tv_nsec = 0;
    itsprof.it_value.tv_sec = nsecprof / GIGA;
    itsprof.it_value.tv_nsec = nsecprof % GIGA;
    if (timer_settime(timerprof, 0, &itsprof, NULL) == -1) {
        profile_running = 0;
        profile_all_tasks = 0;
        return -3;
    }
    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    uv_mutex_lock(&bt_data_prof_lock);
    if (profile_running) {
        timer_delete(timerprof);
        last_timer_delete_time = jl_hrtime();
        profile_running = 0;
    }
    uv_mutex_unlock(&bt_data_prof_lock);
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
    act.sa_sigaction = segv_handler; // NOLINT(julia-first-decl-annotations)
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
    jl_safe_printf("Information request received. A stacktrace will print followed by a %.1f second profile.\n", profile_peek_duration);
    jl_safe_printf("--trace-compile is enabled during profile collection.\n");
    jl_safe_printf("======================================================================================\n");
    if (profile_bt_size_max == 0) {
        // If the buffer hasn't been initialized, initialize with default size
        // Keep these values synchronized with Profile.default_init()
        if (jl_profile_init(10000000, 1000000) == -1) {
            jl_safe_printf("ERROR: could not initialize the profile buffer");
            return;
        }
    }
    profile_bt_size_cur = 0; // clear profile buffer
    if (jl_profile_start_timer(0) < 0)
        jl_safe_printf("ERROR: Could not start profile timer\n");
    else
        profile_autostop_time = jl_hrtime() + (profile_peek_duration * 1e9);
}

#if !defined(JL_DISABLE_LIBUNWIND)

static jl_bt_element_t signal_bt_data[JL_MAX_BT_SIZE + 1];
static size_t signal_bt_size = 0;
static void do_critical_profile(void)
{
    bt_context_t signal_context;
    // sample each thread, round-robin style in reverse order
    // (so that thread zero gets notified last)
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    for (int i = nthreads; i-- > 0; ) {
        // notify thread to stop
        if (!jl_thread_suspend(i, &signal_context))
            continue;

        // do backtrace on thread contexts for critical signals
        // this part must be signal-handler safe
        signal_bt_size += rec_backtrace_ctx(signal_bt_data + signal_bt_size,
                JL_MAX_BT_SIZE / nthreads - 1,
                &signal_context, NULL);
        signal_bt_data[signal_bt_size++].uintptr = 0;
        jl_thread_resume(i);
    }
}

static void do_profile(void) JL_NOTSAFEPOINT
{
    bt_context_t signal_context;
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    int *randperm = profile_get_randperm(nthreads);
    for (int idx = nthreads; idx-- > 0; ) {
        // Stop the threads in the random order.
        int tid = randperm[idx];
        // do backtrace for profiler
        if (!profile_running)
            return;
        if (jl_profile_is_buffer_full()) {
            // Buffer full: Delete the timer
            jl_profile_stop_timer();
            return;
        }
        // notify thread to stop
        if (!jl_thread_suspend(tid, &signal_context))
            return;
        // unwinding can fail, so keep track of the current state
        // and restore from the SEGV handler if anything happens.
        jl_jmp_buf *old_buf = jl_get_safe_restore();
        jl_jmp_buf buf;

        jl_set_safe_restore(&buf);
        if (jl_setjmp(buf, 0)) {
            jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
        }
        else {
            // Get backtrace data
            profile_bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)profile_bt_data_prof + profile_bt_size_cur,
                    profile_bt_size_max - profile_bt_size_cur - 1, &signal_context, NULL);
        }
        jl_set_safe_restore(old_buf);

        jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];

        // store threadid but add 1 as 0 is preserved to indicate end of block
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = ptls2->tid + 1;

        // store task id (never null)
        profile_bt_data_prof[profile_bt_size_cur++].jlvalue = (jl_value_t*)jl_atomic_load_relaxed(&ptls2->current_task);

        // store cpu cycle clock
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = cycleclock();

        // store whether thread is sleeping (don't ever encode a state as `0` since it is preserved to indicate end of block)
        int state = jl_atomic_load_relaxed(&ptls2->sleep_check_state) == 0 ? PROFILE_STATE_THREAD_NOT_SLEEPING : PROFILE_STATE_THREAD_SLEEPING;
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = state;

        // Mark the end of this block with two 0's
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;

        // notify thread to resume
        jl_thread_resume(tid);
    }
}
#endif

static void *signal_listener(void *arg) JL_NOTSAFEPOINT
{
    sigset_t sset;
    int sig, critical, profile, doexit = 0, rescue_bt = 0;
    jl_sigsetset(&sset);
#ifdef HAVE_SIGWAITINFO
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
    // The ^C escalation timer is delivered through this kqueue
    sigint_rescue_kq = sigqueue;
#endif
    int rescue_timer_fired;
    while (1) {
        sig = 0;
        errno = 0;
        rescue_timer_fired = 0;
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
                sigint_rescue_kq = -1;
                for (const int *sig = sigwait_sigs; *sig; sig++)
                    signal(*sig, SIG_DFL);
                continue;
            }
            if (ev.filter == EVFILT_TIMER) {
                // the ^C escalation (rescue) timer expired
                if (ev.ident != JL_SIGINT_RESCUE_TIMER_IDENT)
                    continue;
                sig = SIGINT;
                rescue_timer_fired = 1;
            }
            else {
                sig = ev.ident;
            }
        }
        else
#endif
#ifdef HAVE_SIGWAITINFO
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
#ifdef HAVE_SIGWAITINFO
        if (profile && !(info.si_code == SI_TIMER &&
                info.si_value.sival_ptr == &timerprof))
            profile = 0;
#endif
#endif
#endif

        if (sig == SIGINT) {
#if defined(HAVE_SIGWAITINFO) && !defined(HAVE_KEVENT)
            // Check if this SIGINT came from our rescue timer (si_code == SI_TIMER
            // and sival_int == 1). This means the process failed to respond to
            // the cancellation request in time.
            if (info.si_code == SI_TIMER && info.si_value.sival_int == 1)
                rescue_timer_fired = 1;
#endif
            if (rescue_timer_fired) {
                int est = jl_sigint_episode_state();
                if (est == 0)
                    continue; // the episode already completed - stand down
                // Mark that the timer has expired - the next SIGINT escalates
                // (via the listener, or the direct abandonment below).
                jl_sigint_rescue_timer_expired();
                if (est == 2) {
                    jl_safe_printf("\nWARNING: Cancellation is in progress, but has not completed within 1s.\n"
                                     "         You (or a package author) may need to add more @cancel_check's.\n"
                                     "         Press ^C again to also stop waiting for external resources (e.g. in-flight I/O).\n"
#ifdef SIGINFO
                                     "         Press ^T to print thread backtraces.\n");
#else
                                     "         Send SIGUSR1 to print thread backtraces.\n");
#endif
                    continue;
                }
                if (est == 3) {
                    jl_safe_printf("\nWARNING: Cancellation has still not completed.\n"
                                     "         Press ^C again to forcibly abandon the current task (unsafe; may leak resources).\n"
#ifdef SIGINFO
                                     "         Press ^T to print thread backtraces.\n");
#else
                                     "         Send SIGUSR1 to print thread backtraces.\n");
#endif
                    continue;
                }
                jl_safe_printf("\nWARNING: Process failed to acknowledge SIGINT within 1s.\n"
                                 "         You (or a package author) may need to add more @cancel_check's.\n"
#ifdef SIGINFO
                                 "         Press ^T to print thread backtraces.\n"
#else
                                 "         Send SIGUSR1 to print thread backtraces.\n"
#endif
                                 "         Press ^C again to (unsafely) abandon the current task.\n");
                continue;
            }
            if (jl_ignore_sigint()) {
                continue;
            }
            else if (exit_on_sigint) {
                critical = 1;
            }
            else {
                // Check if the rescue timer has already expired (from a previous SIGINT
                // cycle). If so, the user is pressing Ctrl-C again after the warning -
                // time to abandon the stuck task, unless the julia-side
                // escalation (SAFE -> ABANDON_EXTERNAL -> ABANDON_ALL) can
                // still make progress on it.
                jl_task_t *rescue_task = NULL;
                jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
                // Direct abandonment rips away whatever thread 0 is currently
                // running, which is only legitimate when that is the stuck
                // victim monopolizing the thread in managed compute. A thread
                // in GC-safe state is parked in the scheduler or a foreign
                // call (possibly holding the uv loop lock!) - not the victim;
                // and an idle thread 0 means the julia-side listener can act.
                if (jl_sigint_rescue_timer_expired_peek() && jl_sigint_direct_abandon_allowed()) {
                    // The victim may transiently be inside the allocator or
                    // the GC (runtime locks held / not in managed compute)
                    // when the press lands - especially with an allocating
                    // interpreted loop. Give it a brief window to return to
                    // an abandonable state rather than consuming the press
                    // and losing the escalation rung.
                    for (int tries = 0; tries < 100; tries++) {
                        if (jl_atomic_load_relaxed(&ptls2->gc_state) == 0 &&
                            ptls2->locks.len == 0) {
                            // consumes the expiry; returns NULL if the episode
                            // completed (or was reset) while we waited
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
                    jl_value_t *sigsrc = (jl_value_t*)jl_atomic_load_acquire(&jl_sigint_source);
                // Only a task actually governed by the ^C source may be
                // ripped away: thread 0 can be running unrelated work while
                // the real victim is stalled elsewhere - including runtime
                // infrastructure like the sigint listener itself, whose
                // token binding is empty - and a no-op rung beats
                // destroying a bystander.
                    int governed = bound != NULL && bound != jl_nothing &&
                        sigsrc != NULL && jl_cancel_source_subtree_member(bound, sigsrc);
                    if (ct != NULL && ct != rescue_task && governed) {
                        // Announce BEFORE abandoning: the moment the victim
                        // thread switches to the rescue task, session cleanup
                        // can conclude (in a script it exits the process) -
                        // on a busy or single-CPU machine that exit wins the
                        // race against a message printed afterwards.
                        jl_safe_printf("\nWARNING: Abandoning the current task and switching to a rescue task.\n"
                                         "         This may leave the process in an inconsistent state.\n");
                        if (jl_abandon_task(ct, rescue_task)) {
                            // Let the sigint listener task perform the
                            // Julia-side cleanup (waking the abandoned task's
                            // waiters and re-initializing or shutting down
                            // the session).
                            deliver_sigint_notification();
                        }
                        else {
                            jl_safe_printf("\nWARNING: Could not abandon the current task (it holds runtime resources); still trying.\n");
                        }
                        continue;
                    }
                }

                // Request cancellation of the root task and notify the sigint
                // listener - if the task is not currently running, the sigint
                // listener will take care of safely moving us through the
                // cancellation state machine.
                // TODO: If there is only one thread, we may need to ask the currently
                // running task to yield, so that the sigint listener can run.
                jl_sigint_request_cancellation();
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

        doexit = critical;
#ifdef SIGINFO
        if (sig == SIGINFO) {
            if (jl_sigint_episode_state() != 0) {
                // On-demand thread backtraces during a ^C episode.
                critical = 1;
                doexit = 0;
                rescue_bt = 1;
                goto noexit_critical;
            }
            if (profile_running != 1)
                trigger_profile_peek();
            doexit = 0;
        }
#else
        if (sig == SIGUSR1) {
#ifdef HAVE_SIGWAITINFO
            if (jl_sigint_episode_state() != 0) {
                // On-demand thread backtraces during a ^C episode.
                critical = 1;
                doexit = 0;
                rescue_bt = 1;
                goto noexit_critical;
            }
#endif
            if (profile_running != 1 && timer_graceperiod_elapsed())
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

#if defined(SIGINFO) || defined(HAVE_SIGWAITINFO) || defined(HAVE_KEVENT)
noexit_critical:
#endif
        signal_bt_size = 0;
#if !defined(JL_DISABLE_LIBUNWIND)
        if (critical) {
            do_critical_profile();
        }
        else if (profile) {
            if (profile_all_tasks) {
                // Don't take the stackwalk lock here since it's already taken in `jl_rec_backtrace`
                jl_profile_task();
            }
            else {
                do_profile();
            }
        }
#ifndef HAVE_MACH
        if (profile_running) {
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
            // Let's forbid threads from running GC while we're trying to exit,
            // also let's make sure we're not in the middle of GC.
            jl_atomic_fetch_add(&jl_gc_disable_counter, 1);
            jl_safepoint_wait_gc(NULL);
            jl_exit_thread0(sig, signal_bt_data, signal_bt_size);
        }
        else if (critical) {
            // critical in this case actually means SIGINFO request
#ifndef SIGINFO // SIGINFO already prints something similar automatically
            int nthreads = jl_atomic_load_acquire(&jl_n_threads);
            int n_threads_running = 0;
            for (int idx = nthreads; idx-- > 0; ) {
                jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[idx];
                n_threads_running += !jl_atomic_load_relaxed(&ptls2->sleep_check_state);
            }
            jl_safe_printf("\ncmd: %s %d running %d of %d\n", jl_options.julia_bin ? jl_options.julia_bin : "julia", uv_os_getpid(), n_threads_running, nthreads);
#endif

            jl_safe_printf("\nsignal (%d): %s\n", sig, jl_strsignal(sig));
            size_t i;
            for (i = 0; i < signal_bt_size; i += jl_bt_entry_size(signal_bt_data + i)) {
                jl_fprint_bt_entry_codeloc(ios_safe_stderr, signal_bt_data + i);
            }
            jl_safe_printf("\n");
            // Enable trace compilation to stderr with timing during profile collection
            // (not wanted for the automated ^C-escalation backtrace collection)
            if (!rescue_bt)
                jl_force_trace_compile_timing_enable();
            rescue_bt = 0;
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

#if !defined(HAVE_MACH)
    exit_signal_cond = eventfd(0, EFD_CLOEXEC);
    signal_caught_cond = eventfd(0, EFD_CLOEXEC);
    if (pthread_mutex_init(&in_signal_lock, NULL) != 0 ||
            exit_signal_cond == -1 ||
            signal_caught_cond == -1) {
        jl_error("SIGUSR pthread init failed");
    }
#endif

    if (pthread_create(&signals_thread, NULL, signal_listener, NULL) != 0) {
        jl_error("pthread_create(signal_listener) failed");
    }
}

static void fpe_handler(int sig, siginfo_t *info, void *context) JL_CANSAFEPOINT
{
    (void)info;
    jl_jmp_buf *saferestore = jl_get_safe_restore();
    if (saferestore) { // restarting jl_ or profile
        jl_longjmp_in_ctx(sig, context, *saferestore);
        return;
    }
    jl_task_t *ct = jl_get_current_task();
    if (ct == NULL || ct->eh == NULL) // exception on foreign thread is fatal
        sigdie_handler(sig, info, context);
    else
        jl_throw_in_ctx(ct, jl_diverror_exception, sig, context);
}

static void jl_longjmp_in_ctx(int sig, void *_ctx, jl_jmp_buf jmpbuf)
{
#if defined(_OS_DARWIN_)
    jl_longjmp_in_state((host_thread_state_t*)jl_to_bt_context(_ctx), jmpbuf);
#else
    if (jl_simulate_longjmp(jmpbuf, jl_to_bt_context(_ctx)))
        return;
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, sig);
    pthread_sigmask(SIG_UNBLOCK, &sset, NULL);
    jl_longjmp(jmpbuf, 1);
#endif
}

static void sigint_handler(int sig)
{
    jl_sigint_passed = 1;
}

#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
static void sigtrap_handler(int sig, siginfo_t *info, void *context) JL_CANSAFEPOINT
{
    uintptr_t pc = ((ucontext_t*)context)->uc_mcontext->__ss.__pc; // TODO: Do this in linux as well
    uint32_t* code = (uint32_t*)(pc);                              // https://gcc.gnu.org/legacy-ml/gcc-patches/2013-11/msg02228.html
    if (*code == 0xd4200020) { // brk #0x1 which is what LLVM defines as trap
        signal(sig, SIG_DFL);
        sig = SIGILL; // redefine this as an "unreachable reached" error message
        sigdie_handler(sig, info, context);
    }
}
#endif

void jl_install_default_signal_handlers(void)
{
    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_sigaction = fpe_handler; // NOLINT(julia-first-decl-annotations)
    actf.sa_flags = SA_SIGINFO;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
    struct sigaction acttrap;
    memset(&acttrap, 0, sizeof(struct sigaction));
    sigemptyset(&acttrap.sa_mask);
    acttrap.sa_sigaction = sigtrap_handler; // NOLINT(julia-first-decl-annotations)
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
    act.sa_sigaction = usr2_handler; // NOLINT(julia-first-decl-annotations)
    act.sa_flags = SA_SIGINFO | SA_RESTART;
    if (sigaction(SIGUSR2, &act, NULL) < 0) {
        jl_errorf("fatal error: sigaction: %s", strerror(errno));
    }
#endif

    allocate_segv_handler();

    struct sigaction act_die;
    memset(&act_die, 0, sizeof(struct sigaction));
    sigemptyset(&act_die.sa_mask);
    act_die.sa_sigaction = sigdie_handler; // NOLINT(julia-first-decl-annotations)
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

#if !defined(_OS_DARWIN_)
// Thread suspension based membarrier fallback.
// This is a sound but slow implementation that suspends and resumes each thread
// to force them to execute memory barriers via the signal handling mechanism.
// This is used as a fallback when neither the membarrier syscall nor the mprotect
// hack are available or working.
static void jl_thread_suspend_membarrier(void)
{
    bt_context_t ctx;
    // Suspend each thread and immediately resume it.
    // The act of suspending/resuming forces a memory barrier via
    // the signal handler mechanism.
    // jl_thread_suspend tries to interrupt the thread for up to 1 second,
    // so we retry in a loop until it succeeds or we determine the thread
    // is no longer alive.
    for (int tid = 0; tid < jl_atomic_load_acquire(&jl_n_threads); tid++) {
        while (!jl_thread_suspend(tid, &ctx)) {
            jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
            jl_task_t *ct2 = ptls2 ? jl_atomic_load_relaxed(&ptls2->current_task) : NULL;
            if (ct2 == NULL) {
                // this thread is not alive or already dead, move to next
                goto next_thread;
            }
            // thread is alive but suspend failed, retry
        }
        jl_thread_resume(tid);
next_thread:;
    }
}

// Implementation of the `mprotect` based membarrier fallback.
// This is a common fallback based on the observation that `mprotect` happens to
// issue the necessary memory barriers. However, there is no spec that
// guarantees this behavior. On AArch64, it is known not to work on either
// Linux or FreeBSD, so we don't use it there. However, we use it as a fallback
// here for older versions of Linux and FreeBSD on x86 where we know that it
// happens to work.
#if !defined(_CPU_AARCH64_) && !defined(_CPU_ARM_)
static pthread_mutex_t mprotect_barrier_lock = PTHREAD_MUTEX_INITIALIZER;
static _Atomic(uint64_t) *mprotect_barrier_page = NULL;
// Returns 1 on success, 0 on failure (e.g. mlock fails)
static int jl_init_mprotect_membarrier(void)
{
    int result = pthread_mutex_lock(&mprotect_barrier_lock);
    assert(result == 0);
    if (mprotect_barrier_page == NULL) {
        size_t pagesize = jl_getpagesize();

        mprotect_barrier_page = (_Atomic(uint64_t) *)
                                     mmap(NULL, pagesize, PROT_READ | PROT_WRITE,
                                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (mprotect_barrier_page == MAP_FAILED) {
            mprotect_barrier_page = NULL;
            result = pthread_mutex_unlock(&mprotect_barrier_lock);
            assert(result == 0);
            return 0;
        }
        result = mlock(mprotect_barrier_page, pagesize);
        if (result != 0) {
            // mlock failed (e.g. RLIMIT_MEMLOCK too low), fall back to thread suspension
            munmap(mprotect_barrier_page, pagesize);
            mprotect_barrier_page = NULL;
            result = pthread_mutex_unlock(&mprotect_barrier_lock);
            assert(result == 0);
            return 0;
        }
    }
    result = pthread_mutex_unlock(&mprotect_barrier_lock);
    assert(result == 0);
    (void)result;
    return 1;
}

static void jl_mprotect_membarrier(void)
{
    int result = pthread_mutex_lock(&mprotect_barrier_lock);
    assert(result == 0);
    size_t pagesize = jl_getpagesize();
    result = mprotect(mprotect_barrier_page, pagesize, PROT_READ | PROT_WRITE);
    jl_atomic_fetch_add_relaxed(mprotect_barrier_page, 1);
    assert(result == 0);
    result = mprotect(mprotect_barrier_page, pagesize, PROT_NONE);
    assert(result == 0);
    result = pthread_mutex_unlock(&mprotect_barrier_lock);
    assert(result == 0);
    (void)result;
}
#endif // !_CPU_AARCH64_ && !_CPU_ARM_

// Membarrier implementation selection
enum membarrier_implementation {
    MEMBARRIER_IMPLEMENTATION_UNKNOWN        = 0,
    MEMBARRIER_IMPLEMENTATION_SYS_MEMBARRIER = 1,
    MEMBARRIER_IMPLEMENTATION_MPROTECT       = 2,
    MEMBARRIER_IMPLEMENTATION_THREAD_SUSPEND = 3
};

static _Atomic(enum membarrier_implementation) membarrier_impl = MEMBARRIER_IMPLEMENTATION_UNKNOWN;

// Linux and FreeBSD have compatible membarrier syscall support
#if defined(_OS_LINUX_)
#   include <sys/syscall.h>
#   if defined(__NR_membarrier)
enum membarrier_cmd {
    MEMBARRIER_CMD_QUERY                        = 0,
    MEMBARRIER_CMD_PRIVATE_EXPEDITED            = (1 << 3),
    MEMBARRIER_CMD_REGISTER_PRIVATE_EXPEDITED   = (1 << 4),
};
#    define membarrier(...) syscall(__NR_membarrier, __VA_ARGS__)
#    define HAVE_MEMBARRIER_SYSCALL
#  else
#    warning "Missing linux kernel headers for membarrier syscall, support disabled"
#  endif
#elif defined(_OS_FREEBSD_)
#  include <sys/param.h>
#  if __FreeBSD_version >= 1401500
#    include <sys/membarrier.h>
#    define HAVE_MEMBARRIER_SYSCALL
#  endif
#endif

static enum membarrier_implementation jl_init_membarrier(void) {
#ifdef HAVE_MEMBARRIER_SYSCALL
    int ret = membarrier(MEMBARRIER_CMD_QUERY, 0, 0);
    int needed = MEMBARRIER_CMD_PRIVATE_EXPEDITED | MEMBARRIER_CMD_REGISTER_PRIVATE_EXPEDITED;
    if (ret > 0 && ((ret & needed) == needed)) {
        // supported
        if (membarrier(MEMBARRIER_CMD_REGISTER_PRIVATE_EXPEDITED, 0, 0) == 0) {
            // working
            jl_atomic_store_relaxed(&membarrier_impl, MEMBARRIER_IMPLEMENTATION_SYS_MEMBARRIER);
            return MEMBARRIER_IMPLEMENTATION_SYS_MEMBARRIER;
        }
    }
#endif
    // The mprotect fallback is known not to work on AArch64, so skip it there
#if !defined(_CPU_AARCH64_) && !defined(_CPU_ARM_)
    if (jl_init_mprotect_membarrier()) {
        jl_atomic_store_relaxed(&membarrier_impl, MEMBARRIER_IMPLEMENTATION_MPROTECT);
        return MEMBARRIER_IMPLEMENTATION_MPROTECT;
    }
#endif
    // Fall back to thread suspension (sound but slow)
    jl_atomic_store_relaxed(&membarrier_impl, MEMBARRIER_IMPLEMENTATION_THREAD_SUSPEND);
    return MEMBARRIER_IMPLEMENTATION_THREAD_SUSPEND;
}

JL_DLLEXPORT void jl_membarrier(void) {
    enum membarrier_implementation impl = jl_atomic_load_relaxed(&membarrier_impl);
    if (impl == MEMBARRIER_IMPLEMENTATION_UNKNOWN) {
        impl = jl_init_membarrier();
    }
    switch (impl) {
#ifdef HAVE_MEMBARRIER_SYSCALL
    case MEMBARRIER_IMPLEMENTATION_SYS_MEMBARRIER: {
        int ret = membarrier(MEMBARRIER_CMD_PRIVATE_EXPEDITED, 0, 0);
        assert(ret == 0);
        (void)ret;
        break;
    }
#endif
#if !defined(_CPU_AARCH64_) && !defined(_CPU_ARM_)
    case MEMBARRIER_IMPLEMENTATION_MPROTECT:
        jl_mprotect_membarrier();
        break;
#endif
    case MEMBARRIER_IMPLEMENTATION_THREAD_SUSPEND:
        jl_thread_suspend_membarrier();
        break;
    default:
        abort();
    }
}
#endif // !_OS_DARWIN_
