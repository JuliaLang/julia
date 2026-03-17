// This file is a part of Julia. License is MIT: https://julialang.org/license

// Note that this file is `#include`d by "signals-unix.c"

#include <mach/clock.h>
#include <mach/clock_types.h>
#include <mach/clock_reply.h>
#include <mach/thread_state.h>
#include <mach/mach_traps.h>
#include <mach/task.h>
#include <mach/mig_errors.h>
#include <AvailabilityMacros.h>
#include <stdint.h>
#include "mach_excServer.c"

#ifdef MAC_OS_X_VERSION_10_9
#include <sys/_types/_ucontext64.h>
#else
#define __need_ucontext64_t
#include <sys/_structs.h>
#endif

#include "julia_assert.h"

#if defined(_CPU_X86_64_)
// Size of the xsave area for this CPU, queried via CPUID at init time.
// Used by jl_call_protect_fp_state to save/restore full AVX/AVX-512 state.
// Initialized to 0; a value of 0 means xsave is not available (fallback to fxsave).
uint32_t jl_xsave_size = 0;
static void jl_init_xsave_size(void)
{
    uint32_t eax, ebx, ecx, edx;
    // Check for xsave support: CPUID.01H:ECX.XSAVE[bit 26] and OSXSAVE[bit 27]
    asm("cpuid" : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx) : "a"(1), "c"(0));
    if (!(ecx & (1 << 26)) || !(ecx & (1 << 27)))
        return; // no xsave or OS hasn't enabled it
    // CPUID leaf 0DH, subleaf 0: EBX = size required for currently-enabled features (XCR0)
    asm("cpuid" : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx) : "a"(0xD), "c"(0));
    jl_xsave_size = ebx;
}
#endif

// private keymgr stuff
#define KEYMGR_GCC3_DW2_OBJ_LIST 302
enum {
  NM_ALLOW_RECURSION = 1,
  NM_RECURSION_ILLEGAL = 2
};
extern void _keymgr_set_and_unlock_processwide_ptr(unsigned int key, void *ptr);
extern int _keymgr_unlock_processwide_ptr(unsigned int key);
extern void *_keymgr_get_and_lock_processwide_ptr(unsigned int key);
extern int _keymgr_get_and_lock_processwide_ptr_2(unsigned int key, void **result);
extern int _keymgr_set_lockmode_processwide_ptr(unsigned int key, unsigned int mode);

// private dyld3/dyld4 stuff
extern void _dyld_atfork_prepare(void) __attribute__((weak_import));
extern void _dyld_atfork_parent(void) __attribute__((weak_import));
//extern void _dyld_fork_child(void) __attribute__((weak_import));
extern void _dyld_dlopen_atfork_prepare(void) __attribute__((weak_import));
extern void _dyld_dlopen_atfork_parent(void) __attribute__((weak_import));
//extern void _dyld_dlopen_atfork_child(void) __attribute__((weak_import));

static void attach_exception_port(thread_port_t thread, int segv_only);

static mach_port_t segv_port = 0;

#define HANDLE_MACH_ERROR(msg, retval) \
    if (retval != KERN_SUCCESS) { mach_error(msg XSTR(: __FILE__:__LINE__:), (retval)); abort(); }

void *mach_segv_listener(void *arg)
{
    (void)arg;
    int ret = mach_msg_server(mach_exc_server, 2048, segv_port, MACH_MSG_TIMEOUT_NONE);
    mach_error("mach_msg_server" XSTR(: __FILE__:__LINE__:), ret);
    abort();
}


static void allocate_mach_handler(void)
{
#if defined(_CPU_X86_64_)
    jl_init_xsave_size();
#endif
    // ensure KEYMGR_GCC3_DW2_OBJ_LIST is initialized, as this requires malloc
    // and thus can deadlock when used without first initializing it.
    // Apple caused this problem in their libunwind in 10.9 (circa keymgr-28)
    // when they removed this part of the code from keymgr.
    // Much thanks to Apple for providing source code, or this would probably
    // have simply remained unsolved forever on their platform.
    // This is similar to just calling checkKeyMgrRegisteredFDEs
    // (this is quite thread-unsafe)
    if (_keymgr_set_lockmode_processwide_ptr(KEYMGR_GCC3_DW2_OBJ_LIST, NM_ALLOW_RECURSION))
        jl_error("_keymgr_set_lockmode_processwide_ptr failed");

    pthread_t thread;
    pthread_attr_t attr;
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
}

#if defined(_CPU_X86_64_)
typedef x86_thread_state64_t host_thread_state_t;
typedef x86_exception_state64_t host_exception_state_t;
#define MACH_THREAD_STATE x86_THREAD_STATE64
#define MACH_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define HOST_EXCEPTION_STATE x86_EXCEPTION_STATE64
#define HOST_EXCEPTION_STATE_COUNT x86_EXCEPTION_STATE64_COUNT

#elif defined(_CPU_AARCH64_)
typedef arm_thread_state64_t host_thread_state_t;
typedef arm_exception_state64_t host_exception_state_t;
#define MACH_THREAD_STATE ARM_THREAD_STATE64
#define MACH_THREAD_STATE_COUNT ARM_THREAD_STATE64_COUNT
#define HOST_EXCEPTION_STATE ARM_EXCEPTION_STATE64
#define HOST_EXCEPTION_STATE_COUNT ARM_EXCEPTION_STATE64_COUNT
#endif

#ifdef LLVMLIBUNWIND
volatile mach_port_t mach_profiler_thread = 0;
static kern_return_t profiler_segv_handler(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt,
    host_thread_state_t *state,
    mach_msg_type_number_t *new_stateCnt);
#endif

// Create a fake function that describes the register manipulations in jl_noreturn_call_in_state
// The callee-saved registers still may get smashed (by the cdecl fptr), since we didn't explicitly copy all of the
// state to the stack (to build a real sigreturn frame).
__attribute__((naked, used)) void jl_fake_signal_return(void)
{
#if defined(_CPU_X86_64_)
__asm__(
    "  .cfi_signal_frame\n"
    "  .cfi_def_cfa %rsp, 0\n" // CFA here uses %rsp directly
    "  .cfi_offset %rip, 0\n" // previous value of %rip at CFA
    "  .cfi_offset %rsp, 8\n" // previous value of %rsp at CFA
    "  ud2\n"
    "  ud2\n"
);
#elif defined(_CPU_AARCH64_)
__asm__(
    "  .cfi_signal_frame\n"
    "  .cfi_def_cfa sp, 0\n" // use sp as fp here
    // This is not quite valid, since the AArch64 DWARF spec lacks the ability to define how to restore the LR register correctly,
    // so normally libunwind implementations on linux detect this function specially and hack around the invalid info:
    // https://github.com/llvm/llvm-project/commit/c82deed6764cbc63966374baf9721331901ca958
    "  .cfi_offset lr, 0\n"
    "  .cfi_offset sp, 8\n"
    "  brk #1\n"
    "  brk #1\n"
);
#endif
}

// Optimized version of `jl_call_in_state` that avoids saving most CPU registers.
static void jl_noreturn_call_in_state(host_thread_state_t *state, void (*fptr)(void), uintptr_t arg0, uintptr_t arg1)
{
#ifdef _CPU_X86_64_
    uintptr_t sp = state->__rsp;
#elif defined(_CPU_AARCH64_)
    uintptr_t sp = state->__sp;
#endif
    sp = (sp - 256) & ~(uintptr_t)15; // redzone and re-alignment
    assert(sp % 16 == 0);
#ifdef _CPU_X86_64_
    // push {%rsp, %rip}
    sp -= sizeof(void*);
    *(uintptr_t*)sp = state->__rsp;
    sp -= sizeof(void*);
    *(uintptr_t*)sp = state->__rip;
    // pushq .jl_fake_signal_return + 1; aka call from jl_fake_signal_return
    sp -= sizeof(void*);
    *(uintptr_t*)sp = (uintptr_t)&jl_fake_signal_return + 1;
    state->__rsp = sp; // set stack pointer
    state->__rip = (uint64_t)fptr; // "call" the function
    state->__rdi = arg0;
    state->__rsi = arg1;
#elif defined(_CPU_AARCH64_)
    // push {%pc, %sp}
    sp -= sizeof(void*);
    *(uintptr_t*)sp = state->__sp;
    sp -= sizeof(void*);
    *(uintptr_t*)sp = (uintptr_t)state->__pc;
    state->__sp = sp; // x31
    state->__pc = (uint64_t)fptr; // pc
    state->__lr = (uintptr_t)&jl_fake_signal_return + 4; // x30
    state->__x[0] = arg0;
    state->__x[1] = arg1;
#else
#error "julia: throw-in-context not supported on this platform"
#endif
}

static void jl_longjmp_in_state(host_thread_state_t *state, jl_jmp_buf jmpbuf)
{
    if (!jl_simulate_longjmp(jmpbuf, (bt_context_t*)state)) {
        // for sanitizer builds, fallback to calling longjmp on the original stack
        // (this will fail for stack overflow, but that is hardly sanitizer-legal anyways)
        jl_noreturn_call_in_state(state, (void (*)(void))longjmp, (uintptr_t)jmpbuf, 1);
    }
}

#ifdef _CPU_X86_64_
int is_write_fault(host_exception_state_t exc_state) {
    return exc_reg_is_write_fault(exc_state.__err);
}
#elif defined(_CPU_AARCH64_)
int is_write_fault(host_exception_state_t exc_state) {
    return exc_reg_is_write_fault(exc_state.__esr);
}
#else
#warning Implement this query for consistent PROT_NONE handling
int is_write_fault(host_exception_state_t exc_state) {
    return 0;
}
#endif

static void jl_throw_in_state(jl_ptls_t ptls2, host_thread_state_t *state, jl_value_t *exception)
{
    if (ptls2->safe_restore) {
        jl_longjmp_in_state(state, *ptls2->safe_restore);
    }
    else {
        assert(exception);
        ptls2->bt_size =
            rec_backtrace_ctx(ptls2->bt_data, JL_MAX_BT_SIZE, (bt_context_t *)state,
                            NULL /*current_task?*/);
        ptls2->sig_exception = exception;
        ptls2->io_wait = 0;
        jl_task_t *ct = jl_atomic_load_relaxed(&ptls2->current_task);
        jl_handler_t *eh = ct->eh;
        if (eh != NULL) {
            asan_unpoison_task_stack(ct, &eh->eh_ctx);
            jl_longjmp_in_state(state, eh->eh_ctx);
        }
        else {
            jl_no_exc_handler(exception, ct);
        }
    }
}

static void jl_throw_in_thread(jl_ptls_t ptls2, mach_port_t thread, jl_value_t *exception)
{
    host_thread_state_t state;
    unsigned int count = MACH_THREAD_STATE_COUNT;
    kern_return_t ret = thread_get_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, &count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
    jl_throw_in_state(ptls2, &state, exception);
    ret = thread_set_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);
}

// Trampoline that runs on the faulting thread after being hijacked by the
// Mach exception handler for a safepoint hit. This uses the same codepath
// as the Unix signal handler (jl_set_gc_and_wait), so the faulting thread
// participates in GC synchronization directly.
//
// ptls is passed as an argument (via x0/rdi) since __thread TLS may not
// be valid (e.g. during thread teardown).
static void mach_safepoint_trampoline(jl_ptls_t ptls)
{
    jl_task_t *ct = jl_atomic_load_relaxed(&ptls->current_task);
    if (ct == NULL)
        return; // thread is dead, just resume
    jl_set_gc_and_wait(ct);
    if (jl_atomic_load_relaxed(&ct->tid) != 0)
        return;
    if (ptls->defer_signal) {
        jl_safepoint_defer_sigint();
    }
    else if (jl_safepoint_consume_sigint()) {
        jl_clear_force_sigint();
        jl_throw(jl_interrupt_exception);
    }
}

// Assembly wrapper that saves and restores FP/SIMD registers around a
// function call. This runs on the hijacked thread itself, so it can
// save/restore FP state with direct register access — no syscall needed.
//
// On entry: arg0 in x0/rdi, fptr in x1/rsi (set by jl_call_in_state)
// Calls fptr(arg0) with FP/SIMD state preserved across the call.
#if defined(_CPU_AARCH64_)
// Saves all 32 NEON q-registers (512 bytes) + fpsr + fpcr.
__attribute__((naked, used)) static void jl_call_protect_fp_state(void)
{
    __asm__(
        // Save frame pointer and link register
        "  stp x29, x30, [sp, #-16]!\n"
        "  mov x29, sp\n"
        // Allocate 528 bytes for NEON state (32*16 + fpsr/fpcr + padding)
        "  sub sp, sp, #528\n"
        // Save fpsr and fpcr (x1 = fptr, so use x2/x3 as scratch)
        "  mrs x2, fpsr\n"
        "  mrs x3, fpcr\n"
        "  str w2, [sp, #512]\n"
        "  str w3, [sp, #516]\n"
        // Save q0-q31 (4 registers per instruction)
        "  mov x2, sp\n"
        "  st1 {v0.2d,  v1.2d,  v2.2d,  v3.2d},  [x2], #64\n"
        "  st1 {v4.2d,  v5.2d,  v6.2d,  v7.2d},  [x2], #64\n"
        "  st1 {v8.2d,  v9.2d,  v10.2d, v11.2d}, [x2], #64\n"
        "  st1 {v12.2d, v13.2d, v14.2d, v15.2d}, [x2], #64\n"
        "  st1 {v16.2d, v17.2d, v18.2d, v19.2d}, [x2], #64\n"
        "  st1 {v20.2d, v21.2d, v22.2d, v23.2d}, [x2], #64\n"
        "  st1 {v24.2d, v25.2d, v26.2d, v27.2d}, [x2], #64\n"
        "  st1 {v28.2d, v29.2d, v30.2d, v31.2d}, [x2], #64\n"
        // Call fptr(arg0): x0 = arg0 (untouched), x1 = fptr
        "  blr x1\n"
        // Restore fpsr and fpcr
        "  ldr w0, [sp, #512]\n"
        "  ldr w1, [sp, #516]\n"
        "  msr fpsr, x0\n"
        "  msr fpcr, x1\n"
        // Restore q0-q31 (4 registers per instruction)
        "  mov x0, sp\n"
        "  ld1 {v0.2d,  v1.2d,  v2.2d,  v3.2d},  [x0], #64\n"
        "  ld1 {v4.2d,  v5.2d,  v6.2d,  v7.2d},  [x0], #64\n"
        "  ld1 {v8.2d,  v9.2d,  v10.2d, v11.2d}, [x0], #64\n"
        "  ld1 {v12.2d, v13.2d, v14.2d, v15.2d}, [x0], #64\n"
        "  ld1 {v16.2d, v17.2d, v18.2d, v19.2d}, [x0], #64\n"
        "  ld1 {v20.2d, v21.2d, v22.2d, v23.2d}, [x0], #64\n"
        "  ld1 {v24.2d, v25.2d, v26.2d, v27.2d}, [x0], #64\n"
        "  ld1 {v28.2d, v29.2d, v30.2d, v31.2d}, [x0], #64\n"
        // Deallocate and return to jl_mach_restore_state
        "  add sp, sp, #528\n"
        "  ldp x29, x30, [sp], #16\n"
        "  ret\n"
    );
}
#elif defined(_CPU_X86_64_)
// Saves all FP/SIMD state: uses xsave64/xrstor64 if available (preserves
// AVX/AVX-512), otherwise falls back to fxsave64/fxrstor64 (SSE only).
__attribute__((naked, used)) static void jl_call_protect_fp_state(void)
{
    __asm__(
        // On entry: rdi = arg0, rsi = fptr.
        // rsp is 8 mod 16 (return addr pushed by jl_call_in_state).
        // Use rbp as frame pointer so we can do dynamic stack allocation.
        "  pushq %rbp\n"             // rsp now 0 mod 16
        "  movq %rsp, %rbp\n"
        // Check if xsave is available (jl_xsave_size > 0)
        "  movl _jl_xsave_size(%rip), %eax\n"
        "  testl %eax, %eax\n"
        "  jz 1f\n"
        // --- xsave path ---
        // Allocate xsave buffer on the stack, 64-byte aligned
        "  subq %rax, %rsp\n"
        "  andq $-64, %rsp\n"        // xsave requires 64-byte alignment
        // Zero the XSAVE header's XCOMP_BV (bytes 520-527) and reserved
        // fields (bytes 528-575) before xsave64. xsave does NOT write these
        // bytes, but xrstor requires them to be zero in standard format —
        // otherwise it raises #GP.
        "  movq $0, 520(%rsp)\n"
        "  movq $0, 528(%rsp)\n"
        "  movq $0, 536(%rsp)\n"
        "  movq $0, 544(%rsp)\n"
        "  movq $0, 552(%rsp)\n"
        "  movq $0, 560(%rsp)\n"
        "  movq $0, 568(%rsp)\n"
        "  movl $-1, %eax\n"         // save all state components
        "  movl $-1, %edx\n"
        "  xsave64 (%rsp)\n"
        "  callq *%rsi\n"            // fptr(arg0): rdi untouched, rsi = fptr
        "  movl $-1, %eax\n"
        "  movl $-1, %edx\n"
        "  xrstor64 (%rsp)\n"
        "  jmp 2f\n"
        // --- fxsave fallback path ---
        "1:\n"
        "  subq $512, %rsp\n"        // 512 bytes for fxsave, rsp now 0 mod 16
        "  andq $-16, %rsp\n"        // fxsave requires 16-byte alignment
        "  fxsave64 (%rsp)\n"
        "  callq *%rsi\n"
        "  fxrstor64 (%rsp)\n"
        // --- common epilogue ---
        "2:\n"
        "  movq %rbp, %rsp\n"
        "  popq %rbp\n"
        "  retq\n"
    );
}
#endif

// Assembly stub that restores GP registers from a host_thread_state_t
// saved on the stack, then jumps back to the original PC. This is the
// return address used when hijacking a thread to a C trampoline.
//
// On entry, sp points to a host_thread_state_t containing the full
// GP register state to restore.
// On AArch64, x16 (IP0) is not restored — it ends up holding saved_pc.
// LLVM reserves IP0 on Darwin, so it is never live at a safepoint poll.
#if defined(_CPU_AARCH64_)
// arm_thread_state64_t layout (272 bytes):
//   __x[29] at offset 0   (29 * 8 = 232 bytes, x0-x28)
//   __fp    at offset 232  (x29)
//   __lr    at offset 240  (x30)
//   __sp    at offset 248  (x31)
//   __pc    at offset 256
//   __cpsr  at offset 264
__attribute__((naked, used)) void jl_mach_restore_state(void)
{
    __asm__(
        "  .cfi_signal_frame\n"
        "  .cfi_def_cfa sp, 0\n"
        "  .cfi_offset lr, 256\n"   // saved __pc
        "  .cfi_offset sp, 248\n"   // saved __sp
        "  .cfi_offset x29, 232\n"  // saved __fp
        // sp points to the saved host_thread_state_t
        // Use x16 as the base pointer (scratch register, reserved by LLVM)
        "  mov x16, sp\n"
        // Restore cpsr (condition flags)
        "  ldr w17, [x16, #264]\n"
        "  msr nzcv, x17\n"
        // Restore x0-x15 (caller-saved)
        "  ldp x0,  x1,  [x16, #0]\n"
        "  ldp x2,  x3,  [x16, #16]\n"
        "  ldp x4,  x5,  [x16, #32]\n"
        "  ldp x6,  x7,  [x16, #48]\n"
        "  ldp x8,  x9,  [x16, #64]\n"
        "  ldp x10, x11, [x16, #80]\n"
        "  ldp x12, x13, [x16, #96]\n"
        "  ldp x14, x15, [x16, #112]\n"
        // Skip x18: platform register (reserved on macOS)
        // Skip x19-x28: callee-saved, preserved by the ABI-compliant
        // wrapper and trampoline.
        // Restore lr (x30) — was overwritten by jl_call_in_state
        "  ldr x30, [x16, #240]\n"
        // Skip x29 (fp) — callee-saved, already preserved
        // Restore sp, x17, and jump to saved pc.
        "  ldr x17, [x16, #248]\n"      // x17 = saved sp
        "  mov sp, x17\n"               // restore sp
        "  ldr x17, [x16, #136]\n"      // restore x17 (17*8)
        "  ldr x16, [x16, #256]\n"      // x16 = saved pc (base lost)
        "  br x16\n"                    // jump to saved pc
    );
}
#elif defined(_CPU_X86_64_)
// x86_thread_state64_t layout (168 bytes):
//   __rax at offset 0, __rbx at 8, __rcx at 16, __rdx at 24
//   __rdi at 32, __rsi at 40, __rbp at 48, __rsp at 56
//   __r8 at 64, __r9 at 72, __r10 at 80, __r11 at 88
//   __r12 at 96, __r13 at 104, __r14 at 112, __r15 at 120
//   __rip at 128, __rflags at 136
__attribute__((naked, used)) void jl_mach_restore_state(void)
{
    __asm__(
        "  .cfi_signal_frame\n"
        "  .cfi_def_cfa %rsp, 0\n"
        "  .cfi_offset %rip, 128\n" // saved __rip
        "  .cfi_offset %rsp, 56\n"  // saved __rsp
        // rsp points to the saved host_thread_state_t
        "  movq %rsp, %r11\n"         // use r11 as base pointer
        // Restore rflags
        "  pushq 136(%r11)\n"
        "  popfq\n"
        // Restore caller-saved GP registers
        "  movq 0(%r11), %rax\n"
        // skip rbx (8) — callee-saved
        "  movq 16(%r11), %rcx\n"
        "  movq 24(%r11), %rdx\n"
        "  movq 32(%r11), %rdi\n"
        "  movq 40(%r11), %rsi\n"
        // skip rbp (48), rsp (56) — callee-saved / restored separately
        "  movq 64(%r11), %r8\n"
        "  movq 72(%r11), %r9\n"
        "  movq 80(%r11), %r10\n"
        // skip r12 (96), r13 (104), r14 (112), r15 (120) — callee-saved
        // Restore rsp and r11, then jump to saved rip.
        // The saved rip was stored at orig_rsp - 136 by jl_call_in_state
        // (below the 128-byte red zone) to avoid corrupting it.
        "  movq 56(%r11), %rsp\n"    // restore original rsp
        "  movq 88(%r11), %r11\n"    // restore r11 (base lost)
        "  jmpq *-136(%rsp)\n"       // jump to saved rip below red zone
    );
}
#endif

// Set up state to call fptr(arg0) with full register preservation.
// GP state is saved on the target thread's stack (restored by jl_mach_restore_state).
// FP/SIMD state is saved by jl_call_protect_fp_state which runs on the target thread.
static void jl_call_in_state(host_thread_state_t *state, void (*fptr)(void), uintptr_t arg0)
{
#ifdef _CPU_X86_64_
    uintptr_t sp = state->__rsp;
    sp = (sp - 256) & ~(uintptr_t)15; // redzone and re-alignment
    sp -= sizeof(host_thread_state_t);
    sp &= ~(uintptr_t)15;
    memcpy((void*)sp, state, sizeof(host_thread_state_t));
    // Store saved rip below the original red zone (128 bytes) so that
    // jl_mach_restore_state can jump back without writing into the red zone.
    // This slot is at orig_rsp - 136, safely within our 256-byte gap.
    *(uintptr_t*)(state->__rsp - 136) = state->__rip;
    // Push the return address for the restore stub
    sp -= sizeof(void*);
    *(uintptr_t*)sp = (uintptr_t)&jl_mach_restore_state;
    state->__rsp = sp;
    state->__rip = (uint64_t)fptr;
    state->__rdi = arg0;
#elif defined(_CPU_AARCH64_)
    uintptr_t sp = state->__sp;
    sp = (sp - 256) & ~(uintptr_t)15;
    sp -= sizeof(host_thread_state_t);
    sp &= ~(uintptr_t)15;
    memcpy((void*)sp, state, sizeof(host_thread_state_t));
    state->__sp = sp;
    state->__pc = (uint64_t)fptr;
    state->__lr = (uintptr_t)&jl_mach_restore_state;
    state->__x[0] = arg0;
#else
#error "julia: call-in-state not supported on this platform"
#endif
}

static void segv_handler(int sig, siginfo_t *info, void *context)
{
    assert(sig == SIGSEGV || sig == SIGBUS);
    jl_jmp_buf *saferestore = jl_get_safe_restore();
    if (saferestore) { // restarting jl_ or jl_unwind_stepn
        jl_longjmp_in_state((host_thread_state_t*)jl_to_bt_context(context), *saferestore);
        return;
    }
    jl_task_t *ct = jl_get_current_task();
    if ((sig != SIGBUS || info->si_code == BUS_ADRERR) &&
    !(ct == NULL || ct->ptls == NULL || jl_atomic_load_relaxed(&ct->ptls->gc_state) == JL_GC_STATE_WAITING || ct->eh == NULL)
    && is_addr_on_stack(ct, info->si_addr)) { // stack overflow and not a BUS_ADRALN (alignment error)
        stack_overflow_warning();
    }
    sigdie_handler(sig, info, context);
}

// n.b. mach_exc_server expects us to define this symbol locally
/* The documentation for catch_exception_raise says: A return value of
 * KERN_SUCCESS indicates that the thread is to continue from the point of
 * exception. A return value of MIG_NO_REPLY indicates that the exception was
 * handled directly and the thread was restarted or terminated by the exception
 * handler. A return value of MIG_DESTROY_REQUEST causes the kernel to try
 * another exception handler (or terminate the thread). Any other value will
 * cause mach_msg_server to remove the task and thread port references.
 *
 * However MIG_DESTROY_REQUEST does not exist, not does it appear the source
 * code for mach_msg_server ever destroy those references (only the message
 * itself).
 */
kern_return_t catch_mach_exception_raise_state_identity(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt,
    int *flavor,
    thread_state_t old_state,
    mach_msg_type_number_t old_stateCnt,
    thread_state_t new_state,
    mach_msg_type_number_t *new_stateCnt)
{
    host_thread_state_t *state = (host_thread_state_t*)new_state;
    assert(old_stateCnt >= MACH_THREAD_STATE_COUNT);
    // Copy old state to new state — we'll modify new_state in place
    memcpy(new_state, old_state, old_stateCnt * sizeof(natural_t));
    *new_stateCnt = old_stateCnt;
#ifdef LLVMLIBUNWIND
    if (thread == mach_profiler_thread) {
        return profiler_segv_handler(exception_port, thread, task, exception, code, codeCnt,
                                     state, new_stateCnt);
    }
#endif
    jl_ptls_t ptls2 = NULL;
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    for (int16_t tid = 0; tid < nthreads; tid++) {
        jl_ptls_t _ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        if (_ptls2 == NULL)
            continue;
        if (pthread_mach_thread_np(_ptls2->system_id) == thread) {
            ptls2 = _ptls2;
            break;
        }
    }
    if (!ptls2) {
        // We don't know about this thread, let the kernel try another handler
        // instead. This shouldn't actually happen since we only register the
        // handler for the threads we know about.
        jl_safe_printf("ERROR: Exception handler triggered on unmanaged thread.\n");
        return KERN_INVALID_ARGUMENT;
    }
    if (ptls2->safe_restore) {
        jl_throw_in_state(ptls2, state, NULL);
        return KERN_SUCCESS;
    }
    if (jl_atomic_load_acquire(&ptls2->gc_state) == JL_GC_STATE_WAITING)
        return KERN_FAILURE;
    if (exception == EXC_ARITHMETIC) {
        jl_throw_in_state(ptls2, state, jl_diverror_exception);
        return KERN_SUCCESS;
    }
    assert(exception == EXC_BAD_ACCESS); // SIGSEGV or SIGBUS
    if (codeCnt < 2 || code[0] != KERN_PROTECTION_FAILURE) // SEGV_ACCERR or BUS_ADRERR or BUS_ADRALN
        return KERN_FAILURE;
    uint64_t fault_addr = code[1];
    unsigned int exc_count = HOST_EXCEPTION_STATE_COUNT;
    host_exception_state_t exc_state;
    kern_return_t ret = thread_get_state(thread, HOST_EXCEPTION_STATE, (thread_state_t)&exc_state, &exc_count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
    if (jl_addr_is_safepoint(fault_addr) && !is_write_fault(exc_state)) {
        // Hijack the faulting thread to handle the safepoint on its own
        // stack, using the same jl_set_gc_and_wait() codepath as Unix signals.
        jl_call_in_state(state, (void (*)(void))&mach_safepoint_trampoline, (uintptr_t)ptls2);
        return KERN_SUCCESS;
    }
    if (jl_atomic_load_relaxed(&ptls2->current_task)->eh == NULL)
        return KERN_FAILURE;
    jl_value_t *excpt;
    if (is_addr_on_stack(jl_atomic_load_relaxed(&ptls2->current_task), (void*)fault_addr)) {
        stack_overflow_warning();
        excpt = jl_stackovf_exception;
    }
    else if (is_write_fault(exc_state)) // false for alignment errors
        excpt = jl_readonlymemory_exception;
    else
        return KERN_FAILURE;
    jl_throw_in_state(ptls2, state, excpt);
    return KERN_SUCCESS;
}

//mach_exc_server expects us to define this symbol locally
kern_return_t catch_mach_exception_raise_state(
    mach_port_t exception_port,
    exception_type_t exception,
    const mach_exception_data_t code,
    mach_msg_type_number_t codeCnt,
    int *flavor,
    const thread_state_t old_state,
    mach_msg_type_number_t old_stateCnt,
    thread_state_t new_state,
    mach_msg_type_number_t *new_stateCnt)
{
    return KERN_INVALID_ARGUMENT; // we only use EXCEPTION_STATE_IDENTITY
}

//mach_exc_server expects us to define this symbol locally
kern_return_t catch_mach_exception_raise(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt)
{
    return KERN_INVALID_ARGUMENT; // we only use EXCEPTION_STATE_IDENTITY
}

static void attach_exception_port(thread_port_t thread, int segv_only)
{
    kern_return_t ret;
    // https://www.opensource.apple.com/source/xnu/xnu-2782.1.97/osfmk/man/thread_set_exception_ports.html
    exception_mask_t mask = EXC_MASK_BAD_ACCESS;
    if (!segv_only)
        mask |= EXC_MASK_ARITHMETIC;
    ret = thread_set_exception_ports(thread, mask, segv_port, EXCEPTION_STATE_IDENTITY | MACH_EXCEPTION_CODES, MACH_THREAD_STATE);
    HANDLE_MACH_ERROR("thread_set_exception_ports", ret);
}

static int jl_thread_suspend_and_get_state2(int tid, host_thread_state_t *ctx) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL) // this thread is not alive
        return 0;
    jl_task_t *ct2 = jl_atomic_load_relaxed(&ptls2->current_task);
    if (ct2 == NULL) // this thread is already dead
        return 0;

    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);

    kern_return_t ret = thread_suspend(thread);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // Do the actual sampling
    unsigned int count = MACH_THREAD_STATE_COUNT;
    memset(ctx, 0, sizeof(*ctx));

    // Get the state of the suspended thread
    ret = thread_get_state(thread, MACH_THREAD_STATE, (thread_state_t)ctx, &count);
    return 1;
}

static int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx)
{
    (void)timeout;
    host_thread_state_t state;
    if (!jl_thread_suspend_and_get_state2(tid, &state)) {
        return 0;
    }
    *ctx = *(unw_context_t*)&state;
    return 1;
}

void jl_thread_resume(int tid)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);
    kern_return_t ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

// Throw jl_interrupt_exception if the master thread is in a signal async region
// or if SIGINT happens too often.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);

    kern_return_t ret = thread_suspend(thread);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // This aborts `sleep` and other syscalls.
    ret = thread_abort(thread);
    HANDLE_MACH_ERROR("thread_abort", ret);

    jl_safepoint_enable_sigint();
    int force = jl_check_force_sigint();
    if (force || (!ptls2->defer_signal && ptls2->io_wait)) {
        jl_safepoint_consume_sigint();
        if (force)
            jl_safe_printf("WARNING: Force throwing a SIGINT\n");
        jl_clear_force_sigint();
        jl_throw_in_thread(ptls2, thread, jl_interrupt_exception);
    }
    else {
        jl_wake_libuv();
    }

    ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

static void jl_exit_thread0_cb(int signo)
{
    jl_fprint_critical_error(ios_safe_stderr, signo, 0, NULL, jl_current_task);
    jl_atexit_hook(128);
    jl_raise(signo);
}

static void jl_exit_thread0(int signo, jl_bt_element_t *bt_data, size_t bt_size)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);

    host_thread_state_t state;
    if (!jl_thread_suspend_and_get_state2(0, &state)) {
        // thread 0 is gone? just do the signal ourself
        jl_raise(signo);
    }

    // This aborts `sleep` and other syscalls.
    kern_return_t ret = thread_abort(thread);
    HANDLE_MACH_ERROR("thread_abort", ret);

    ptls2->bt_size = bt_size; // <= JL_MAX_BT_SIZE
    memcpy(ptls2->bt_data, bt_data, ptls2->bt_size * sizeof(bt_data[0]));

    jl_noreturn_call_in_state(&state, (void (*)(void))&jl_exit_thread0_cb, signo, 0);
    unsigned int count = MACH_THREAD_STATE_COUNT;
    ret = thread_set_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);

    ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

static int profile_started = 0;
mach_timespec_t timerprof;
static pthread_t profiler_thread;
clock_serv_t clk;
static mach_port_t profile_port = 0;

#ifdef LLVMLIBUNWIND
volatile static int forceDwarf = -2;
static unw_context_t profiler_uc;

static kern_return_t profiler_segv_handler(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt,
    host_thread_state_t *state,
    mach_msg_type_number_t *new_stateCnt)
{
    assert(thread == mach_profiler_thread);

    // Not currently unwinding. Raise regular segfault
    if (forceDwarf == -2)
        return KERN_FAILURE;

    if (forceDwarf == 0)
        forceDwarf = 1;
    else
        forceDwarf = -1;

#ifdef _CPU_X86_64_
    // don't change cs fs gs rflags
    uint64_t cs = state->__cs;
    uint64_t fs = state->__fs;
    uint64_t gs = state->__gs;
    uint64_t rflags = state->__rflags;
#elif defined(_CPU_AARCH64_)
    uint64_t cpsr = state->__cpsr;
#else
#error Unknown CPU
#endif

    memcpy(state, &profiler_uc, sizeof(*state));

#ifdef _CPU_X86_64_
    state->__cs = cs;
    state->__fs = fs;
    state->__gs = gs;
    state->__rflags = rflags;
#else
    state->__cpsr = cpsr;
#endif

    *new_stateCnt = MACH_THREAD_STATE_COUNT;
    return KERN_SUCCESS;
}
#endif

// WARNING: we are unable to handle sigsegv while the dlsymlock is held
static int jl_lock_profile_mach(int dlsymlock)
{
    jl_lock_profile();
    // workaround for old keymgr bugs
    void *unused = NULL;
    int keymgr_locked = _keymgr_get_and_lock_processwide_ptr_2(KEYMGR_GCC3_DW2_OBJ_LIST, &unused) == 0;
    // workaround for new dlsym4 bugs in the workaround for dlsym bugs: _dyld_atfork_prepare
    // acquires its locks in the wrong order, but fortunately we happen to able to guard it
    // with this call to force it to prevent that TSAN violation from causing a deadlock
    if (dlsymlock && _dyld_dlopen_atfork_prepare != NULL && _dyld_dlopen_atfork_parent != NULL)
        _dyld_dlopen_atfork_prepare();
    // workaround for new dlsym4 bugs (API and bugs introduced circa macOS 12.1)
    if (dlsymlock && _dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
        _dyld_atfork_prepare();
    return keymgr_locked;
}

static void jl_unlock_profile_mach(int dlsymlock, int keymgr_locked)
{
    if (dlsymlock && _dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
        _dyld_atfork_parent();
    if (dlsymlock && _dyld_dlopen_atfork_prepare != NULL && _dyld_dlopen_atfork_parent != NULL)
        _dyld_dlopen_atfork_parent();
    if (keymgr_locked)
        _keymgr_unlock_processwide_ptr(KEYMGR_GCC3_DW2_OBJ_LIST);
    jl_unlock_profile();
}

int jl_thread_suspend(int16_t tid, bt_context_t *ctx)
{
    int lockret = jl_lock_profile_mach(1);
    int success = jl_thread_suspend_and_get_state(tid, 1, ctx);
    jl_unlock_profile_mach(1, lockret);
    return success;
}

void jl_with_stackwalk_lock(void (*f)(void*), void *ctx)
{
    int lockret = jl_lock_profile_mach(1);
    f(ctx);
    jl_unlock_profile_mach(1, lockret);
}

// assumes holding `jl_lock_profile_mach`
void jl_profile_thread_mach(int tid)
{
    // if there is no space left, return early
    if (jl_profile_is_buffer_full()) {
        jl_profile_stop_timer();
        return;
    }
    if (_dyld_dlopen_atfork_prepare != NULL && _dyld_dlopen_atfork_parent != NULL)
        _dyld_dlopen_atfork_prepare();
    if (_dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
        _dyld_atfork_prepare(); // briefly acquire the dlsym lock
    host_thread_state_t state;
    int valid_thread = jl_thread_suspend_and_get_state2(tid, &state);
    unw_context_t *uc = (unw_context_t*)&state;
    if (_dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
        _dyld_atfork_parent(); // quickly release the dlsym lock
    if (_dyld_dlopen_atfork_prepare != NULL && _dyld_dlopen_atfork_parent != NULL)
        _dyld_dlopen_atfork_parent();
    if (!valid_thread)
        return;
    if (profile_running) {
#ifdef LLVMLIBUNWIND
        /*
            *  Unfortunately compact unwind info is incorrectly generated for quite a number of
            *  libraries by quite a large number of compilers. We can fall back to DWARF unwind info
            *  in some cases, but in quite a number of cases (especially libraries not compiled in debug
            *  mode, only the compact unwind info may be available). Even more unfortunately, there is no
            *  way to detect such bogus compact unwind info (other than noticing the resulting segfault).
            *  What we do here is ugly, but necessary until the compact unwind info situation improves.
            *  We try to use the compact unwind info and if that results in a segfault, we retry with DWARF info.
            *  Note that in a small number of cases this may result in bogus stack traces, but at least the topmost
            *  entry will always be correct, and the number of cases in which this is an issue is rather small.
            *  Other than that, this implementation is not incorrect as the other thread is paused while we are profiling
            *  and during stack unwinding we only ever read memory, but never write it.
            */

        forceDwarf = 0;
        unw_getcontext(&profiler_uc); // will resume from this point if the next lines segfault at any point

        if (forceDwarf == 0) {
            // Save the backtrace
            profile_bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)profile_bt_data_prof + profile_bt_size_cur, profile_bt_size_max - profile_bt_size_cur - 1, uc, NULL);
        }
        else if (forceDwarf == 1) {
            profile_bt_size_cur += rec_backtrace_ctx_dwarf((jl_bt_element_t*)profile_bt_data_prof + profile_bt_size_cur, profile_bt_size_max - profile_bt_size_cur - 1, uc, NULL);
        }
        else if (forceDwarf == -1) {
            jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
        }

        forceDwarf = -2;
#else
        profile_bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)profile_bt_data_prof + profile_bt_size_cur, profile_bt_size_max - profile_bt_size_cur - 1, uc, NULL);
#endif
        jl_ptls_t ptls = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];

        // store threadid but add 1 as 0 is preserved to indicate end of block
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = ptls->tid + 1;

        // store task id (never null)
        profile_bt_data_prof[profile_bt_size_cur++].jlvalue = (jl_value_t*)jl_atomic_load_relaxed(&ptls->current_task);

        // store cpu cycle clock
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = cycleclock();

        // store whether thread is sleeping (don't ever encode a state as `0` since is preserved to indicate end of block)
        int state = jl_atomic_load_relaxed(&ptls->sleep_check_state) == 0 ? PROFILE_STATE_THREAD_NOT_SLEEPING : PROFILE_STATE_THREAD_SLEEPING;
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = state;

        // Mark the end of this block with two 0's
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;
        profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;
    }
    // We're done! Resume the thread.
    jl_thread_resume(tid);
}

void *mach_profile_listener(void *arg)
{
    (void)arg;
    const int max_size = 512;
    attach_exception_port(mach_thread_self(), 1);
#ifdef LLVMLIBUNWIND
    mach_profiler_thread = mach_thread_self();
#endif
    mig_reply_error_t *bufRequest = (mig_reply_error_t*)malloc_s(max_size);
    while (1) {
        kern_return_t ret = mach_msg(&bufRequest->Head, MACH_RCV_MSG,
                                     0, max_size, profile_port,
                                     MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        HANDLE_MACH_ERROR("mach_msg", ret);
        // sample each thread, round-robin style in reverse order
        // (so that thread zero gets notified last)
        int keymgr_locked = jl_lock_profile_mach(0);
        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        if (profile_all_tasks) {
            // Don't take the stackwalk lock here since it's already taken in `jl_rec_backtrace`
            jl_profile_task();
        }
        else {
            int *randperm = profile_get_randperm(nthreads);
            for (int idx = nthreads; idx-- > 0; ) {
                // Stop the threads in random order.
                int i = randperm[idx];
                jl_profile_thread_mach(i);
            }
        }
        jl_unlock_profile_mach(0, keymgr_locked);
        if (profile_running) {
            jl_check_profile_autostop();
            // Reset the alarm
            kern_return_t ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
            HANDLE_MACH_ERROR("clock_alarm", ret)
        }
    }
}


JL_DLLEXPORT int jl_profile_start_timer(uint8_t all_tasks)
{
    kern_return_t ret;
    if (!profile_started) {
        mach_port_t self = mach_task_self();

        ret = host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, (clock_serv_t *)&clk);
        HANDLE_MACH_ERROR("host_get_clock_service", ret);

        ret = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &profile_port);
        HANDLE_MACH_ERROR("mach_port_allocate", ret);

        // Alright, create a thread to serve as the listener for exceptions
        pthread_attr_t attr;
        if (pthread_attr_init(&attr) != 0) {
            jl_error("pthread_attr_init failed");
        }
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&profiler_thread, &attr, mach_profile_listener, NULL) != 0) {
            jl_error("pthread_create failed");
        }
        pthread_attr_destroy(&attr);

        profile_started = 1;
    }

    timerprof.tv_sec = nsecprof/GIGA;
    timerprof.tv_nsec = nsecprof%GIGA;

    profile_running = 1;
    profile_all_tasks = all_tasks;
    // ensure the alarm is running
    ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
    HANDLE_MACH_ERROR("clock_alarm", ret);

    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    uv_mutex_lock(&bt_data_prof_lock);
    profile_running = 0;
    profile_all_tasks = 0;
    uv_mutex_unlock(&bt_data_prof_lock);
}

// The mprotect implementation in signals-unix.c does not work on macOS/aarch64, as mentioned.
// This implementation comes from dotnet, but is similarly dependent on undocumented behavior of the OS.
// Copyright (c) .NET Foundation and Contributors
// MIT LICENSE
JL_DLLEXPORT void jl_membarrier(void) {
    uintptr_t sp;
    uintptr_t registerValues[128];
    kern_return_t machret;

    // Iterate through each of the threads in the list.
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    for (int tid = 0; tid < nthreads; tid++) {
        jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        thread_act_t thread = pthread_mach_thread_np(ptls2->system_id);
        if (__builtin_available (macOS 10.14, iOS 12, tvOS 9, *))
        {
            // Request the threads pointer values to force the thread to emit a memory barrier
            size_t registers = 128;
            machret = thread_get_register_pointer_values(thread, &sp, &registers, registerValues);
        }
        else
        {
            // fallback implementation for older OS versions
#if defined(_CPU_X86_64_)
            x86_thread_state64_t threadState;
            mach_msg_type_number_t count = x86_THREAD_STATE64_COUNT;
            machret = thread_get_state(thread, x86_THREAD_STATE64, (thread_state_t)&threadState, &count);
#elif defined(_CPU_AARCH64_)
            arm_thread_state64_t threadState;
            mach_msg_type_number_t count = ARM_THREAD_STATE64_COUNT;
            machret = thread_get_state(thread, ARM_THREAD_STATE64, (thread_state_t)&threadState, &count);
#else
            #error Unexpected architecture
#endif
        }

        if (machret == KERN_INSUFFICIENT_BUFFER_SIZE)
        {
            HANDLE_MACH_ERROR("thread_get_register_pointer_values()", machret);
        }
    }
}
