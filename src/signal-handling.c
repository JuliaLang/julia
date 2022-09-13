// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <unistd.h>
#include <sys/mman.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <threading.h>

// Profiler control variables
// Note: these "static" variables are also used in "signals-*.c"
static volatile jl_bt_element_t *bt_data_prof = NULL;
static volatile size_t bt_size_max = 0;
static volatile size_t bt_size_cur = 0;
static volatile uint64_t nsecprof = 0;
static volatile int running = 0;
static const    uint64_t GIGA = 1000000000ULL;
// Timers to take samples at intervals
JL_DLLEXPORT void jl_profile_stop_timer(void);
JL_DLLEXPORT int jl_profile_start_timer(void);

///////////////////////
// Utility functions //
///////////////////////
JL_DLLEXPORT int jl_profile_init(size_t maxsize, uint64_t delay_nsec)
{
    bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);
    bt_data_prof = (jl_bt_element_t*) calloc(maxsize, sizeof(jl_bt_element_t));
    if (bt_data_prof == NULL && maxsize > 0)
        return -1;
    bt_size_cur = 0;
    return 0;
}

JL_DLLEXPORT uint8_t *jl_profile_get_data(void)
{
    return (uint8_t*) bt_data_prof;
}

JL_DLLEXPORT size_t jl_profile_len_data(void)
{
    return bt_size_cur;
}

JL_DLLEXPORT size_t jl_profile_maxlen_data(void)
{
    return bt_size_max;
}

JL_DLLEXPORT uint64_t jl_profile_delay_nsec(void)
{
    return nsecprof;
}

JL_DLLEXPORT void jl_profile_clear_data(void)
{
    bt_size_cur = 0;
}

JL_DLLEXPORT int jl_profile_is_running(void)
{
    return running;
}

// Any function that acquires this lock must be either a unmanaged thread
// or in the GC safe region and must NOT allocate anything through the GC
// while holding this lock.
// Certain functions in this file might be called from an unmanaged thread
// and cannot have any interaction with the julia runtime
// They also may be re-entrant, and operating while threads are paused, so we
// separately manage the re-entrant count behavior for safety across platforms
// Note that we cannot safely upgrade read->write
uv_rwlock_t debuginfo_asyncsafe;
#ifndef _OS_WINDOWS_
pthread_key_t debuginfo_asyncsafe_held;
#else
DWORD debuginfo_asyncsafe_held;
#endif

void jl_init_profile_lock(void)
{
    uv_rwlock_init(&debuginfo_asyncsafe);
#ifndef _OS_WINDOWS_
    pthread_key_create(&debuginfo_asyncsafe_held, NULL);
#else
    debuginfo_asyncsafe_held = TlsAlloc();
#endif
}

uintptr_t jl_lock_profile_rd_held(void)
{
#ifndef _OS_WINDOWS_
    return (uintptr_t)pthread_getspecific(debuginfo_asyncsafe_held);
#else
    return (uintptr_t)TlsGetValue(debuginfo_asyncsafe_held);
#endif
}

void jl_lock_profile(void)
{
    uintptr_t held = jl_lock_profile_rd_held();
    if (held++ == 0)
        uv_rwlock_rdlock(&debuginfo_asyncsafe);
#ifndef _OS_WINDOWS_
    pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
    TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
}

JL_DLLEXPORT void jl_unlock_profile(void)
{
    uintptr_t held = jl_lock_profile_rd_held();
    assert(held);
    if (--held == 0)
        uv_rwlock_rdunlock(&debuginfo_asyncsafe);
#ifndef _OS_WINDOWS_
    pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
    TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
}

void jl_lock_profile_wr(void)
{
    uv_rwlock_wrlock(&debuginfo_asyncsafe);
}

void jl_unlock_profile_wr(void)
{
    uv_rwlock_wrunlock(&debuginfo_asyncsafe);
}


#ifndef _OS_WINDOWS_
static uint64_t profile_cong_rng_seed = 0;
static int *profile_round_robin_thread_order = NULL;
static int profile_round_robin_thread_order_size = 0;

static void jl_shuffle_int_array_inplace(int *carray, int size, uint64_t *seed)
{
    // The "modern Fisher–Yates shuffle" - O(n) algorithm
    // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
    for (int i = size; i-- > 1; ) {
        uint64_t unbias = UINT64_MAX; // slightly biased, but i is very small
        size_t j = cong(i, unbias, seed);
        uint64_t tmp = carray[j];
        carray[j] = carray[i];
        carray[i] = tmp;
    }
}


static int *profile_get_randperm(int size)
{
    if (profile_round_robin_thread_order_size < size) {
        free(profile_round_robin_thread_order);
        profile_round_robin_thread_order = (int*)malloc_s(size * sizeof(int));
        for (int i = 0; i < size; i++)
            profile_round_robin_thread_order[i] = i;
        profile_round_robin_thread_order_size = size;
        profile_cong_rng_seed = jl_rand();
    }
    jl_shuffle_int_array_inplace(profile_round_robin_thread_order, size, &profile_cong_rng_seed);
    return profile_round_robin_thread_order;
}
#endif


JL_DLLEXPORT int jl_profile_is_buffer_full(void)
{
    // declare buffer full if there isn't enough room to take samples across all threads
    #if defined(_OS_WINDOWS_)
        uint64_t nthreads = 1; // windows only profiles the main thread
    #else
        uint64_t nthreads = jl_n_threads;
    #endif
    // the `+ 6` is for the two block terminators `0` plus 4 metadata entries
    return bt_size_cur + (((JL_BT_MAX_ENTRY_SIZE + 1) + 6) * nthreads) > bt_size_max;
}

static uint64_t jl_last_sigint_trigger = 0;
static uint64_t jl_disable_sigint_time = 0;
static void jl_clear_force_sigint(void)
{
    jl_last_sigint_trigger = 0;
}

static int jl_check_force_sigint(void)
{
    static double accum_weight = 0;
    uint64_t cur_time = uv_hrtime();
    uint64_t dt = cur_time - jl_last_sigint_trigger;
    uint64_t last_t = jl_last_sigint_trigger;
    jl_last_sigint_trigger = cur_time;
    if (last_t == 0) {
        accum_weight = 0;
        return 0;
    }
    double new_weight = accum_weight * exp(-(dt / 1e9)) + 0.3;
    if (!isnormal(new_weight))
        new_weight = 0;
    accum_weight = new_weight;
    if (new_weight > 1) {
        jl_disable_sigint_time = cur_time + (uint64_t)0.5e9;
        return 1;
    }
    jl_disable_sigint_time = 0;
    return 0;
}

#ifndef _OS_WINDOWS_
// Not thread local, should only be accessed by the signal handler thread.
static volatile int jl_sigint_passed = 0;
static sigset_t jl_sigint_sset;
#endif

static int jl_ignore_sigint(void)
{
    // On Unix, we get the SIGINT before the debugger which makes it very
    // hard to interrupt a running process in the debugger with `Ctrl-C`.
    // Manually raise a `SIGINT` on current thread with the signal temporarily
    // unblocked and use it's behavior to decide if we need to handle the signal.
#ifndef _OS_WINDOWS_
    jl_sigint_passed = 0;
    pthread_sigmask(SIG_UNBLOCK, &jl_sigint_sset, NULL);
    // This can swallow an external `SIGINT` but it's not an issue
    // since we don't deliver the same number of signals anyway.
    pthread_kill(pthread_self(), SIGINT);
    pthread_sigmask(SIG_BLOCK, &jl_sigint_sset, NULL);
    if (!jl_sigint_passed)
        return 1;
#endif
    // Force sigint requires pressing `Ctrl-C` repeatedly.
    // Ignore sigint for a short time after that to avoid rethrowing sigint too
    // quickly again. (Code that has this issue is inherently racy but this is
    // an interactive feature anyway.)
    return jl_disable_sigint_time && jl_disable_sigint_time > uv_hrtime();
}

static int exit_on_sigint = 0;
JL_DLLEXPORT void jl_exit_on_sigint(int on)
{
    exit_on_sigint = on;
}

static uintptr_t jl_get_pc_from_ctx(const void *_ctx);
void jl_show_sigill(void *_ctx);
#if defined(_CPU_X86_64_) || defined(_CPU_X86_) \
    || (defined(_OS_LINUX_) && defined(_CPU_AARCH64_)) \
    || (defined(_OS_LINUX_) && defined(_CPU_ARM_))
static size_t jl_safe_read_mem(const volatile char *ptr, char *out, size_t len)
{
    jl_jmp_buf *old_buf = jl_get_safe_restore();
    jl_jmp_buf buf;
    jl_set_safe_restore(&buf);
    volatile size_t i = 0;
    if (!jl_setjmp(buf, 0)) {
        for (; i < len; i++) {
            out[i] = ptr[i];
        }
    }
    jl_set_safe_restore(old_buf);
    return i;
}
#endif

static double profile_autostop_time = -1.0;
static double profile_peek_duration = 1.0; // seconds

double jl_get_profile_peek_duration(void)
{
    return profile_peek_duration;
}
void jl_set_profile_peek_duration(double t)
{
    profile_peek_duration = t;
}

uintptr_t profile_show_peek_cond_loc;
JL_DLLEXPORT void jl_set_peek_cond(uintptr_t cond)
{
    profile_show_peek_cond_loc = cond;
}

static void jl_check_profile_autostop(void)
{
    if ((profile_autostop_time != -1.0) && (jl_hrtime() > profile_autostop_time)) {
        profile_autostop_time = -1.0;
        jl_profile_stop_timer();
        jl_safe_printf("\n==============================================================\n");
        jl_safe_printf("Profile collected. A report will print at the next yield point\n");
        jl_safe_printf("==============================================================\n\n");
        uv_async_send((uv_async_t*)profile_show_peek_cond_loc);
    }
}

#if defined(_WIN32)
#include "signals-win.c"
#else
#include "signals-unix.c"
#endif

static uintptr_t jl_get_pc_from_ctx(const void *_ctx)
{
#if defined(_OS_LINUX_) && defined(_CPU_X86_64_)
    return ((ucontext_t*)_ctx)->uc_mcontext.gregs[REG_RIP];
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_64_)
    return ((ucontext_t*)_ctx)->uc_mcontext.mc_rip;
#elif defined(_OS_LINUX_) && defined(_CPU_X86_)
    return ((ucontext_t*)_ctx)->uc_mcontext.gregs[REG_EIP];
#elif defined(_OS_FREEBSD_) && defined(_CPU_X86_)
    return ((ucontext_t*)_ctx)->uc_mcontext.mc_eip;
#elif defined(_OS_DARWIN_) && defined(_CPU_x86_64_)
    return ((ucontext64_t*)_ctx)->uc_mcontext64->__ss.__rip;
#elif defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
    return ((ucontext64_t*)_ctx)->uc_mcontext64->__ss.__pc;
#elif defined(_OS_WINDOWS_) && defined(_CPU_X86_)
    return ((CONTEXT*)_ctx)->Eip;
#elif defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    return ((CONTEXT*)_ctx)->Rip;
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
    return ((ucontext_t*)_ctx)->uc_mcontext.pc;
#elif defined(_OS_LINUX_) && defined(_CPU_ARM_)
    return ((ucontext_t*)_ctx)->uc_mcontext.arm_pc;
#else
    // TODO for PPC
    return 0;
#endif
}

void jl_show_sigill(void *_ctx)
{
    char *pc = (char*)jl_get_pc_from_ctx(_ctx);
    // unsupported platform
    if (!pc)
        return;
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
    uint8_t inst[15]; // max length of x86 instruction
    size_t len = jl_safe_read_mem(pc, (char*)inst, sizeof(inst));
    // ud2
    if (len >= 2 && inst[0] == 0x0f && inst[1] == 0x0b) {
        jl_safe_printf("Unreachable reached at %p\n", (void*)pc);
    }
    else {
        jl_safe_printf("Invalid instruction at %p: ", (void*)pc);
        for (int i = 0;i < len;i++) {
            if (i == 0) {
                jl_safe_printf("0x%02" PRIx8, inst[i]);
            }
            else {
                jl_safe_printf(", 0x%02" PRIx8, inst[i]);
            }
        }
        jl_safe_printf("\n");
    }
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
    uint32_t inst = 0;
    size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
    if (len < 4)
        jl_safe_printf("Fault when reading instruction: %d bytes read\n", (int)len);
    if (inst == 0xd4200020) { // brk #0x1
        // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
        jl_safe_printf("Unreachable reached at %p\n", pc);
    }
    else {
        jl_safe_printf("Invalid instruction at %p: 0x%08" PRIx32 "\n", pc, inst);
    }
#elif defined(_OS_LINUX_) && defined(_CPU_ARM_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    if (ctx->uc_mcontext.arm_cpsr & (1 << 5)) {
        // Thumb
        uint16_t inst[2] = {0, 0};
        size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
        if (len < 2)
            jl_safe_printf("Fault when reading Thumb instruction: %d bytes read\n", (int)len);
        // LLVM and GCC uses different code for the trap...
        if (inst[0] == 0xdefe || inst[0] == 0xdeff) {
            // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
            jl_safe_printf("Unreachable reached in Thumb mode at %p: 0x%04" PRIx16 "\n",
                           (void*)pc, inst[0]);
        }
        else {
            jl_safe_printf("Invalid Thumb instruction at %p: 0x%04" PRIx16 ", 0x%04" PRIx16 "\n",
                           (void*)pc, inst[0], inst[1]);
        }
    }
    else {
        uint32_t inst = 0;
        size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
        if (len < 4)
            jl_safe_printf("Fault when reading instruction: %d bytes read\n", (int)len);
        // LLVM and GCC uses different code for the trap...
        if (inst == 0xe7ffdefe || inst == 0xe7f000f0) {
            // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
            jl_safe_printf("Unreachable reached in ARM mode at %p: 0x%08" PRIx32 "\n",
                           (void*)pc, inst);
        }
        else {
            jl_safe_printf("Invalid ARM instruction at %p: 0x%08" PRIx32 "\n", (void*)pc, inst);
        }
    }
#else
    // TODO for PPC
    (void)_ctx;
#endif
}

// what to do on a critical error on a thread
void jl_critical_error(int sig, bt_context_t *context, jl_task_t *ct)
{
    jl_bt_element_t *bt_data = ct ? ct->ptls->bt_data : NULL;
    size_t *bt_size = ct ? &ct->ptls->bt_size : NULL;
    size_t i, n = ct ? *bt_size : 0;
    if (sig) {
        // kill this task, so that we cannot get back to it accidentally (via an untimely ^C or jlbacktrace in jl_exit)
        jl_set_safe_restore(NULL);
        if (ct) {
            ct->gcstack = NULL;
            ct->eh = NULL;
            ct->excstack = NULL;
            ct->ptls->locks.len = 0;
            ct->ptls->in_pure_callback = 0;
            ct->ptls->in_finalizer = 1;
            ct->world_age = 1;
        }
#ifndef _OS_WINDOWS_
        sigset_t sset;
        sigemptyset(&sset);
        // n.b. In `abort()`, Apple's libSystem "helpfully" blocks all signals
        // on all threads but SIGABRT. But we also don't know what the thread
        // was doing, so unblock all critical signals so that they will crash
        // hard, and not just get stuck.
        sigaddset(&sset, SIGSEGV);
        sigaddset(&sset, SIGBUS);
        sigaddset(&sset, SIGILL);
        // also unblock fatal signals now, so we won't get back here twice
        sigaddset(&sset, SIGTERM);
        sigaddset(&sset, SIGABRT);
        sigaddset(&sset, SIGQUIT);
        // and the original signal is now fatal too, in case it wasn't
        // something already listed (?)
        if (sig != SIGINT)
            sigaddset(&sset, sig);
        pthread_sigmask(SIG_UNBLOCK, &sset, NULL);
#endif
        jl_safe_printf("\n[%d] signal (%d): %s\n", getpid(), sig, strsignal(sig));
    }
    jl_safe_printf("in expression starting at %s:%d\n", jl_filename, jl_lineno);
    if (context && ct) {
        // Must avoid extended backtrace frames here unless we're sure bt_data
        // is properly rooted.
        *bt_size = n = rec_backtrace_ctx(bt_data, JL_MAX_BT_SIZE, context, NULL);
    }
    for (i = 0; i < n; i += jl_bt_entry_size(bt_data + i)) {
        jl_print_bt_entry_codeloc(bt_data + i);
    }
    jl_gc_debug_print_status();
    jl_gc_debug_critical_error();
}

#ifdef __cplusplus
}
#endif
