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

// Profiler control variables //
static volatile intptr_t *bt_data_prof = NULL;
static volatile size_t bt_size_max = 0;
static volatile size_t bt_size_cur = 0;
static volatile uint64_t nsecprof = 0;
static volatile int running = 0;
static const    uint64_t GIGA = 1000000000ULL;
// Timers to take samples at intervals
JL_DLLEXPORT void jl_profile_stop_timer(void);
JL_DLLEXPORT int jl_profile_start_timer(void);

static uint64_t jl_last_sigint_trigger = 0;
static uint64_t jl_disable_sigint_time = 0;
static void jl_clear_force_sigint(void)
{
    jl_last_sigint_trigger = 0;
}

static int jl_check_force_sigint(void)
{
    static double accum_weight = 0;
    uint64_t cur_time = jl_hrtime();
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
    return jl_disable_sigint_time && jl_disable_sigint_time > jl_hrtime();
}

static int exit_on_sigint = 0;
JL_DLLEXPORT void jl_exit_on_sigint(int on)
{
    exit_on_sigint = on;
}

static uintptr_t jl_get_pc_from_ctx(const void *_ctx);
void jl_show_sigill(void *_ctx);
static size_t jl_safe_read_mem(const volatile char *ptr, char *out, size_t len)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_jmp_buf *old_buf = ptls->safe_restore;
    jl_jmp_buf buf;
    ptls->safe_restore = &buf;
    volatile size_t i = 0;
    if (!jl_setjmp(buf, 0)) {
        for (;i < len;i++) {
            out[i] = ptr[i];
        }
    }
    ptls->safe_restore = old_buf;
    return i;
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
#elif defined(_OS_DARWIN_)
    return ((ucontext64_t*)_ctx)->uc_mcontext64->__ss.__rip;
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

// what to do on a critical error
void jl_critical_error(int sig, bt_context_t *context, uintptr_t *bt_data, size_t *bt_size)
{
    // This function is not allowed to reference any TLS variables.
    // We need to explicitly pass in the TLS buffer pointer when
    // we make `jl_filename` and `jl_lineno` thread local.
    size_t i, n = *bt_size;
    if (sig)
        jl_safe_printf("\nsignal (%d): %s\n", sig, strsignal(sig));
    jl_safe_printf("in expression starting at %s:%d\n", jl_filename, jl_lineno);
    if (context)
        *bt_size = n = rec_backtrace_ctx(bt_data, JL_MAX_BT_SIZE, context);
    for (i = 0; i < n; i++)
        jl_gdblookup(bt_data[i] - 1);
    gc_debug_print_status();
    gc_debug_critical_error();
}

///////////////////////
// Utility functions //
///////////////////////
JL_DLLEXPORT int jl_profile_init(size_t maxsize, uint64_t delay_nsec)
{
    bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);
    bt_data_prof = (intptr_t*) calloc(maxsize, sizeof(intptr_t));
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

#ifdef __cplusplus
}
#endif
