// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// 0: no sigint is pending
// 1: at least one sigint is pending, only the sigint page is enabled.
// 2: at least one sigint is pending, both safepoint pages are enabled.
JL_DLLEXPORT sig_atomic_t jl_signal_pending = 0;
uint32_t jl_gc_running = 0;
char *jl_safepoint_pages = NULL;
// The number of safepoints enabled on the three pages.
// The first page, is the SIGINT page, only used by the master thread.
// The second page, is the GC page for the master thread, this is where
// the `safepoint` tls pointer points to for the master thread.
// The third page is the GC page for the other threads. The thread's
// `safepoint` tls pointer points the beginning of this page + `sizeof(size_t)`
// so that both safepoint load and pending signal load falls in this page.
// The initialization of the `safepoint` pointer is done `ti_initthread`
// in `threading.c`.
uint8_t jl_safepoint_enable_cnt[3] = {0, 0, 0};

// This lock should be acquired before enabling/disabling the safepoint
// or accessing one of the following variables:
//
// * jl_gc_running
// * jl_signal_pending
// * jl_safepoint_enable_cnt
//
// Additionally accessing `jl_gc_running` should use acquire/release
// load/store so that threads waiting for the GC doesn't have to also
// fight on the safepoint lock...
//
// Acquiring and releasing this lock should use the `jl_mutex_*_nogc` functions
jl_mutex_t safepoint_lock;

static void jl_safepoint_enable(int idx)
{
    // safepoint_lock should be held
    assert(0 <= idx && idx < 3);
    if (jl_safepoint_enable_cnt[idx]++ != 0) {
        // We expect this to be enabled at most twice
        // one for the GC, one for SIGINT.
        // Update this if this is not the case anymore in the future.
        assert(jl_safepoint_enable_cnt[idx] <= 2);
        return;
    }
    // Now that we are requested to mprotect the page and it wasn't already.
    char *pageaddr = jl_safepoint_pages + jl_page_size * idx;
#ifdef _OS_WINDOWS_
    DWORD old_prot;
    VirtualProtect(pageaddr, jl_page_size, PAGE_NOACCESS, &old_prot);
#else
    mprotect(pageaddr, jl_page_size, PROT_NONE);
#endif
}

static void jl_safepoint_disable(int idx)
{
    // safepoint_lock should be held
    assert(0 <= idx && idx < 3);
    if (--jl_safepoint_enable_cnt[idx] != 0) {
        assert(jl_safepoint_enable_cnt[idx] > 0);
        return;
    }
    // Now that we are requested to un-mprotect the page and no one else
    // want it to be kept protected.
    char *pageaddr = jl_safepoint_pages + jl_page_size * idx;
#ifdef _OS_WINDOWS_
    DWORD old_prot;
    VirtualProtect(pageaddr, jl_page_size, PAGE_READONLY, &old_prot);
#else
    mprotect(pageaddr, jl_page_size, PROT_READ);
#endif
}

void jl_safepoint_init(void)
{
    // jl_page_size isn't available yet.
    size_t pgsz = jl_getpagesize();
#ifdef _OS_WINDOWS_
    char *addr = (char*)VirtualAlloc(NULL, pgsz * 3, MEM_COMMIT, PAGE_READONLY);
#else
    char *addr = (char*)mmap(0, pgsz * 3, PROT_READ,
                             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (addr == MAP_FAILED)
        addr = NULL;
#endif
    if (addr == NULL) {
        jl_printf(JL_STDERR, "could not allocate GC synchronization page\n");
        gc_debug_critical_error();
        abort();
    }
    // The signal page is for the gc safepoint.
    // The page before it is the sigint pending flag.
    jl_safepoint_pages = addr;
}

int jl_safepoint_start_gc(void)
{
    if (jl_n_threads == 1) {
        jl_gc_running = 1;
        return 1;
    }
    // The thread should have set this already
    assert(jl_get_ptls_states()->gc_state == JL_GC_STATE_WAITING);
    jl_mutex_lock_nogc(&safepoint_lock);
    // In case multiple threads enter the GC at the same time, only allow
    // one of them to actually run the collection. We can't just let the
    // master thread do the GC since it might be running unmanaged code
    // and can take arbitrarily long time before hitting a safe point.
    if (jl_atomic_compare_exchange(&jl_gc_running, 0, 1) != 0) {
        jl_mutex_unlock_nogc(&safepoint_lock);
        jl_safepoint_wait_gc();
        return 0;
    }
    jl_safepoint_enable(1);
    jl_safepoint_enable(2);
    jl_mutex_unlock_nogc(&safepoint_lock);
    return 1;
}

void jl_safepoint_end_gc(void)
{
    assert(jl_gc_running);
    if (jl_n_threads == 1) {
        jl_gc_running = 0;
        return;
    }
    jl_mutex_lock_nogc(&safepoint_lock);
    // Need to reset the page protection before resetting the flag since
    // the thread will trigger a segfault immediately after returning from
    // the signal handler.
    jl_safepoint_disable(2);
    jl_safepoint_disable(1);
    jl_atomic_store_release(&jl_gc_running, 0);
#  ifdef __APPLE__
    // This wakes up other threads on mac.
    jl_mach_gc_end();
#  endif
    jl_mutex_unlock_nogc(&safepoint_lock);
}

void jl_safepoint_wait_gc(void)
{
    // The thread should have set this is already
    assert(jl_get_ptls_states()->gc_state != 0);
    // Use normal volatile load in the loop for speed until GC finishes.
    // Then use an acquire load to make sure the GC result is visible on this thread.
    while (jl_atomic_load_relaxed(&jl_gc_running) || jl_atomic_load_acquire(&jl_gc_running)) {
        jl_cpu_pause(); // yield?
    }
}

void jl_safepoint_enable_sigint(void)
{
    jl_mutex_lock_nogc(&safepoint_lock);
    // Make sure both safepoints are enabled exactly once for SIGINT.
    switch (jl_signal_pending) {
    default:
        assert(0 && "Shouldn't happen.");
    case 0:
        // Enable SIGINT page
        jl_safepoint_enable(0);
        // fall through
    case 1:
        // SIGINT page is enabled, enable GC page
        jl_safepoint_enable(1);
        // fall through
    case 2:
        jl_signal_pending = 2;
    }
    jl_mutex_unlock_nogc(&safepoint_lock);
}

void jl_safepoint_defer_sigint(void)
{
    jl_mutex_lock_nogc(&safepoint_lock);
    // Make sure the GC safepoint is disabled for SIGINT.
    if (jl_signal_pending == 2) {
        jl_safepoint_disable(1);
        jl_signal_pending = 1;
    }
    jl_mutex_unlock_nogc(&safepoint_lock);
}

int jl_safepoint_consume_sigint(void)
{
    int has_signal = 0;
    jl_mutex_lock_nogc(&safepoint_lock);
    // Make sure both safepoints are disabled for SIGINT.
    switch (jl_signal_pending) {
    default:
        assert(0 && "Shouldn't happen.");
    case 2:
        // Disable gc page
        jl_safepoint_disable(1);
        // fall through
    case 1:
        // GC page is disabled, disable SIGINT page
        jl_safepoint_disable(0);
        has_signal = 1;
        // fall through
    case 0:
        jl_signal_pending = 0;
    }
    jl_mutex_unlock_nogc(&safepoint_lock);
    return has_signal;
}

#ifdef __cplusplus
}
#endif
