// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"
#include "options.h"
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
_Atomic(uint32_t) jl_gc_running = 0;
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
uv_mutex_t safepoint_lock;
uv_cond_t safepoint_cond;

jl_mutex_t safepoint_master_lock;
const uint64_t timeout_ns = 1e5;

extern _Atomic(int32_t) nworkers_marking;
extern void gc_mark_loop(jl_ptls_t ptls) JL_NOTSAFEPOINT;

static void jl_safepoint_enable(int idx) JL_NOTSAFEPOINT
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

static void jl_safepoint_disable(int idx) JL_NOTSAFEPOINT
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
    uv_mutex_init(&safepoint_lock);
	uv_cond_init(&safepoint_cond);
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
        jl_gc_debug_critical_error();
        abort();
    }
    // The signal page is for the gc safepoint.
    // The page before it is the sigint pending flag.
    jl_safepoint_pages = addr;
}

int jl_safepoint_start_gc(void)
{
    if (jl_n_threads == 1) {
        jl_atomic_store_relaxed(&jl_gc_running, 1);
        return 1;
    }
    // The thread should have set this already
    assert(jl_atomic_load_relaxed(&jl_current_task->ptls->gc_state) == JL_GC_STATE_WAITING);
    uv_mutex_lock(&safepoint_lock);
    // In case multiple threads enter the GC at the same time, only allow
    // one of them to actually run the collection. We can't just let the
    // master thread do the GC since it might be running unmanaged code
    // and can take arbitrarily long time before hitting a safe point.
    uint32_t running = 0;
    if (!jl_atomic_cmpswap(&jl_gc_running, &running, 1)) {
        uv_mutex_unlock(&safepoint_lock);
        jl_spinmaster_wait_gc();
        return 0;
    }
    jl_safepoint_enable(1);
    jl_safepoint_enable(2);
    uv_mutex_unlock(&safepoint_lock);
    return 1;
}

void jl_safepoint_end_gc(void)
{
    assert(jl_atomic_load_relaxed(&jl_gc_running));
    if (jl_n_threads == 1) {
        jl_atomic_store_relaxed(&jl_gc_running, 0);
        return;
    }
    uv_mutex_lock(&safepoint_lock);
    // Need to reset the page protection before resetting the flag since
    // the thread will trigger a segfault immediately after returning from
    // the signal handler.
    jl_safepoint_disable(2);
    jl_safepoint_disable(1);
    jl_atomic_store_release(&jl_gc_running, 0);
    uv_mutex_unlock(&safepoint_lock);
	uv_cond_broadcast(&safepoint_cond);
}

// Thread recruitment scheme inspired by Hassanein's "spin-master",
// `Understanding and Improving JVM GC Work Stealing at the
// Data Center Scale`

int jl_spinmaster_all_workers_done(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    return (jl_atomic_load_acquire(&nworkers_marking) == 0);
}

#ifndef GC_VERIFY
int64_t jl_spinmaster_count_work(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    int64_t work = 0;
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        jl_gc_markqueue_t *mq2 = &ptls2->mark_queue;
        ws_queue_t *q2 = &mq2->q;
        // This count can be slightly off, but it doesn't matter
        // for recruitment heuristics
        int64_t t2 = jl_atomic_load_relaxed(&q2->top);
        int64_t b2 = jl_atomic_load_relaxed(&q2->bottom);
        work += b2 - t2;
    }
    return work;
}

void jl_spinmaster_notify_all(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    for (int i = 0; i < jl_n_threads; i++) {
        if (i == ptls->tid)
            continue;
        uv_cond_signal(&jl_all_tls_states[i]->gc_wake_signal);
    }
}

void jl_spinmaster_recruit_workers(jl_ptls_t ptls, size_t nworkers) JL_NOTSAFEPOINT
{
    for (int i = 0; i < jl_n_threads && nworkers > 0; i++) {
        if (i == ptls->tid)
            continue;
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        if (jl_atomic_load_acquire(&ptls2->gc_state) == JL_GC_STATE_WAITING) {
            uv_cond_signal(&ptls->gc_wake_signal);
            nworkers--;
        }
    }
}
#endif

int jl_spinmaster_end_marking(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    // Fast path for mark-loop termination
    if (jl_spinmaster_all_workers_done(ptls)) {
        return 1;
    }
#ifndef GC_VERIFY
    if (jl_mutex_trylock_nogc(&safepoint_master_lock)) {
        spin : {
            // Check if all threads have finished marking
            if (!jl_spinmaster_all_workers_done(ptls)) {
                int64_t work = jl_spinmaster_count_work(ptls);
                // If there is enough work, recruit workers and also become a worker,
                // relinquishing the spin-master status
                if (work > 1) {
                    jl_spinmaster_recruit_workers(ptls, work - 1);
                    jl_mutex_unlock_nogc(&safepoint_master_lock);
                    gc_mark_loop(ptls);
                    return 0;
                }
                jl_cpu_pause();
                goto spin;
            }
        }
        jl_spinmaster_notify_all(ptls);
        jl_mutex_unlock_nogc(&safepoint_master_lock);
        return 1;
    }
#endif
    return 0;
}

void jl_spinmaster_wait_pmark(void) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    // There are still workers in the mark-loop: go through
    // spin-master protocol
    while(!jl_spinmaster_end_marking(ptls)) {
        uv_mutex_lock(&ptls->gc_sleep_lock);
        if (!uv_cond_timedwait(&ptls->gc_wake_signal,
                               &ptls->gc_sleep_lock, timeout_ns)) {
            // Stopped waiting because we got a notification
            // from spin-master: try to get recruited
            gc_mark_loop(ptls);
        }
        uv_mutex_unlock(&ptls->gc_sleep_lock);
        // Otherwise, just go to the top of the loop and try
        // to become a spin-master
    }
}

void jl_spinmaster_wait_sweeping(void) JL_NOTSAFEPOINT
{
	// Use normal volatile load in the loop for speed until GC finishes.
	// Then use an acquire load to make sure the GC result is visible on this thread.
    while (jl_atomic_load_relaxed(&jl_gc_running) || jl_atomic_load_acquire(&jl_gc_running)) {
        // Use system mutexes rather than spin locking to minimize wasted CPU
        // time on the idle cores while we wait for the GC to finish.
        // This is particularly important when run under rr.
        uv_mutex_lock(&safepoint_lock);
        if (jl_atomic_load_relaxed(&jl_gc_running))
            uv_cond_wait(&safepoint_cond, &safepoint_lock);
        uv_mutex_unlock(&safepoint_lock);
    }
}

void jl_spinmaster_wait_gc(void) JL_NOTSAFEPOINT
{
    while (jl_atomic_load_relaxed(&jl_gc_running) ||
           jl_atomic_load_acquire(&jl_gc_running)) {
        jl_spinmaster_wait_pmark();
        jl_spinmaster_wait_sweeping();
    }
}

void jl_safepoint_enable_sigint(void)
{
    uv_mutex_lock(&safepoint_lock);
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
    uv_mutex_unlock(&safepoint_lock);
}

void jl_safepoint_defer_sigint(void)
{
    uv_mutex_lock(&safepoint_lock);
    // Make sure the GC safepoint is disabled for SIGINT.
    if (jl_signal_pending == 2) {
        jl_safepoint_disable(1);
        jl_signal_pending = 1;
    }
    uv_mutex_unlock(&safepoint_lock);
}

int jl_safepoint_consume_sigint(void)
{
    int has_signal = 0;
    uv_mutex_lock(&safepoint_lock);
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
    uv_mutex_unlock(&safepoint_lock);
    return has_signal;
}

#ifdef __cplusplus
}
#endif
