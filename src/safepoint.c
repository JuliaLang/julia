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

_Atomic(void *) jl_gc_recruiting_location = NULL;
_Atomic(int32_t) jl_gc_safepoint_master = -1;
_Atomic(int32_t) nworkers_marking = 0;

extern uv_mutex_t *safepoint_sleep_locks;
extern uv_cond_t *safepoint_wake_signals;

const uint64_t timeout_ns = 500;

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
        jl_safepoint_wait_gc();
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
}

// Thread recruitment scheme inspired by Hassanein, 
// `Understanding and Improving JVM GC Work Stealing at the
// Data Center Scale`

void jl_safepoint_try_recruit(jl_ptls_t ptls)
{
    if (jl_atomic_load_relaxed(&jl_gc_recruiting_location)) {
        jl_gc_mark_loop_enter(ptls);
        void *location = jl_atomic_load_acquire(&jl_gc_recruiting_location);
        if (location)
            ((void (*)(jl_ptls_t))location)(ptls);
        jl_gc_mark_loop_leave(ptls);
    }
}

size_t jl_safepoint_master_count_work(jl_ptls_t ptls)
{
    size_t work = 0;
    for (int i = 0; i < jl_n_threads; i++) {
        if (i == ptls->tid)
            continue;
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        if (jl_atomic_load_relaxed(&ptls2->gc_state) == JL_GC_STATE_PARALLEL) {
            jl_gc_mark_cache_t *gc_cache2 = &ptls2->gc_cache;
            jl_gc_ws_queue_t *mark_queue2 = &gc_cache2->mark_queue;
            // This count can be slightly off, but it doesn't matter 
            // for recruitment heuristics
            jl_gc_ws_bottom_t bottom2 = jl_atomic_load_relaxed(&mark_queue2->bottom);
            jl_gc_ws_top_t top2 = jl_atomic_load_relaxed(&mark_queue2->top);
            work += bottom2.pc_offset - top2.offset;
        }
    }
    return work;
}

void jl_safepoint_master_notify_all(jl_ptls_t ptls)
{
    for (int i = 0; i < jl_n_threads; i++) {
        if (i == ptls->tid)
            continue;
        uv_mutex_lock(&safepoint_sleep_locks[i]);
        uv_cond_signal(&safepoint_wake_signals[i]);
        uv_mutex_unlock(&safepoint_sleep_locks[i]);
    }
}

void jl_safepoint_master_recruit_workers(jl_ptls_t ptls, size_t nworkers)
{
    for (int i = 0; i < jl_n_threads && nworkers > 0; i++) {
        if (i == ptls->tid)
            continue;
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        if (jl_atomic_load_acquire(&ptls2->gc_state) == JL_GC_STATE_WAITING) {
            uv_mutex_lock(&safepoint_sleep_locks[i]);
            uv_cond_signal(&safepoint_wake_signals[i]);
            uv_mutex_unlock(&safepoint_sleep_locks[i]);
            nworkers--;
        }
    }
}

int jl_safepoint_master_end_marking(jl_ptls_t ptls)
{
    // All workers done with marking
    if (jl_atomic_load_acquire(&nworkers_marking) == 0)
        return 1;
    int no_master = -1;
    if (jl_atomic_cmpswap(&jl_gc_safepoint_master, &no_master, ptls->tid)) {
        spin: {
            if (jl_atomic_load_acquire(&nworkers_marking) > 0) {
                size_t work = jl_safepoint_master_count_work(ptls);
                // If there is enough work, recruit workers and also become a worker,
                // relinquishing the safepoint master status
                if (work > 2) {
                    jl_safepoint_master_recruit_workers(ptls, work - 1);
                    jl_atomic_store_release(&jl_gc_safepoint_master, -1);
                    jl_safepoint_try_recruit(ptls);
                    return 0;
                }
                goto spin;
            }
        }
        jl_atomic_store_release(&jl_gc_safepoint_master, -1);
        jl_safepoint_master_notify_all(ptls);
        return 1;
    }
    return 0;
}

void jl_safepoint_wait_gc(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    while (jl_atomic_load_relaxed(&jl_gc_running) || jl_atomic_load_acquire(&jl_gc_running)) {
        if (jl_safepoint_master_end_marking(ptls)) {
            // Clean-up buffers from `reclaim_set`
            jl_gc_mark_cache_t *gc_cache = &ptls->gc_cache;
            jl_gc_ws_queue_t *mark_queue = &gc_cache->mark_queue;
            arraylist_t *rs = mark_queue->reclaim_set;
            jl_gc_ws_array_t *a;
            while ((a = (jl_gc_ws_array_t*)arraylist_pop(rs))) {
                free(a->pc_start);
                free(a->data_start);
                free(a);
            }
            break;
        }
        uv_mutex_lock(&safepoint_sleep_locks[ptls->tid]);
        if (!uv_cond_timedwait(&safepoint_wake_signals[ptls->tid], 
                               &safepoint_sleep_locks[ptls->tid], timeout_ns)) {
            // Stopped waiting because we got a notification 
            // from safepoint master: try to get recruited
            jl_safepoint_try_recruit(ptls);
        }
        uv_mutex_unlock(&safepoint_sleep_locks[ptls->tid]);
        // Otherwise, just go to the top of the loop and try
        // to become a safepoint master
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
