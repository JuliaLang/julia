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
// The fourth page is the count of suspended threads
uint16_t jl_safepoint_enable_cnt[4] = {0, 0, 0, 0};

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
uv_cond_t safepoint_cond_begin;
uv_cond_t safepoint_cond_end;

static void jl_safepoint_enable(int idx) JL_NOTSAFEPOINT
{
    // safepoint_lock should be held
    assert(0 <= idx && idx <= 3);
    if (jl_safepoint_enable_cnt[idx]++ != 0) {
        // We expect this to be enabled at most twice
        // one for the GC, one for SIGINT.
        // Update this if this is not the case anymore in the future.
        assert(jl_safepoint_enable_cnt[idx] <= (idx == 3 ? INT16_MAX : 2));
        return;
    }
    // Now that we are requested to mprotect the page and it wasn't already.
    char *pageaddr = jl_safepoint_pages + jl_page_size * idx;
#ifdef _OS_WINDOWS_
    DWORD old_prot;
    VirtualProtect(pageaddr, jl_page_size, PAGE_NOACCESS, &old_prot);
#else
    int r = mprotect(pageaddr, jl_page_size, PROT_NONE);
    (void)r; //if (r) perror("mprotect");
#endif
}

static void jl_safepoint_disable(int idx) JL_NOTSAFEPOINT
{
    // safepoint_lock should be held
    assert(0 <= idx && idx <= 3);
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
    int r = mprotect(pageaddr, jl_page_size, PROT_READ);
    (void)r; //if (r) perror("mprotect");
#endif
}

void jl_safepoint_init(void)
{
    uv_mutex_init(&safepoint_lock);
    uv_cond_init(&safepoint_cond_begin);
    uv_cond_init(&safepoint_cond_end);
    // jl_page_size isn't available yet.
    size_t pgsz = jl_getpagesize();
#ifdef _OS_WINDOWS_
    char *addr = (char*)VirtualAlloc(NULL, pgsz * 4, MEM_COMMIT, PAGE_READONLY);
#else
    char *addr = (char*)mmap(0, pgsz * 4, PROT_READ,
                             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (addr == MAP_FAILED)
        addr = NULL;
#endif
    if (addr == NULL) {
        jl_printf(JL_STDERR, "could not allocate GC synchronization page\n");
        jl_gc_debug_critical_error();
        abort();
    }
//    // If we able to skip past the faulting safepoint instruction conditionally,
//    // then we can make this safepoint page unconditional. But otherwise we
//    // only enable this page when required, though it gives us less
//    // fine-grained control over individual resume.
//    char *pageaddr = addr + pgsz * 3;
//#ifdef _OS_WINDOWS_
//    DWORD old_prot;
//    VirtualProtect(pageaddr, pgsz, PAGE_NOACCESS, &old_prot);
//#else
//    int r = mprotect(pageaddr, pgsz, PROT_NONE);
//    (void)r; //if (r) perror("mprotect");
//#endif
    // The signal page is for the gc safepoint.
    // The page before it is the sigint pending flag.
    jl_safepoint_pages = addr;
}

void jl_gc_wait_for_the_world(jl_ptls_t* gc_all_tls_states, int gc_n_threads)
{
    JL_TIMING(GC, GC_Stop);
#ifdef USE_TRACY
    TracyCZoneCtx ctx = JL_TIMING_DEFAULT_BLOCK->tracy_ctx;
    TracyCZoneColor(ctx, 0x696969);
#endif
    assert(gc_n_threads);
    if (gc_n_threads > 1)
        jl_wake_libuv();
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL) {
            // This acquire load pairs with the release stores
            // in the signal handler of safepoint so we are sure that
            // all the stores on those threads are visible.
            // We're currently also using atomic store release in mutator threads
            // (in jl_gc_state_set), but we may want to use signals to flush the
            // memory operations on those threads lazily instead.
            while (!jl_atomic_load_relaxed(&ptls2->gc_state) || !jl_atomic_load_acquire(&ptls2->gc_state)) {
                // Use system mutexes rather than spin locking to minimize wasted CPU time
                // while we wait for other threads reach a safepoint.
                // This is particularly important when run under rr.
                if (jl_options.timeout_for_safepoint_straggler_s == -1) { // timeout was not specified: no need to dump the backtrace
                    uv_mutex_lock(&safepoint_lock);
                    if (!jl_atomic_load_relaxed(&ptls2->gc_state)) {
                        uv_cond_wait(&safepoint_cond_begin, &safepoint_lock);
                    }
                    uv_mutex_unlock(&safepoint_lock);
                }
                else {
                    const int64_t timeout = jl_options.timeout_for_safepoint_straggler_s * 1000000000LL; // convert to nanoseconds
                    int ret = 0;
                    uv_mutex_lock(&safepoint_lock);
                    if (!jl_atomic_load_relaxed(&ptls2->gc_state)) {
                        ret = uv_cond_timedwait(&safepoint_cond_begin, &safepoint_lock, timeout);
                    }
                    uv_mutex_unlock(&safepoint_lock);
                    // If we woke up because of a timeout, print the backtrace of the straggler
                    if (ret == UV_ETIMEDOUT) {
                        jl_safe_printf("===== Thread %d failed to reach safepoint after %d seconds, printing backtrace below =====\n", ptls2->tid + 1, jl_options.timeout_for_safepoint_straggler_s);
                        // Try to record the backtrace of the straggler using `jl_try_record_thread_backtrace`
                        jl_ptls_t ptls = jl_current_task->ptls;
                        size_t bt_size = jl_try_record_thread_backtrace(ptls2, ptls->bt_data, JL_MAX_BT_SIZE);
                        // Print the backtrace of the straggler
                        for (size_t i = 0; i < bt_size; i += jl_bt_entry_size(ptls->bt_data + i)) {
                            jl_print_bt_entry_codeloc(ptls->bt_data + i);
                        }
                    }
                }
            }
        }
    }
}

int jl_safepoint_start_gc(jl_task_t *ct)
{
    // The thread should have just set this before entry
    assert(jl_atomic_load_relaxed(&ct->ptls->gc_state) == JL_GC_STATE_WAITING);
    uv_mutex_lock(&safepoint_lock);
    uv_cond_broadcast(&safepoint_cond_begin);
    // make sure we are permitted to run GC now (we might be required to stop instead)
    while (jl_atomic_load_relaxed(&ct->ptls->suspend_count)) {
        uv_mutex_unlock(&safepoint_lock);
        jl_safepoint_wait_thread_resume(ct);
        uv_mutex_lock(&safepoint_lock);
    }
    // In case multiple threads enter the GC at the same time, only allow
    // one of them to actually run the collection. We can't just let the
    // master thread do the GC since it might be running unmanaged code
    // and can take arbitrarily long time before hitting a safe point.
    uint32_t running = 0;
    if (!jl_atomic_cmpswap(&jl_gc_running, &running, 1)) {
        uv_mutex_unlock(&safepoint_lock);
        jl_safepoint_wait_gc(ct);
        return 0;
    }
    // Foreign thread adoption disables the GC and waits for it to finish, however, that may
    // introduce a race between it and this thread checking if the GC is enabled and only
    // then setting jl_gc_running. To avoid that, check again now that we won that race.
    if (jl_atomic_load_acquire(&jl_gc_disable_counter)) {
        jl_atomic_store_release(&jl_gc_running, 0);
        uv_mutex_unlock(&safepoint_lock);
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
    uv_mutex_lock(&safepoint_lock);
    // Need to reset the page protection before resetting the flag since
    // the thread will trigger a segfault immediately after returning from
    // the signal handler.
    jl_safepoint_disable(2);
    jl_safepoint_disable(1);
    jl_atomic_store_release(&jl_gc_running, 0);
#  ifdef _OS_DARWIN_
    // This wakes up other threads on mac.
    jl_mach_gc_end();
#  endif
    uv_mutex_unlock(&safepoint_lock);
    uv_cond_broadcast(&safepoint_cond_end);
}

void jl_set_gc_and_wait(jl_task_t *ct) // n.b. not used on _OS_DARWIN_
{
    // reading own gc state doesn't need atomic ops since no one else
    // should store to it.
    int8_t state = jl_atomic_load_relaxed(&ct->ptls->gc_state);
    jl_atomic_store_release(&ct->ptls->gc_state, JL_GC_STATE_WAITING);
    uv_mutex_lock(&safepoint_lock);
    uv_cond_broadcast(&safepoint_cond_begin);
    uv_mutex_unlock(&safepoint_lock);
    jl_safepoint_wait_gc(ct);
    jl_atomic_store_release(&ct->ptls->gc_state, state);
    jl_safepoint_wait_thread_resume(ct); // block in thread-suspend now if requested, after clearing the gc_state
}

// this is the core of jl_set_gc_and_wait
void jl_safepoint_wait_gc(jl_task_t *ct) JL_NOTSAFEPOINT
{
    if (ct) {
        JL_TIMING_SUSPEND_TASK(GC_SAFEPOINT, ct);
        // The thread should have set this is already
        assert(jl_atomic_load_relaxed(&ct->ptls->gc_state) != JL_GC_STATE_UNSAFE);
    }
    // Use normal volatile load in the loop for speed until GC finishes.
    // Then use an acquire load to make sure the GC result is visible on this thread.
    while (jl_atomic_load_relaxed(&jl_gc_running) || jl_atomic_load_acquire(&jl_gc_running)) {
        // Use system mutexes rather than spin locking to minimize wasted CPU
        // time on the idle cores while we wait for the GC to finish.
        // This is particularly important when run under rr.
        uv_mutex_lock(&safepoint_lock);
        if (jl_atomic_load_relaxed(&jl_gc_running))
            uv_cond_wait(&safepoint_cond_end, &safepoint_lock);
        uv_mutex_unlock(&safepoint_lock);
    }
}

// equivalent to jl_set_gc_and_wait, but waiting on resume-thread lock instead
void jl_safepoint_wait_thread_resume(jl_task_t *ct)
{
    // n.b. we do not permit a fast-path here that skips the lock acquire since
    // we otherwise have no synchronization point to ensure that this thread
    // will observe the change to the safepoint, even though the other thread
    // might have already observed our gc_state.
    // if (!jl_atomic_load_relaxed(&ct->ptls->suspend_count)) return;
    int8_t state = jl_atomic_load_relaxed(&ct->ptls->gc_state);
    jl_atomic_store_release(&ct->ptls->gc_state, JL_GC_STATE_WAITING);
    uv_mutex_lock(&ct->ptls->sleep_lock);
    if (jl_atomic_load_relaxed(&ct->ptls->suspend_count)) {
        // defer this broadcast until we determine whether uv_cond_wait is really going to be needed
        uv_mutex_unlock(&ct->ptls->sleep_lock);
        uv_mutex_lock(&safepoint_lock);
        uv_cond_broadcast(&safepoint_cond_begin);
        uv_mutex_unlock(&safepoint_lock);
        uv_mutex_lock(&ct->ptls->sleep_lock);
        while (jl_atomic_load_relaxed(&ct->ptls->suspend_count))
            uv_cond_wait(&ct->ptls->wake_signal, &ct->ptls->sleep_lock);
    }
    // must exit gc while still holding the mutex_unlock, so we know other
    // threads in jl_safepoint_suspend_thread will observe this thread in the
    // correct GC state, and not still stuck in JL_GC_STATE_WAITING
    jl_atomic_store_release(&ct->ptls->gc_state, state);
    uv_mutex_unlock(&ct->ptls->sleep_lock);
}
// This takes the sleep lock and puts the thread in GC_SAFE
int8_t jl_safepoint_take_sleep_lock(jl_ptls_t ptls)
{
    int8_t gc_state = jl_gc_safe_enter(ptls);
    uv_mutex_lock(&ptls->sleep_lock);
    if (jl_atomic_load_relaxed(&ptls->suspend_count)) {
        // This dance with the locks is because we are not allowed to hold both these locks at the same time
        // This avoids a situation where  jl_safepoint_suspend_thread loads our GC state and sees GC_UNSAFE
        // But we are in the process of becoming GC_SAFE, and also trigger the old safepoint, this causes us
        // to go sleep in scheduler and the suspender thread to go to sleep in safepoint_cond_begin meaning we hang
        // To avoid this we do the broadcast below to force it to observe the new gc_state
        uv_mutex_unlock(&ptls->sleep_lock);
        uv_mutex_lock(&safepoint_lock);
        uv_cond_broadcast(&safepoint_cond_begin);
        uv_mutex_unlock(&safepoint_lock);
        uv_mutex_lock(&ptls->sleep_lock);
    }
    return gc_state;
}

// n.b. suspended threads may still run in the GC or GC safe regions
// but shouldn't be observable, depending on which enum the user picks (only 1 and 2 are typically recommended here)
// waitstate = 0 : do not wait for suspend to finish
// waitstate = 1 : wait for gc_state != 0 (JL_GC_STATE_WAITING or JL_GC_STATE_SAFE)
// waitstate = 2 : wait for gc_state != 0 (JL_GC_STATE_WAITING or JL_GC_STATE_SAFE) and that GC is not running on that thread
// waitstate = 3 : wait for full suspend (gc_state == JL_GC_STATE_WAITING) -- this may never happen if thread is sleeping currently
// if another thread comes along and calls jl_safepoint_resume, we also return early
// return new suspend count on success, 0 on failure
int jl_safepoint_suspend_thread(int tid, int waitstate)
{
    if (0 > tid || tid >= jl_atomic_load_acquire(&jl_n_threads))
        return 0;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    jl_task_t *ct2 = ptls2 ? jl_atomic_load_relaxed(&ptls2->current_task) : NULL;
    if (ct2 == NULL) {
        // this thread is not alive yet or already dead
        return 0;
    }
    uv_mutex_lock(&safepoint_lock);
    uv_mutex_lock(&ptls2->sleep_lock);
    int16_t suspend_count = jl_atomic_load_relaxed(&ptls2->suspend_count) + 1;
    jl_atomic_store_relaxed(&ptls2->suspend_count, suspend_count);
    if (suspend_count == 1) { // first to suspend
        jl_safepoint_enable(3);
        jl_atomic_store_relaxed(&ptls2->safepoint, (size_t*)(jl_safepoint_pages + jl_page_size * 3 + sizeof(void*)));
        if (jl_atomic_load(&_threadedregion) != 0 || tid == jl_atomic_load_relaxed(&io_loop_tid))
            jl_wake_libuv(); // our integration with libuv right now doesn't handle except by waking it
    }
    uv_mutex_unlock(&ptls2->sleep_lock);
    if (waitstate) {
        // wait for suspend (or another thread to call resume)
        if (waitstate >= 2) {
            // We currently cannot distinguish if a thread is helping run GC or
            // not, so assume it is running GC and wait for GC to finish first.
            // It will be unable to reenter helping with GC because we have
            // changed its safepoint page.
            uv_mutex_unlock(&safepoint_lock);
            jl_set_gc_and_wait(jl_current_task);
            uv_mutex_lock(&safepoint_lock);
        }
        while (jl_atomic_load_acquire(&ptls2->suspend_count) != 0) {
            int8_t state2 = jl_atomic_load_acquire(&ptls2->gc_state);
            if (waitstate <= 2 && state2 != JL_GC_STATE_UNSAFE)
                break;
            if (waitstate == 3 && state2 == JL_GC_STATE_WAITING)
                break;
            uv_cond_wait(&safepoint_cond_begin, &safepoint_lock);
        }
    }
    uv_mutex_unlock(&safepoint_lock);
    return suspend_count;
}

// return old suspend count on success, 0 on failure
// n.b. threads often do not resume until after all suspended threads have been resumed!
int jl_safepoint_resume_thread(int tid) JL_NOTSAFEPOINT
{
    if (0 > tid || tid >= jl_atomic_load_acquire(&jl_n_threads))
        return 0;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    jl_task_t *ct2 = ptls2 ? jl_atomic_load_relaxed(&ptls2->current_task) : NULL;
    if (ct2 == NULL) {
        // this thread is not alive yet or already dead
        return 0;
    }
    uv_mutex_lock(&safepoint_lock);
    uv_mutex_lock(&ptls2->sleep_lock);
    int16_t suspend_count = jl_atomic_load_relaxed(&ptls2->suspend_count);
    if (suspend_count == 1) { // last to unsuspend
        if (tid == 0)
            jl_atomic_store_relaxed(&ptls2->safepoint, (size_t*)(jl_safepoint_pages + jl_page_size));
        else
            jl_atomic_store_relaxed(&ptls2->safepoint, (size_t*)(jl_safepoint_pages + jl_page_size * 2 + sizeof(void*)));
        uv_cond_signal(&ptls2->wake_signal);
#ifdef _OS_DARWIN_
        jl_safepoint_resume_thread_mach(ptls2, tid);
#endif
        uv_cond_broadcast(&safepoint_cond_begin);
    }
    if (suspend_count != 0) {
        jl_atomic_store_relaxed(&ptls2->suspend_count, suspend_count - 1);
        if (suspend_count == 1)
            jl_safepoint_disable(3);
    }
    uv_mutex_unlock(&ptls2->sleep_lock);
    uv_mutex_unlock(&safepoint_lock);
    return suspend_count;
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
