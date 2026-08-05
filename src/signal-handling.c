// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include <unistd.h>
#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#endif
#ifdef _OS_LINUX_
#include <sys/prctl.h>
#include <sys/syscall.h>
#endif
#ifndef _OS_WINDOWS_
#include <signal.h>
#include <time.h>
#include <string.h>
#endif

// Platform selection for the SIGINT rescue timer mechanism (keep in sync
// with the HAVE_MACH/HAVE_KEVENT/HAVE_TIMER selection in signals-unix.c).
#if defined(__APPLE__) || defined(__OpenBSD__)
#define JL_HAVE_KEVENT_TIMER
#include <sys/event.h>
#elif defined(_OS_WINDOWS_)
#define JL_HAVE_WIN32_TIMER
#else
#define JL_HAVE_POSIX_TIMER
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <threading.h>

// Native mutex (not jl_mutex_t): the paths below run on non-Julia threads
// (the signal listener, Win32 console-ctrl handler threads, timer callback
// threads), which have no Julia task to derive lock ownership from.
static uv_mutex_t sigint_state_lock;

// Profiler control variables
uv_mutex_t live_tasks_lock;
uv_mutex_t bt_data_prof_lock;
volatile jl_bt_element_t *profile_bt_data_prof = NULL;
volatile size_t profile_bt_size_max = 0;
volatile size_t profile_bt_size_cur = 0;
static volatile uint64_t nsecprof = 0;
volatile int profile_running = 0;
volatile int profile_all_tasks = 0;
static const uint64_t GIGA = 1000000000ULL;
// Timers to take samples at intervals
JL_DLLEXPORT void jl_profile_stop_timer(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_profile_start_timer(uint8_t) JL_NOTSAFEPOINT;

///////////////////////
// Utility functions //
///////////////////////
JL_DLLEXPORT int jl_profile_init(size_t maxsize, uint64_t delay_nsec)
{
    uv_mutex_lock(&bt_data_prof_lock);
    if (profile_running) {
        // the sampler may be writing into the buffer we are about to free. Note this
        // only rules out re-initializing while running: `jl_profile_stop_timer` doesn't
        // wait for a sampler iteration already under way, and the thread samplers write
        // without taking this lock.
        uv_mutex_unlock(&bt_data_prof_lock);
        return -2;
    }
    profile_bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (profile_bt_data_prof != NULL)
        free((void*)profile_bt_data_prof);
    profile_bt_data_prof = (jl_bt_element_t*) calloc(maxsize, sizeof(jl_bt_element_t));
    if (profile_bt_data_prof == NULL && maxsize > 0) {
        uv_mutex_unlock(&bt_data_prof_lock);
        return -1;
    }
    profile_bt_size_cur = 0;
    uv_mutex_unlock(&bt_data_prof_lock);
    return 0;
}

JL_DLLEXPORT uint8_t *jl_profile_get_data(void)
{
    return (uint8_t*) profile_bt_data_prof;
}

JL_DLLEXPORT size_t jl_profile_len_data(void)
{
    return profile_bt_size_cur;
}

JL_DLLEXPORT size_t jl_profile_maxlen_data(void)
{
    return profile_bt_size_max;
}

JL_DLLEXPORT uint64_t jl_profile_delay_nsec(void)
{
    return nsecprof;
}

JL_DLLEXPORT void jl_profile_clear_data(void)
{
    profile_bt_size_cur = 0;
}

JL_DLLEXPORT int jl_profile_is_running(void)
{
    return profile_running;
}

// Any function that acquires this lock must be either an unmanaged thread
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
    uv_mutex_init(&sigint_state_lock);
    uv_rwlock_init(&debuginfo_asyncsafe);
#ifndef _OS_WINDOWS_
    pthread_key_create(&debuginfo_asyncsafe_held, NULL);
#else
    debuginfo_asyncsafe_held = TlsAlloc();
#endif
}

static uintptr_t jl_lock_profile_rd_held(void) JL_NOTSAFEPOINT
{
#ifndef _OS_WINDOWS_
    return (uintptr_t)pthread_getspecific(debuginfo_asyncsafe_held);
#else
    return (uintptr_t)TlsGetValue(debuginfo_asyncsafe_held);
#endif
}

void jl_lock_profile(void)
{
    int got = jl_trylock_profile();
    assert(got); (void)got;
}

int jl_trylock_profile(void)
{
    uintptr_t held = jl_lock_profile_rd_held();
    if (held == -1)
        return 0;
    if (held == 0) {
        held = -1;
#ifndef _OS_WINDOWS_
        pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
        TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
        uv_rwlock_rdlock(&debuginfo_asyncsafe);
        held = 0;
    }
    held++;
#ifndef _OS_WINDOWS_
    pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
    TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
    return 1;
}

JL_DLLEXPORT void jl_unlock_profile(void) JL_NO_SAFEPOINT_ANALYSIS
{
    uintptr_t held = jl_lock_profile_rd_held();
    assert(held && held != -1);
    held--;
#ifndef _OS_WINDOWS_
    pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
    TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
    if (held == 0)
        uv_rwlock_rdunlock(&debuginfo_asyncsafe);
}

int jl_lock_profile_wr(void)
{
    uintptr_t held = jl_lock_profile_rd_held();
    if (held)
        return 0;
    held = -1;
#ifndef _OS_WINDOWS_
    pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
    TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
    uv_rwlock_wrlock(&debuginfo_asyncsafe);
    return 1;
}

void jl_unlock_profile_wr(void) JL_NO_SAFEPOINT_ANALYSIS
{
    uintptr_t held = jl_lock_profile_rd_held();
    assert(held == -1);
    held = 0;
#ifndef _OS_WINDOWS_
    pthread_setspecific(debuginfo_asyncsafe_held, (void*)held);
#else
    TlsSetValue(debuginfo_asyncsafe_held, (void*)held);
#endif
    uv_rwlock_wrunlock(&debuginfo_asyncsafe);
}


static uint64_t profile_cong_rng_seed = 0;
static int *profile_round_robin_thread_order = NULL;
static int profile_round_robin_thread_order_size = 0;

static void jl_shuffle_int_array_inplace(int *carray, int size, uint64_t *seed)
{
    // The "modern Fisher–Yates shuffle" - O(n) algorithm
    // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
    for (int i = size; i-- > 1; ) {
        size_t j = cong(i + 1, seed); // cong is an open interval so we add 1
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


JL_DLLEXPORT int jl_profile_is_buffer_full(void) JL_NOTSAFEPOINT
{
    // Declare buffer full if there isn't enough room to sample even just the
    // thread metadata and one max-sized frame. The `+ 6` is for the two block
    // terminator `0`'s plus the 4 metadata entries.
    return profile_bt_size_cur + ((JL_BT_MAX_ENTRY_SIZE + 1) + 6) > profile_bt_size_max;
}

#define PROFILE_TASK_DEBUG_FORCE_SAMPLING_FAILURE (0)
#define PROFILE_TASK_DEBUG_FORCE_STOP_THREAD_FAILURE (0)

void jl_profile_task(void) JL_NOTSAFEPOINT JL_NO_SAFEPOINT_ANALYSIS
{
    if (jl_profile_is_buffer_full()) {
        // Buffer full: Delete the timer
        jl_profile_stop_timer();
        return;
    }

    jl_task_t *t = NULL;
    int got_mutex = 0;
    if (uv_mutex_trylock(&live_tasks_lock) != 0) {
        goto collect_backtrace;
    }
    got_mutex = 1;

    {
        arraylist_t *tasks = jl_get_all_tasks_arraylist();
        uint64_t seed = jl_rand();
        const int n_max_random_attempts = 4;
        // randomly select a task that is not done
        for (int i = 0; i < n_max_random_attempts; i++) {
            t = (jl_task_t*)tasks->items[cong(tasks->len, &seed)];
            assert(t == NULL || jl_is_task(t));
            if (t == NULL) {
                continue;
            }
            int t_state = jl_atomic_load_relaxed(&t->_state);
            if (t_state == JL_TASK_STATE_DONE) {
                continue;
            }
            break;
        }
        arraylist_free(tasks);
        free(tasks);
    }

collect_backtrace:

    uv_mutex_lock(&bt_data_prof_lock);
    if (profile_running == 0) {
        uv_mutex_unlock(&bt_data_prof_lock);
        if (got_mutex) {
            uv_mutex_unlock(&live_tasks_lock);
        }
        return;
    }

    jl_record_backtrace_result_t r = {0, -1};
    jl_bt_element_t *bt_data_prof = (jl_bt_element_t*)(profile_bt_data_prof + profile_bt_size_cur);
    size_t bt_size_max = profile_bt_size_max - profile_bt_size_cur - 1;
    if (t == NULL || PROFILE_TASK_DEBUG_FORCE_SAMPLING_FAILURE) {
        // failed to find a task
        r.bt_size = failed_to_sample_task_fun(bt_data_prof, bt_size_max, 0);
    }
    else {
        if (!PROFILE_TASK_DEBUG_FORCE_STOP_THREAD_FAILURE) {
            r = jl_record_backtrace(t, bt_data_prof, bt_size_max, 1);
        }
        // we failed to get a backtrace
        if (r.bt_size == 0) {
            r.bt_size = failed_to_stop_thread_fun(bt_data_prof, bt_size_max, 0);
        }
    }

    // update the profile buffer size
    profile_bt_size_cur += r.bt_size;

    // store threadid but add 1 as 0 is preserved to indicate end of block
    profile_bt_data_prof[profile_bt_size_cur++].uintptr = r.tid == -1 ? -1 : (uintptr_t)r.tid + 1;

    // store task id (never null)
    profile_bt_data_prof[profile_bt_size_cur++].jlvalue = (jl_value_t*)t;

    // store cpu cycle clock
    profile_bt_data_prof[profile_bt_size_cur++].uintptr = cycleclock();

    // the thread profiler uses this block to record whether the thread is not sleeping (1) or sleeping (2)
    // let's use a dummy value which is not 1 or 2 to
    // indicate that we are profiling a task, and therefore, this block is not about the thread state
    profile_bt_data_prof[profile_bt_size_cur++].uintptr = 3;

    // Mark the end of this block with two 0's
    profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;
    profile_bt_data_prof[profile_bt_size_cur++].uintptr = 0;

    uv_mutex_unlock(&bt_data_prof_lock);
    if (got_mutex) {
        uv_mutex_unlock(&live_tasks_lock);
    }
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
    // unblocked and use its behavior to decide if we need to handle the signal.
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
    return 0;
}

static int exit_on_sigint = 0;
JL_DLLEXPORT void jl_exit_on_sigint(int on)
{
    exit_on_sigint = on;
}

static uintptr_t jl_get_pc_from_ctx(const void *_ctx);
static void jl_fprint_sigill(ios_t *s, void *_ctx);
#if defined(_CPU_X86_64_) || defined(_CPU_X86_) \
    || (defined(_OS_LINUX_) && defined(_CPU_AARCH64_)) \
    || (defined(_OS_LINUX_) && defined(_CPU_ARM_)) \
    || (defined(_OS_LINUX_) && defined(_CPU_RISCV64_))
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

jl_mutex_t profile_show_peek_cond_lock;
static uv_async_t *profile_show_peek_cond_loc;
JL_DLLEXPORT void jl_set_peek_cond(uv_async_t *cond)
{
    JL_LOCK_NOGC(&profile_show_peek_cond_lock);
    profile_show_peek_cond_loc = cond;
    JL_UNLOCK_NOGC(&profile_show_peek_cond_lock);
}

static void jl_check_profile_autostop(void) JL_NOTSAFEPOINT
{
    if (profile_show_peek_cond_loc != NULL && profile_autostop_time != -1.0 && jl_hrtime() > profile_autostop_time) {
        profile_autostop_time = -1.0;
        jl_profile_stop_timer();
        // Disable trace compilation when profile collection ends
        jl_force_trace_compile_timing_disable();
        jl_safe_printf("\n==============================================================\n");
        jl_safe_printf("Profile collected. A report will print at the next yield point.\n");
        jl_safe_printf("Disabling --trace-compile\n");
        jl_safe_printf("==============================================================\n\n");
        JL_LOCK_NOGC(&profile_show_peek_cond_lock);
        if (profile_show_peek_cond_loc != NULL)
            uv_async_send(profile_show_peek_cond_loc);
        JL_UNLOCK_NOGC(&profile_show_peek_cond_lock);
    }
}

// State for delegating SIGINT handling to a dedicated listener task (similar
// to the profile listener above): the signal listener marks the ^C episode
// source cancelled and pings this async condition; the Base-side listener
// task performs the remaining delivery work.
static _Atomic(uv_async_t *) sigint_cond_loc = NULL;
JL_DLLEXPORT void jl_set_sigint_cond(uv_async_t *cond) JL_NOTSAFEPOINT
{
    // The lock pairs with deliver_sigint_notification: a notification in
    // flight completes its async send before exit can clear + close the
    // handle. (Lock-free readers only probe for NULL.)
    uv_mutex_lock(&sigint_state_lock);
    jl_atomic_store_relaxed(&sigint_cond_loc, cond);
    uv_mutex_unlock(&sigint_state_lock);
}

// The rescue timer implements the escalation state machine for ^C: if the
// process does not acknowledge a cancellation request within the timeout,
// the user gets a warning, and the next user-sent ^C abandons the stuck task
// (switching the thread to the rescue task).
// N.B.: The rescue task is rooted on the Julia side (Base keeps a global
// reference).
static jl_task_t *sigint_rescue_task = NULL;
// Rescue-timer expiry, tagged with an episode generation. The flag is
// written by timer threads (a POSIX timer signal, a Win32 timer-queue
// callback) and read/consumed by the signal listener and the Julia-side
// listener; `volatile` is not inter-thread synchronization, and a queued
// timer firing that lands after the episode was reset must not leak its
// expiry into the fresh episode. The generation is bumped on escalation
// delivery and on episode reset (NOT on a mere re-arm - a standing offer
// must survive the repeat press that accepts it); an expiry is only
// visible while its recorded generation is current. Starts at 1 so a
// recorded expiry can never collide with expired_gen's 0 sentinel.
static _Atomic(uint32_t) sigint_rescue_gen = 1;
static _Atomic(uint32_t) sigint_rescue_armed_gen = 0;   // generation of the last arm
static _Atomic(uint32_t) sigint_rescue_expired_gen = 0; // 0 = no expiry

// Classifies the ^C episode state recorded on the root task's cancellation
// request (see base/Base.jl's sigint listener), for escalation decisions and
// the rescue-timer warning text. Defined below, after the shared state.
static int jl_sigint_episode_state(void) JL_NOTSAFEPOINT;

#if defined(JL_HAVE_POSIX_TIMER)
// The timer raises SIGINT, distinguished from a user ^C by
// SI_TIMER/sival_int == 1, which the signal listener thread handles.
static timer_t sigint_rescue_timer;
static int sigint_rescue_timer_created = 0;

JL_DLLEXPORT void jl_set_sigint_rescue_task(jl_task_t *t) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&sigint_state_lock);
    if (!sigint_rescue_timer_created) {
        struct sigevent ev;
        memset(&ev, 0, sizeof(ev));
        ev.sigev_notify = SIGEV_SIGNAL;
        ev.sigev_signo = SIGINT;
        ev.sigev_value.sival_int = 1;
        if (timer_create(CLOCK_MONOTONIC, &ev, &sigint_rescue_timer) == 0) {
            sigint_rescue_timer_created = 1;
        }
    }
    sigint_rescue_task = t;
    uv_mutex_unlock(&sigint_state_lock);
}

static void jl_arm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    // Record the current generation for this arming. Re-arming (a repeat
    // press within the same rung) must NOT invalidate a standing expiry -
    // a press after the offer printed is exactly what escalates. Only a
    // rung change (jl_sigint_escalation_delivered) or episode close
    // (jl_reset_sigint_rescue_timer) opens a fresh generation.
    jl_atomic_store_relaxed(&sigint_rescue_armed_gen,
                            jl_atomic_load_relaxed(&sigint_rescue_gen));
    if (!sigint_rescue_timer_created)
        return;
    struct itimerspec its;
    its.it_interval.tv_sec = 0;
    its.it_interval.tv_nsec = 0;
    its.it_value.tv_sec = 1; // 1s
    its.it_value.tv_nsec = 0;
    timer_settime(sigint_rescue_timer, 0, &its, NULL);
}

JL_DLLEXPORT void jl_disarm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    if (!sigint_rescue_timer_created)
        return;
    struct itimerspec its;
    its.it_interval.tv_sec = 0;
    its.it_interval.tv_nsec = 0;
    its.it_value.tv_sec = 0;
    its.it_value.tv_nsec = 0;
    timer_settime(sigint_rescue_timer, 0, &its, NULL);
}

#elif defined(JL_HAVE_KEVENT_TIMER)
// The timer is an EVFILT_TIMER event on the signal listener's kqueue
// (created in signals-unix.c, which stores the fd here).
static int sigint_rescue_kq = -1;
#define JL_SIGINT_RESCUE_TIMER_IDENT ((uintptr_t)0x51C4)

JL_DLLEXPORT void jl_set_sigint_rescue_task(jl_task_t *t) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&sigint_state_lock);
    sigint_rescue_task = t;
    uv_mutex_unlock(&sigint_state_lock);
}

static void jl_arm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    // Record the current generation for this arming. Re-arming (a repeat
    // press within the same rung) must NOT invalidate a standing expiry -
    // a press after the offer printed is exactly what escalates. Only a
    // rung change (jl_sigint_escalation_delivered) or episode close
    // (jl_reset_sigint_rescue_timer) opens a fresh generation.
    jl_atomic_store_relaxed(&sigint_rescue_armed_gen,
                            jl_atomic_load_relaxed(&sigint_rescue_gen));
    if (sigint_rescue_kq == -1)
        return;
    struct kevent ev;
    EV_SET(&ev, JL_SIGINT_RESCUE_TIMER_IDENT, EVFILT_TIMER,
           EV_ADD | EV_ONESHOT, 0, 1000 /* ms */, 0);
    kevent(sigint_rescue_kq, &ev, 1, NULL, 0, NULL);
}

JL_DLLEXPORT void jl_disarm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    if (sigint_rescue_kq == -1)
        return;
    struct kevent ev;
    EV_SET(&ev, JL_SIGINT_RESCUE_TIMER_IDENT, EVFILT_TIMER, EV_DELETE, 0, 0, 0);
    kevent(sigint_rescue_kq, &ev, 1, NULL, 0, NULL); // ignore ENOENT
}

#elif defined(JL_HAVE_WIN32_TIMER)
// The timer is a timer-queue timer; its callback (on a system pool thread)
// warns the user and marks the escalation state.
static HANDLE sigint_rescue_timer_handle = NULL;

JL_DLLEXPORT void jl_set_sigint_rescue_task(jl_task_t *t) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&sigint_state_lock);
    sigint_rescue_task = t;
    uv_mutex_unlock(&sigint_state_lock);
}

static VOID CALLBACK sigint_rescue_timer_cb(PVOID param, BOOLEAN fired)
{
    (void)param; (void)fired;
    int est = jl_sigint_episode_state();
    if (est == 0)
        return; // the episode already completed - stand down
    jl_atomic_store_release(&sigint_rescue_expired_gen,
                            jl_atomic_load_relaxed(&sigint_rescue_armed_gen));
    if (est == 2) {
        jl_safe_printf("\nWARNING: Cancellation is in progress, but has not completed within 1s.\n"
                         "         Press ^C again to also stop waiting for external resources (e.g. in-flight I/O).\n");
    }
    else if (est == 3) {
        jl_safe_printf("\nWARNING: Cancellation has still not completed.\n"
                         "         Press ^C again to forcibly abandon the current task (unsafe; may leak resources).\n");
    }
    else {
        jl_safe_printf("\nWARNING: Process failed to acknowledge SIGINT within 1s.\n"
                         "         You (or a package author) may need to add more @cancel_check's.\n"
                         "         Press ^C again to (unsafely) abandon the current task.\n");
    }
}

static void jl_arm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    // Record the current generation for this arming. Re-arming (a repeat
    // press within the same rung) must NOT invalidate a standing expiry -
    // a press after the offer printed is exactly what escalates. Only a
    // rung change (jl_sigint_escalation_delivered) or episode close
    // (jl_reset_sigint_rescue_timer) opens a fresh generation.
    jl_atomic_store_relaxed(&sigint_rescue_armed_gen,
                            jl_atomic_load_relaxed(&sigint_rescue_gen));
    uv_mutex_lock(&sigint_state_lock);
    if (sigint_rescue_timer_handle == NULL) {
        if (!CreateTimerQueueTimer(&sigint_rescue_timer_handle, NULL,
                                   sigint_rescue_timer_cb, NULL, 1000, 0,
                                   WT_EXECUTEONLYONCE))
            sigint_rescue_timer_handle = NULL;
    }
    uv_mutex_unlock(&sigint_state_lock);
}

JL_DLLEXPORT void jl_disarm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&sigint_state_lock);
    if (sigint_rescue_timer_handle != NULL) {
        DeleteTimerQueueTimer(NULL, sigint_rescue_timer_handle, NULL);
        sigint_rescue_timer_handle = NULL;
    }
    uv_mutex_unlock(&sigint_state_lock);
}

#else
JL_DLLEXPORT void jl_set_sigint_rescue_task(jl_task_t *t) JL_NOTSAFEPOINT
{
    (void)t;
}

static void jl_arm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    // Record the current generation for this arming. Re-arming (a repeat
    // press within the same rung) must NOT invalidate a standing expiry -
    // a press after the offer printed is exactly what escalates. Only a
    // rung change (jl_sigint_escalation_delivered) or episode close
    // (jl_reset_sigint_rescue_timer) opens a fresh generation.
    jl_atomic_store_relaxed(&sigint_rescue_armed_gen,
                            jl_atomic_load_relaxed(&sigint_rescue_gen));
}

JL_DLLEXPORT void jl_disarm_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
}
#endif

#if !defined(JL_HAVE_WIN32_TIMER)
// Called when the rescue timer expires - marks that we should use aggressive
// cancellation on the next user-sent SIGINT. (On Windows the timer callback
// records this itself.)
static void jl_sigint_rescue_timer_expired(void) JL_NOTSAFEPOINT
{
    jl_atomic_store_release(&sigint_rescue_expired_gen,
                            jl_atomic_load_relaxed(&sigint_rescue_armed_gen));
}
#endif

// Check if the rescue timer has expired and we should abandon the current task.
// Returns the rescue task if abandonment should proceed, NULL otherwise.
// Clears the expired flag if it was set.
static int sigint_rescue_expiry_current(void) JL_NOTSAFEPOINT
{
    uint32_t egen = jl_atomic_load_acquire(&sigint_rescue_expired_gen);
    return egen != 0 && egen == jl_atomic_load_relaxed(&sigint_rescue_gen);
}

static jl_task_t *jl_check_sigint_rescue_abandon(void) JL_NOTSAFEPOINT
{
    if (!sigint_rescue_expiry_current())
        return NULL;
    jl_atomic_store_relaxed(&sigint_rescue_expired_gen, 0);
    return sigint_rescue_task;
}

// Non-consuming query of the rescue timer expiry: the julia-side listener
// only escalates the severity of an active cancellation once its grace
// period has passed.
JL_DLLEXPORT int jl_sigint_rescue_timer_expired_peek(void) JL_NOTSAFEPOINT
{
    return sigint_rescue_expiry_current();
}

// An escalated severity was just delivered: consume the expiry and start a
// fresh grace period for the next rung.
JL_DLLEXPORT void jl_sigint_escalation_delivered(void) JL_NOTSAFEPOINT
{
    jl_atomic_fetch_add_relaxed(&sigint_rescue_gen, 1); // invalidate the expiry
    jl_arm_sigint_rescue_timer();
}

// Reset the rescue timer state (e.g. when cancellation succeeds)
JL_DLLEXPORT void jl_reset_sigint_rescue_timer(void) JL_NOTSAFEPOINT
{
    jl_disarm_sigint_rescue_timer();
    jl_atomic_fetch_add_relaxed(&sigint_rescue_gen, 1); // invalidate the expiry
}

// Set while a ^C notification has been posted to the event loop but not yet
// picked up by the julia-side sigint listener. While set, idle threads take
// over running the event loop if its owning thread cannot (e.g. it is blocked
// in a long-running foreign call) - see jl_task_get_next.
_Atomic(int) jl_sigint_dispatch_pending = 0;

// Atomically claim a pending ^C notification: the sigint listener (one per
// threadpool) that wins the claim processes the episode; others re-park.
JL_DLLEXPORT int jl_claim_sigint_dispatch(void) JL_NOTSAFEPOINT
{
    return jl_atomic_exchange_relaxed(&jl_sigint_dispatch_pending, 0);
}

static void deliver_sigint_notification(void) JL_NOTSAFEPOINT
{
    // Runs on a dedicated (non-Julia) thread - the signal listener thread or
    // a Win32 console ctrl handler thread - so blocking on the lock is fine.
    uv_mutex_lock(&sigint_state_lock);
    uv_async_t *cond = jl_atomic_load_relaxed(&sigint_cond_loc);
    if (cond != NULL) {
        jl_atomic_store_release(&jl_sigint_dispatch_pending, 1);
        uv_async_send(cond);
        // Wake every thread: a parked thread's scheduler loop performs the
        // dispatch pass inline on waking (see jl_dispatch_sigint_inline),
        // so delivery does not depend on any particular thread - in
        // particular not on the loop-owning thread, which may be stuck in
        // a long-running foreign call. (No preemption of running tasks:
        // the pass needs only one schedulable thread, and preempting e.g.
        // the REPL frontend would inject a yield into innocent code on
        // every press.)
        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        for (int16_t tid = 0; tid < nthreads; tid++)
            jl_wakeup_thread_from_foreign(tid);
    }
    uv_mutex_unlock(&sigint_state_lock);
}

// The cancellation token source governing the current interactive foreground
// evaluation (the "^C episode source"). The REPL backend / script driver
// installs a fresh source per episode via jl_set_sigint_source (Base keeps
// its own rooted reference for the episode's lifetime); the signal listener
// thread reads this mirror lock-free and uses the object only under a GC
// exclusion (see jl_sigint_request_cancellation).
static _Atomic(jl_value_t *) jl_sigint_source = NULL;

// Episode generation, seqlock-style (odd while a swap is in progress): a
// press records the generation it targeted, so a press for episode A can
// never be delivered to a later-installed episode B (the source pointer
// alone would be ABA-prone); consumption compares generations, never
// dereferences. Writers are serialized by `sigint_state_lock`.
static _Atomic(uint64_t) sigint_episode_gen = 2;
// The generation a not-yet-dispatched press targeted; 0 = none pending.
static _Atomic(uint64_t) sigint_pending_gen = 0;

JL_DLLEXPORT uint64_t jl_set_sigint_source(jl_value_t *src) JL_NOTSAFEPOINT
{
    // The signal thread may load the outgoing source concurrently: its use
    // is covered by GC exclusion, not rooting, and marking a just-retired
    // episode's subtree is the very cancellation that episode's press
    // requested (idempotent, monotonic). The generation brackets the swap
    // (odd = in progress) so the signal thread can snapshot a consistent
    // (source, generation) pair.
    uv_mutex_lock(&sigint_state_lock);
    uint64_t g = jl_atomic_load_relaxed(&sigint_episode_gen) + 1; // odd
    jl_atomic_store_release(&sigint_episode_gen, g);
    jl_atomic_store(&jl_sigint_source,
                    (src == NULL || src == jl_nothing) ? NULL : src);
    jl_atomic_store_release(&sigint_episode_gen, g + 1); // even: complete
    uv_mutex_unlock(&sigint_state_lock);
    // A fresh episode source stands the escalation machinery down (a press
    // for an older generation self-invalidates at consumption, but the
    // rescue timer must not keep escalating the retired episode).
    jl_reset_sigint_rescue_timer();
    // Callers record the returned generation next to their rooted source
    // reference; a stale press (recorded under an older generation) then
    // self-invalidates at consumption.
    return g + 1;
}

// Consume the pending press if (and only if) it targeted the caller's
// episode generation; a press recorded for another generation stays
// pending for the pass that owns that episode's view.
JL_DLLEXPORT int jl_consume_sigint_pending(uint64_t gen) JL_NOTSAFEPOINT
{
    uint64_t p = jl_atomic_load_acquire(&sigint_pending_gen);
    if (p == 0 || p != gen)
        return 0;
    return jl_atomic_cmpswap(&sigint_pending_gen, &p, (uint64_t)0);
}

// The task driving the current foreground evaluation (the caller of
// Base.sigint_new_episode!; rooted by Base like the episode source). The
// direct-abandonment rung consults it: a `bound_cancel_token` binding is
// only published by *compiled* cancellation points, so a foreground victim
// that entered checkless compute without ever blocking (e.g. a spin typed
// straight at the REPL on an unloaded machine) carries no binding at all -
// but abandoning the episode's own foreground task is legitimate whether or
// not it happened to pass a publishing point on the way in.
static _Atomic(jl_task_t *) jl_sigint_fg_task = NULL;

JL_DLLEXPORT void jl_set_sigint_foreground_task(jl_value_t *t) JL_NOTSAFEPOINT
{
    jl_task_t *newt = (t == NULL || t == jl_nothing) ? NULL : (jl_task_t*)t;
    jl_atomic_store_release(&jl_sigint_fg_task, newt);
}

jl_task_t *jl_get_sigint_foreground_task(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_acquire(&jl_sigint_fg_task);
}

JL_DLLEXPORT int jl_peek_sigint_dispatch(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&jl_sigint_dispatch_pending);
}

// Mark every descendant of `root` (including `root`) cancelled at `sev`
// (CAS-max, monotonic). Only state bytes are written - waking parked waiters
// stays with the julia-side listener - but this eager marking lets the
// per-thread cancellation sends identify governed *running* tasks purely by
// their bound source's state, including tasks bound to scoped descendant
// sources (e.g. the source a `@sync` installs).
//
// The traversal is lock-free by the child lists' design (mutators only
// prepend via CAS on `child_head`; the collector unlinks dead entries with
// the world stopped), and linear in the live subtree: a node is descended
// into only when our CAS actually advanced its state (state-advance
// deduplication, like `Base.cancel!`'s walk), and attachment is
// level-triggered (a child linked under a cancelled parent is born marked),
// so no descendant is missed. The worklist may malloc: this runs on
// ordinary threads (the unix sigwait listener thread, a Windows
// console-handler thread), NOT in async-signal context. (The one
// nearly-theoretical exception is a Windows CRT `raise(SIGINT)` via
// crt_sig_handler, which runs on the raising thread - unsafe only if the
// raise itself came from inside malloc.)
static void jl_cancel_subtree_mark(jl_cancel_source_t *root, uint8_t sev) JL_NOTSAFEPOINT
{
    arraylist_t worklist;
    arraylist_new(&worklist, 0);
    jl_cancel_source_t *node = root;
    while (1) {
        // Mark the node before iterating its children (pre-order), pairing
        // with the seq_cst attach dance (publish link, then read the
        // parent's state): an attacher this walk misses observes the state.
        uint8_t st = jl_atomic_load(&node->state);
        int advanced = 0;
        while (st < sev) {
            if (jl_atomic_cmpswap(&node->state, &st, sev)) {
                advanced = 1;
                break;
            }
        }
        if (advanced) {
            jl_value_t *c = jl_atomic_load(&node->child_head);
            while (c != NULL && c != jl_nothing) {
                jl_cancel_source_t *cs = (jl_cancel_source_t*)c;
                // the sibling link lives in the child's link entry for `node`
                jl_cancel_parent_link_t *links = jl_cancel_source_links(cs);
                size_t np = cs->nparents;
                jl_value_t *cnext = jl_nothing;
                for (size_t i = 0; i < np; i++) {
                    if (links[i].parent == node) {
                        cnext = jl_atomic_load_relaxed(&links[i].next);
                        break;
                    }
                }
                arraylist_push(&worklist, cs);
                c = cnext;
            }
        }
        if (worklist.len == 0)
            break;
        node = (jl_cancel_source_t*)arraylist_pop(&worklist);
    }
    arraylist_free(&worklist);
}

// Whether this platform delivers foreign-call cancellation handlers
// asynchronously; every currently supported platform does (the in-handler
// invocation on unix, the suspend-based deliveries on Windows and mach), so
// this is a hook for future platforms whose delivery is level-triggered only.
JL_DLLEXPORT int jl_have_cancel_handler_delivery(void) JL_NOTSAFEPOINT
{
    return 1;
}

// Shared entry point for a user-initiated interrupt (^C): mark the episode
// source cancelled, mark the interrupt as pending, arm the escalation timer,
// and notify the sigint listener task, which performs the remaining
// (Julia-side) delivery work. Callable from non-Julia threads; must not
// allocate GC memory or take Julia-side locks.
static void jl_sigint_request_cancellation(void) JL_NOTSAFEPOINT
{
    // Mark the episode source cancelled (SAFE severity) here rather than
    // leaving it to the julia-side listener: the state byte is what every
    // cancellation point and signal-delivery gate reads, so the signal-based
    // delivery below is sufficient on its own even if the listener never
    // gets to run (a single-threaded session, or every thread stuck in a
    // long foreign call) - exactly the situation ^C must cut through. When
    // the listener does run, it finds the already-cancelled source and does
    // the remaining bookkeeping (waking parked waiters; see
    // `Base.redeliver!`).
    int have_listener = jl_atomic_load_relaxed(&sigint_cond_loc) != NULL;
    // The subtree walk dereferences sources reachable only through weak
    // child links, and this thread does not participate in stop-the-world -
    // exclude the collector for the walk instead. The mirror load happens
    // inside the exclusion so the loaded object cannot have been swept.
    jl_safepoint_exclude_gc_begin();
    // Snapshot a consistent (source, generation) pair: seqlock read
    // against jl_set_sigint_source's bracketed swap.
    uint64_t gen;
    jl_cancel_source_t *src;
    while (1) {
        gen = jl_atomic_load_acquire(&sigint_episode_gen);
        if (gen & 1) {
            jl_cpu_pause();
            continue;
        }
        src = (jl_cancel_source_t*)jl_atomic_load(&jl_sigint_source);
        if (jl_atomic_load_acquire(&sigint_episode_gen) == gen)
            break;
    }
    if (src != NULL) {
        // Mark the whole descendant subtree, so the per-thread sends below
        // can identify a governed running task - even one bound to a scoped
        // descendant source - by its bound source's state alone.
        jl_cancel_subtree_mark(src, 0x1);
        // The heavy side of the asymmetric fence pairing with the
        // compiler-order-only publication of task token bindings at
        // cancellation points (the light side; mirrors
        // `Threads.atomic_fence_heavy()` in `Base.cancel!`): after this,
        // either the sends below observe a running task's binding, or the
        // task's next cancellation point observes the state writes above.
        jl_membarrier();
    }
    jl_safepoint_exclude_gc_end();
    // Everything below reads only per-thread state and tasks' strongly-held
    // fields - no weakly-reachable source pointers.
    if (src != NULL) {
        // Mark the dispatch pending BEFORE the per-thread sends (idle
        // threads keep the event loop moving until the listener claims it).
        // Skipped when no listener is registered (e.g. a trimmed binary that
        // stubs it out): nothing would ever claim the flag, and the
        // scheduler's stay-awake gate would keep idle threads polling
        // forever.
        if (have_listener)
            jl_atomic_store_release(&jl_sigint_dispatch_pending, 1);
        // Interrupt asynchronously-interruptible regions right away, on
        // every thread: the request-5 dispatch delivers to tasks whose own
        // bound token source is (now) cancelled, and is a no-op for threads
        // running unrelated (or no) work.
        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        for (int16_t tid = 0; tid < (int16_t)nthreads; tid++)
            jl_send_cancellation_signal(tid);
    }
    jl_atomic_store_release(&sigint_pending_gen, gen);
    // Set a timer for the event loop to run and process the cancellation. If
    // this does not happen in time, we will advance to more aggressive
    // cancellation.
    jl_arm_sigint_rescue_timer();
    deliver_sigint_notification();
}

// Whether a press recorded for the *current* episode generation is still
// unconsumed. A pending marker left by a retired episode self-invalidates
// at consumption and must not read as pressure on the current episode.
static int jl_sigint_press_pending(void) JL_NOTSAFEPOINT
{
    uint64_t p = jl_atomic_load_acquire(&sigint_pending_gen);
    return p != 0 && p == jl_atomic_load_acquire(&sigint_episode_gen);
}

// Episode states:
//  0 - no active ^C episode
//  1 - a request that its target never observed: the pending marker, or a
//      listener-delivered SAFE cancellation that was never acknowledged
//      (e.g. the target is compute-bound with no cancellation points)
//  2 - SAFE severity acknowledged (the target is unwinding, but stuck)
//  3 - ABANDON_EXTERNAL severity active
//  4 - ABANDON_ALL severity active
static int jl_sigint_episode_state(void) JL_NOTSAFEPOINT
{
    jl_cancel_source_t *src = (jl_cancel_source_t*)jl_atomic_load_relaxed(&jl_sigint_source);
    int pending = jl_sigint_press_pending();
    if (src == NULL)
        return pending ? 1 : 0;
    uint8_t sev = jl_atomic_load_relaxed(&src->state);
    if (sev == 0x00)
        return pending ? 1 : 0;
    uint8_t delivered = jl_atomic_load_relaxed(&src->delivered);
    if (sev == 0x01)
        return (delivered & (1 << 0x01)) ? 2 : 1; // undelivered SAFE was never observed
    return sev == 0x03 ? 3 : 4;
}

// Whether the C side may abandon the interrupted task directly (bypassing the
// julia-side escalation): either the initial SAFE request was never even
// delivered (no thread was available to run the sigint listener, e.g. a
// single-threaded session with the thread stuck in compute), or the listener
// already escalated to an abandoning severity and the task still did not
// yield.
static int jl_sigint_direct_abandon_allowed(void) JL_NOTSAFEPOINT
{
    int est = jl_sigint_episode_state();
    if (est == 1)
        return 1; // the initial request was never even delivered
    // For delivered episodes the julia-side listener drives the graded
    // escalation - unless it CANNOT run because the session's only worker
    // thread is monopolized by the stuck victim (its notification still
    // undispatched). With more worker threads an unclaimed dispatch only
    // means the listener has not run *yet* - a delayed but graded
    // escalation beats ripping away whatever thread 0 happens to be
    // running at that moment.
    if (est >= 2 && est <= 4 && jl_atomic_load_relaxed(&jl_sigint_dispatch_pending) &&
        jl_n_threads_per_pool[JL_THREADPOOL_ID_INTERACTIVE] +
        jl_n_threads_per_pool[JL_THREADPOOL_ID_DEFAULT] == 1)
        return 1;
    return 0;
}

// Re-publish a refused direct abandonment. A refusal is transient (the
// victim held a runtime resource - e.g. an allocator lock or a claimed
// scheduler sleep slot - at the delivery instant), so "still trying" must
// be real: re-validate the same gates as the press path (a live episode
// whose listener cannot make progress, a governed victim monopolizing
// thread 0) and re-publish against the thread's current task. Returns the
// new request tid, or -1 when the episode completed or the victim is no
// longer a legitimate target.
static int16_t jl_retry_direct_abandon(void) JL_NOTSAFEPOINT
{
    if (!jl_sigint_direct_abandon_allowed())
        return -1;
    jl_task_t *rescue = sigint_rescue_task;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    if (rescue == NULL || ptls2 == NULL)
        return -1;
    jl_task_t *ct = jl_atomic_load_relaxed(&ptls2->current_task);
    jl_value_t *sigsrc = jl_atomic_load_acquire(&jl_sigint_source);
    if (ct == NULL || ct == rescue || sigsrc == NULL)
        return -1;
    jl_value_t *bound = jl_atomic_load_acquire(&ct->bound_cancel_token);
    int governed = bound != NULL && bound != jl_nothing &&
        jl_cancel_source_subtree_member(bound, sigsrc);
    if (!governed && ct == jl_get_sigint_foreground_task())
        governed = 1;
    if (!governed)
        return -1;
    int reqtid = jl_abandon_task_request(ct, rescue, NULL, NULL);
    return reqtid < 0 ? (int16_t)-1 : (int16_t)reqtid;
}

// Propagate a pending ^C episode to the interrupted task's own bound
// cancellation source. The julia-side sigint listener normally performs the
// tree walk that carries the episode's cancellation down to scoped child
// sources (e.g. the source a @sync installs) - but the listener may be
// starved: in a single-threaded process whose only thread runs a
// compute-bound task, nothing ever schedules it, and a task polling
// cancellation points against its *own* (descendant) source would never
// observe the ^C. Called from the per-thread cancellation-delivery paths
// (which every ^C already triggers on every thread) while the listener has
// not yet claimed the dispatch: if the task's bound source is governed by
// the episode source, CAS-max the episode's state byte into it directly -
// a single async-signal-safe byte write; the listener's eventual walk
// redoes the remaining bookkeeping (waiter wakes, delivered bits)
// level-triggered. Returns 1 if the bound source is (now) cancelled.
static int jl_sigint_propagate_to_bound(jl_value_t *bound) JL_NOTSAFEPOINT
{
    if (bound == NULL || bound == jl_nothing)
        return 0;
    if (!jl_atomic_load_relaxed(&jl_sigint_dispatch_pending))
        return 0;
    jl_cancel_source_t *sigsrc = (jl_cancel_source_t*)jl_atomic_load_acquire(&jl_sigint_source);
    if (sigsrc == NULL || (jl_value_t*)sigsrc == bound)
        return 0;
    uint8_t sst = jl_atomic_load_relaxed(&sigsrc->state);
    if (sst == 0)
        return 0;
    if (!jl_cancel_source_subtree_member(bound, (jl_value_t*)sigsrc))
        return 0;
    jl_cancel_source_t *bsrc = (jl_cancel_source_t*)bound;
    uint8_t bst = jl_atomic_load_relaxed(&bsrc->state);
    while (bst < sst) { // CAS-max: the state byte is the severity, ordered
        if (jl_atomic_cmpswap(&bsrc->state, &bst, sst))
            break;
    }
    return 1;
}

static void stack_overflow_warning(void)
{
    jl_safe_printf("Warning: detected a stack overflow; program state may be corrupted, so further execution might be unreliable.\n");
}

// Async-signal-safe replacement for libc strsignal(). We call this from fatal-signal
// handlers, and glibc's strsignal() is not async-signal-safe: it routes through gettext
// (to localize the description), which calls malloc(). If the interrupted thread already
// held the malloc arena lock, that reentrant malloc() self-deadlocks. A fixed table of
// string literals avoids gettext/malloc entirely and is portable across libc flavors
// (musl/BSD/macOS lack glibc's sigdescr_np/sigabbrev_np). Cases are #ifdef-guarded so this
// compiles wherever a given signal is (or is not) defined.
static const char *jl_strsignal(int sig) JL_NOTSAFEPOINT
{
    switch (sig) {
#ifdef SIGHUP
    case SIGHUP:     return "Hangup";
#endif
#ifdef SIGINT
    case SIGINT:     return "Interrupt";
#endif
#ifdef SIGQUIT
    case SIGQUIT:    return "Quit";
#endif
#ifdef SIGILL
    case SIGILL:     return "Illegal instruction";
#endif
#ifdef SIGTRAP
    case SIGTRAP:    return "Trace/breakpoint trap";
#endif
#ifdef SIGABRT
    case SIGABRT:    return "Aborted";
#endif
#if defined(SIGABRT_COMPAT) && (!defined(SIGABRT) || SIGABRT_COMPAT != SIGABRT)
    case SIGABRT_COMPAT: return "Aborted";
#endif
#ifdef SIGBUS
    case SIGBUS:     return "Bus error";
#endif
#ifdef SIGFPE
    case SIGFPE:     return "Floating point exception";
#endif
#ifdef SIGKILL
    case SIGKILL:    return "Killed";
#endif
#ifdef SIGUSR1
    case SIGUSR1:    return "User defined signal 1";
#endif
#ifdef SIGSEGV
    case SIGSEGV:    return "Segmentation fault";
#endif
#ifdef SIGUSR2
    case SIGUSR2:    return "User defined signal 2";
#endif
#ifdef SIGPIPE
    case SIGPIPE:    return "Broken pipe";
#endif
#ifdef SIGALRM
    case SIGALRM:    return "Alarm clock";
#endif
#ifdef SIGTERM
    case SIGTERM:    return "Terminated";
#endif
#ifdef SIGBREAK
    case SIGBREAK:   return "Break";
#endif
#ifdef SIGSTKFLT
    case SIGSTKFLT:  return "Stack fault";
#endif
#ifdef SIGCHLD
    case SIGCHLD:    return "Child exited";
#endif
#ifdef SIGCONT
    case SIGCONT:    return "Continued";
#endif
#ifdef SIGSTOP
    case SIGSTOP:    return "Stopped (signal)";
#endif
#ifdef SIGTSTP
    case SIGTSTP:    return "Stopped";
#endif
#ifdef SIGTTIN
    case SIGTTIN:    return "Stopped (tty input)";
#endif
#ifdef SIGTTOU
    case SIGTTOU:    return "Stopped (tty output)";
#endif
#ifdef SIGURG
    case SIGURG:     return "Urgent I/O condition";
#endif
#ifdef SIGXCPU
    case SIGXCPU:    return "CPU time limit exceeded";
#endif
#ifdef SIGXFSZ
    case SIGXFSZ:    return "File size limit exceeded";
#endif
#ifdef SIGVTALRM
    case SIGVTALRM:  return "Virtual timer expired";
#endif
#ifdef SIGPROF
    case SIGPROF:    return "Profiling timer expired";
#endif
#ifdef SIGWINCH
    case SIGWINCH:   return "Window changed";
#endif
#ifdef SIGSYS
    case SIGSYS:     return "Bad system call";
#endif
    default:         return "Unknown signal";
    }
}

#if defined(_WIN32)
#include "signals-win.c"
#else
#include "signals-unix.c"
#endif

// jl_send_reset_signal is the static per-platform delivery defined by the
// file included above; `reset_code` becomes the reset point's setjmp return.

JL_DLLEXPORT void jl_send_cancellation_signal(int16_t tid) JL_NOTSAFEPOINT
{
    jl_send_reset_signal(tid, JL_RESET_CODE_CANCEL);
}

// Request a cooperative yield from the target thread's current task: mark
// the task (honored at its next cancellation point) before kicking it out
// of any published reset region.
JL_DLLEXPORT void jl_send_preempt_signal(int16_t tid) JL_NOTSAFEPOINT
{
    if (tid < 0 || tid >= jl_atomic_load_acquire(&jl_n_threads))
        return;
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL)
        return;
    jl_task_t *ct2 = jl_atomic_load_relaxed(&ptls2->current_task);
    if (ct2 == NULL)
        return;
    jl_atomic_store_release(&ct2->preempt_request, 1);
    jl_send_reset_signal(tid, JL_RESET_CODE_PREEMPT);
}

// Deliver cancellation shootdowns to every thread whose current task is
// bound to a now-cancelled token source; called by `cancel!` after
// propagation and a heavy fence. The check is only a hint (the sender
// re-validates; a missed task recovers level-triggered), and the unrooted
// token read is safe because this thread runs GC-unsafe.
JL_DLLEXPORT void jl_shootdown_cancelled_tasks(void) JL_NOTSAFEPOINT
{
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    for (int16_t tid = 0; tid < nthreads; tid++) {
        jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        if (ptls2 == NULL)
            continue;
        jl_task_t *ct2 = jl_atomic_load_relaxed(&ptls2->current_task);
        if (ct2 == NULL)
            continue;
        jl_value_t *bound = jl_atomic_load_relaxed(&ct2->bound_cancel_token);
        if (bound == NULL || bound == jl_nothing ||
            jl_atomic_load_relaxed(&((jl_cancel_source_t*)bound)->state) == 0)
            continue;
        jl_send_cancellation_signal(tid);
    }
}

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
#elif defined(_OS_FREEBSD_) && defined(_CPU_AARCH64_)
    return ((ucontext_t*)_ctx)->uc_mcontext.mc_gpregs.gp_elr;
#elif defined(_OS_LINUX_) && defined(_CPU_ARM_)
    return ((ucontext_t*)_ctx)->uc_mcontext.arm_pc;
#elif defined(_OS_LINUX_) && defined(_CPU_RISCV64_)
    return ((ucontext_t*)_ctx)->uc_mcontext.__gregs[REG_PC];
#else
    // TODO for PPC
    return 0;
#endif
}

static void jl_fprint_sigill(ios_t *s, void *_ctx)
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
        jl_safe_fprintf(s, "Unreachable reached at %p\n", (void*)pc);
    }
    else {
        jl_safe_fprintf(s, "Invalid instruction at %p: ", (void*)pc);
        for (int i = 0;i < len;i++) {
            if (i == 0) {
                jl_safe_fprintf(s, "0x%02" PRIx8, inst[i]);
            }
            else {
                jl_safe_fprintf(s, ", 0x%02" PRIx8, inst[i]);
            }
        }
        jl_safe_fprintf(s, "\n");
    }
#elif defined(_OS_LINUX_) && defined(_CPU_AARCH64_)
    uint32_t inst = 0;
    size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
    if (len < 4)
        jl_safe_fprintf(s, "Fault when reading instruction: %d bytes read\n", (int)len);
    if (inst == 0xd4200020) { // brk #0x1
        // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
        jl_safe_fprintf(s, "Unreachable reached at %p\n", pc);
    }
    else {
        jl_safe_fprintf(s, "Invalid instruction at %p: 0x%08" PRIx32 "\n", pc, inst);
    }
#elif defined(_OS_LINUX_) && defined(_CPU_ARM_)
    ucontext_t *ctx = (ucontext_t*)_ctx;
    if (ctx->uc_mcontext.arm_cpsr & (1 << 5)) {
        // Thumb
        uint16_t inst[2] = {0, 0};
        size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
        if (len < 2)
            jl_safe_fprintf(s, "Fault when reading Thumb instruction: %d bytes read\n", (int)len);
        // LLVM and GCC uses different code for the trap...
        if (inst[0] == 0xdefe || inst[0] == 0xdeff) {
            // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
            jl_safe_fprintf(s, "Unreachable reached in Thumb mode at %p: 0x%04" PRIx16 "\n",
                            (void*)pc, inst[0]);
        }
        else {
            jl_safe_fprintf(s, "Invalid Thumb instruction at %p: 0x%04" PRIx16 ", 0x%04" PRIx16 "\n",
                            (void*)pc, inst[0], inst[1]);
        }
    }
    else {
        uint32_t inst = 0;
        size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
        if (len < 4)
            jl_safe_fprintf(s, "Fault when reading instruction: %d bytes read\n", (int)len);
        // LLVM and GCC uses different code for the trap...
        if (inst == 0xe7ffdefe || inst == 0xe7f000f0) {
            // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
            jl_safe_fprintf(s, "Unreachable reached in ARM mode at %p: 0x%08" PRIx32 "\n",
                            (void*)pc, inst);
        }
        else {
            jl_safe_fprintf(s, "Invalid ARM instruction at %p: 0x%08" PRIx32 "\n", (void*)pc, inst);
        }
    }
#elif defined(_OS_LINUX_) && defined(_CPU_RISCV64_)
    uint32_t inst = 0;
    size_t len = jl_safe_read_mem(pc, (char*)&inst, 4);
    if (len < 2)
        jl_safe_printf("Fault when reading instruction: %d bytes read\n", (int)len);
    if (inst == 0x00100073 || // ebreak
        inst == 0xc0001073 || // unimp (pseudo-instruction for illegal `csrrw x0, cycle, x0`)
        (inst & ((1 << 16) - 1)) == 0x0000) { // c.unimp (compressed form)
        // The signal might actually be SIGTRAP instead, doesn't hurt to handle it here though.
        jl_safe_printf("Unreachable reached at %p\n", pc);
    }
    else {
        jl_safe_printf("Invalid instruction at %p: 0x%08" PRIx32 "\n", pc, inst);
    }
#else
    // TODO for PPC
    (void)_ctx;
#endif
}

// make it invalid for a task to return from this point to its stack
// this is generally quite a foolish operation, but does free you up to do
// arbitrary things on this stack now without worrying about corrupt state that
// existed already on it
void jl_task_frame_noreturn(jl_task_t *ct)
{
    jl_set_safe_restore(NULL);
    if (ct) {
        ct->gcstack = NULL;
        ct->eh = NULL;
        ct->world_age = 1;
        // Force all locks to drop. Is this a good idea? Of course not. But the alternative would probably deadlock instead of crashing.
        jl_ptls_t ptls = ct->ptls;
        small_arraylist_t *locks = &ptls->locks;
#ifndef __clang_safetyanalysis__
        for (size_t i = locks->len; i > 0; i--)
            jl_mutex_unlock_nogc((jl_mutex_t*)locks->items[i - 1]);
#endif
        locks->len = 0;
        ptls->in_pure_callback = 0;
        ptls->in_finalizer = 0;
        ptls->defer_signal = 0;
        // forcibly exit GC (if we were in it) or safe into unsafe, without the mandatory safepoint
        jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_UNSAFE);
        surprise_wakeup(ptls);
        // allow continuing to use a Task that should have already died--unsafe necromancy!
        jl_atomic_store_relaxed(&ct->_state, JL_TASK_STATE_RUNNABLE);
    }
}

// what to do on a critical error on a thread
void jl_fprint_critical_error(ios_t *s, int sig, int si_code, bt_context_t *context, jl_task_t *ct)
{
    jl_bt_element_t *bt_data = ct ? ct->ptls->bt_data : NULL;
    size_t *bt_size = ct ? &ct->ptls->bt_size : NULL;
    size_t i, n = ct ? *bt_size : 0;
    // Threads unknown to Julia have no ptls, and hence no pre-allocated
    // backtrace buffer; a small stack buffer is used for them instead.
    jl_bt_element_t bt_data_foreign[JL_BT_MAX_ENTRY_SIZE * 8];
    if (sig) {
        // kill this task, so that we cannot get back to it accidentally (via an untimely ^C or jl_fprint_backtrace in jl_exit)
        // and also resets the state of ct and ptls so that some code can run on this task again
        jl_task_frame_noreturn(ct);
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
        if (si_code)
            jl_safe_fprintf(s, "\n[%d] signal %d (%d): %s\n", getpid(), sig, si_code, jl_strsignal(sig));
        else
            jl_safe_fprintf(s, "\n[%d] signal %d: %s\n", getpid(), sig, jl_strsignal(sig));
        if (sig == SIGQUIT) {
            jl_print_task_backtraces(0);
        }
    }
    jl_safe_fprintf(s, "in expression starting at %s:%d\n", jl_atomic_load_relaxed(&jl_filename), jl_atomic_load_relaxed(&jl_lineno));
    if (context && ct) {
        // Must avoid extended backtrace frames here unless we're sure bt_data
        // is properly rooted.
        *bt_size = n = rec_backtrace_ctx(bt_data, JL_MAX_BT_SIZE, context, NULL);
    }
    else if (context) {
        // The faulting thread was not created or adopted by Julia (e.g. a
        // thread started by foreign code that crashed without ever calling
        // into Julia), so it has no Julia task or backtrace buffer. Record a
        // native-only backtrace into the local buffer instead, so that at
        // least the faulting instruction pointer is reported.
#ifdef _OS_LINUX_
        char thread_name[16]; // the kernel limits thread names to 16 bytes (TASK_COMM_LEN)
        if (prctl(PR_GET_NAME, (unsigned long)thread_name, 0, 0, 0) != 0)
            thread_name[0] = '\0';
        jl_safe_fprintf(s, "unknown thread \"%s\" (os tid %ld); this thread is not managed by Julia, no Julia backtrace available\n",
                        thread_name, (long)syscall(SYS_gettid));
#else
        jl_safe_fprintf(s, "unknown thread; this thread is not managed by Julia, no Julia backtrace available\n");
#endif
        bt_data = bt_data_foreign;
        n = rec_backtrace_ctx(bt_data, sizeof(bt_data_foreign) / sizeof(jl_bt_element_t), context, NULL);
    }
    for (i = 0; i < n; i += jl_bt_entry_size(bt_data + i)) {
        jl_fprint_bt_entry_codeloc(s, bt_data + i);
    }
    if (n == 0 && context)
        jl_safe_fprintf(s, "no backtrace could be recorded from the signal context\n");
    jl_gc_debug_fprint_status(s);
    jl_gc_debug_fprint_critical_error(s);
}

#ifdef __cplusplus
}
#endif
