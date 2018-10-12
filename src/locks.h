// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LOCKS_H
#define JL_LOCKS_H

#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// Lock acquire and release primitives

// JL_LOCK and jl_mutex_lock are GC safe points while JL_LOCK_NOGC
// and jl_mutex_lock_nogc are not.
// Always use JL_LOCK unless no one holding the lock can trigger a GC or GC
// safepoint. JL_LOCK_NOGC should only be needed for GC internal locks.
// The JL_LOCK* and JL_UNLOCK* macros are no-op for non-threading build
// while the jl_mutex_* functions are always locking and unlocking the locks.

static inline void jl_mutex_wait(jl_mutex_t *lock, int safepoint)
{
    unsigned long self = jl_thread_self();
    unsigned long owner = jl_atomic_load_acquire(&lock->owner);
    if (owner == self) {
        lock->count++;
        return;
    }
    while (1) {
        if (owner == 0 &&
            jl_atomic_compare_exchange(&lock->owner, 0, self) == 0) {
            lock->count = 1;
            return;
        }
        if (safepoint) {
            jl_ptls_t ptls = jl_get_ptls_states();
            jl_gc_safepoint_(ptls);
        }
        jl_cpu_pause();
        owner = lock->owner;
    }
}

static inline void jl_mutex_lock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
#ifndef __clang_analyzer__
    // Hide this body from the analyzer, otherwise it complains that we're calling
    // a non-safepoint from this function. The 0 arguments guarantees that we do
    // not reach the safepoint, but the analyzer can't figure that out
    jl_mutex_wait(lock, 0);
#endif
}

#ifdef JULIA_ENABLE_THREADING
static inline void jl_lock_frame_push(jl_mutex_t *lock)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // For early bootstrap
    if (__unlikely(!ptls->current_task))
        return;
    arraylist_t *locks = &ptls->current_task->locks;
    size_t len = locks->len;
    if (__unlikely(len >= locks->max)) {
        arraylist_grow(locks, 1);
    }
    else {
        locks->len = len + 1;
    }
    locks->items[len] = (void*)lock;
}
static inline void jl_lock_frame_pop(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (__likely(ptls->current_task)) {
        ptls->current_task->locks.len--;
    }
}
#else
static inline void jl_lock_frame_push(jl_mutex_t *lock)
{
    (void)lock;
}
static inline void jl_lock_frame_pop(void)
{
}
#endif // ifndef JULIA_ENABLE_THREADING

#define JL_SIGATOMIC_BEGIN() do {               \
        jl_get_ptls_states()->defer_signal++;   \
        jl_signal_fence();                      \
    } while (0)
#define JL_SIGATOMIC_END() do {                                 \
        jl_signal_fence();                                      \
        if (--jl_get_ptls_states()->defer_signal == 0) {        \
            jl_sigint_safepoint(jl_get_ptls_states());          \
        }                                                       \
    } while (0)

static inline void jl_mutex_lock(jl_mutex_t *lock)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_SIGATOMIC_BEGIN();
    jl_mutex_wait(lock, 1);
    jl_lock_frame_push(lock);
    jl_gc_enable_finalizers(ptls, 0);
}

/* Call this function for code that could be called from either a managed
   or an unmanaged thread */
static inline void jl_mutex_lock_maybe_nogc(jl_mutex_t *lock)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->safepoint) {
        jl_mutex_lock(lock);
    } else {
        jl_mutex_lock_nogc(lock);
    }
}

static inline void jl_mutex_unlock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
#ifndef __clang_analyzer__
    assert(lock->owner == jl_thread_self() &&
           "Unlocking a lock in a different thread.");
    if (--lock->count == 0) {
        jl_atomic_store_release(&lock->owner, 0);
        jl_cpu_wake();
    }
#endif
}

static inline void jl_mutex_unlock(jl_mutex_t *lock)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_mutex_unlock_nogc(lock);
    jl_gc_enable_finalizers(ptls, 1);
    jl_lock_frame_pop();
    JL_SIGATOMIC_END();
}

static inline void jl_mutex_unlock_maybe_nogc(jl_mutex_t *lock) {
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->safepoint) {
        jl_mutex_unlock(lock);
    } else {
        jl_mutex_unlock_nogc(lock);
    }
}

static inline void jl_mutex_init(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
    lock->owner = 0;
    lock->count = 0;
}

#ifdef JULIA_ENABLE_THREADING
#define JL_MUTEX_INIT(m) jl_mutex_init(m)
#define JL_LOCK(m) jl_mutex_lock(m)
#define JL_UNLOCK(m) jl_mutex_unlock(m)
#define JL_LOCK_NOGC(m) jl_mutex_lock_nogc(m)
#define JL_UNLOCK_NOGC(m) jl_mutex_unlock_nogc(m)
#else // JULIA_ENABLE_THREADING
static inline void jl_mutex_check_type(jl_mutex_t *m)
{
    (void)m;
}
#define JL_MUTEX_INIT(m) jl_mutex_check_type(m)
#define JL_LOCK(m) do {                                   \
        JL_SIGATOMIC_BEGIN();                             \
        jl_gc_enable_finalizers(jl_get_ptls_states(), 0); \
        jl_mutex_check_type(m);                           \
    } while (0)
#define JL_UNLOCK(m) do {                                       \
        jl_gc_enable_finalizers(jl_get_ptls_states(), 1);       \
        jl_mutex_check_type(m);                                 \
        JL_SIGATOMIC_END();                                     \
    } while (0)
#define JL_LOCK_NOGC(m) jl_mutex_check_type(m)
#define JL_UNLOCK_NOGC(m) jl_mutex_check_type(m)
#endif // JULIA_ENABLE_THREADING

#ifdef __cplusplus
}
#endif

#endif
