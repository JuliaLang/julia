// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LOCKS_H
#define JL_LOCKS_H

#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// Lock acquire and release primitives

// JL_LOCK and jl_mutex_lock are GC safe points, use uv_mutex_t if that is not desired.
// Always use JL_LOCK unless no one holding the lock can trigger a GC or GC
// safepoint. uv_mutex_t should only be needed for GC internal locks.
// The JL_LOCK* and JL_UNLOCK* macros are no-op for non-threading build
// while the jl_mutex_* functions are always locking and unlocking the locks.

JL_DLLEXPORT void _jl_mutex_wait(jl_task_t *self, jl_mutex_t *lock, int safepoint);
JL_DLLEXPORT void _jl_mutex_lock(jl_task_t *self, jl_mutex_t *lock);
JL_DLLEXPORT int _jl_mutex_trylock_nogc(jl_task_t *self, jl_mutex_t *lock) JL_NOTSAFEPOINT;
JL_DLLEXPORT int _jl_mutex_trylock(jl_task_t *self, jl_mutex_t *lock);
JL_DLLEXPORT void _jl_mutex_unlock(jl_task_t *self, jl_mutex_t *lock);
JL_DLLEXPORT void _jl_mutex_unlock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT;

static inline void jl_mutex_wait(jl_mutex_t *lock, int safepoint)
{
    _jl_mutex_wait(jl_current_task, lock, safepoint);
}

static inline void jl_mutex_lock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
#ifndef __clang_gcanalyzer__
    // Hide this body from the analyzer, otherwise it complains that we're calling
    // a non-safepoint from this function. The 0 arguments guarantees that we do
    // not reach the safepoint, but the analyzer can't figure that out
    jl_mutex_wait(lock, 0);
#endif
}

#define JL_SIGATOMIC_BEGIN() do {               \
        jl_current_task->ptls->defer_signal++;  \
        jl_signal_fence();                      \
    } while (0)
#define JL_SIGATOMIC_END() do {                                 \
        jl_signal_fence();                                      \
        if (--jl_current_task->ptls->defer_signal == 0) {       \
            jl_sigint_safepoint(jl_current_task->ptls);         \
        }                                                       \
    } while (0)

#define JL_SIGATOMIC_BEGIN_self() do {          \
        self->ptls->defer_signal++;             \
        jl_signal_fence();                      \
    } while (0)
#define JL_SIGATOMIC_END_self() do {            \
        jl_signal_fence();                      \
        if (--self->ptls->defer_signal == 0) {  \
            jl_sigint_safepoint(self->ptls);    \
        }                                       \
    } while (0)

static inline void jl_mutex_lock(jl_mutex_t *lock)
{
    _jl_mutex_lock(jl_current_task, lock);
}

static inline int jl_mutex_trylock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
    return _jl_mutex_trylock_nogc(jl_current_task, lock);
}

static inline int jl_mutex_trylock(jl_mutex_t *lock)
{
    return _jl_mutex_trylock(jl_current_task, lock);
}

static inline void jl_mutex_unlock(jl_mutex_t *lock)
{
    _jl_mutex_unlock(jl_current_task, lock);
}

static inline void jl_mutex_unlock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
    _jl_mutex_unlock_nogc(lock);
}

static inline void jl_mutex_init(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&lock->owner, (jl_task_t*)NULL);
    lock->count = 0;
}

#define JL_MUTEX_INIT(m) jl_mutex_init(m)
#define JL_LOCK(m) jl_mutex_lock(m)
#define JL_UNLOCK(m) jl_mutex_unlock(m)
#define JL_LOCK_NOGC(m) jl_mutex_lock_nogc(m)
#define JL_UNLOCK_NOGC(m) jl_mutex_unlock_nogc(m)

#ifdef __cplusplus
}
#endif

#endif
