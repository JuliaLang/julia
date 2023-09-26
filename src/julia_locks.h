// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LOCKS_H
#define JL_LOCKS_H

#ifdef __cplusplus
extern "C" {
#endif

// Lock acquire and release primitives

// JL_SPIN_LOCK and jl_spin_mutex_lock are GC safe points, use uv_mutex_t if that is not desired.
// Always use JL_SPIN_LOCK unless no one holding the lock can trigger a GC or GC
// safepoint. uv_mutex_t should only be needed for GC internal locks.
// The JL_SPIN_LOCK* and JL_SPIN_UNLOCK* macros are no-op for non-threading build
// while the jl_mutex_* functions are always locking and unlocking the locks.

JL_DLLEXPORT void _jl_spin_mutex_init(jl_spin_mutex_t *lock, const char *name) JL_NOTSAFEPOINT;
JL_DLLEXPORT void _jl_spin_mutex_wait(jl_task_t *self, jl_spin_mutex_t *lock, int safepoint);
JL_DLLEXPORT void _jl_spin_mutex_lock(jl_task_t *self, jl_spin_mutex_t *lock);
JL_DLLEXPORT int _jl_spin_mutex_trylock_nogc(jl_task_t *self, jl_spin_mutex_t *lock) JL_NOTSAFEPOINT;
JL_DLLEXPORT int _jl_spin_mutex_trylock(jl_task_t *self, jl_spin_mutex_t *lock);
JL_DLLEXPORT void _jl_spin_mutex_unlock(jl_task_t *self, jl_spin_mutex_t *lock);
JL_DLLEXPORT void _jl_spin_mutex_unlock_nogc(jl_spin_mutex_t *lock) JL_NOTSAFEPOINT;

JL_DLLEXPORT void _jl_sleep_mutex_init(jl_sleep_mutex_t *lock, const char *name) JL_NOTSAFEPOINT;
JL_DLLEXPORT void _jl_sleep_mutex_wait(jl_task_t *self, jl_sleep_mutex_t *lock, int safepoint);
JL_DLLEXPORT void _jl_sleep_mutex_lock(jl_task_t *self, jl_sleep_mutex_t *lock);
JL_DLLEXPORT int _jl_sleep_mutex_trylock_nogc(jl_task_t *self, jl_sleep_mutex_t *lock) JL_NOTSAFEPOINT;
JL_DLLEXPORT int _jl_sleep_mutex_trylock(jl_task_t *self, jl_sleep_mutex_t *lock);
JL_DLLEXPORT void _jl_sleep_mutex_unlock(jl_task_t *self, jl_sleep_mutex_t *lock);
JL_DLLEXPORT void _jl_sleep_mutex_unlock_nogc(jl_sleep_mutex_t *lock) JL_NOTSAFEPOINT;

JL_DLLEXPORT void _jl_dyn_mutex_unlock(jl_task_t *self, void *lock);
JL_DLLEXPORT void _jl_dyn_mutex_unlock_nogc(void *lock) JL_NOTSAFEPOINT;

static inline void jl_spin_mutex_wait(jl_spin_mutex_t *lock, int safepoint)
{
    _jl_spin_mutex_wait(jl_current_task, lock, safepoint);
}

static inline void jl_spin_mutex_lock_nogc(jl_spin_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
{
#ifndef __clang_gcanalyzer__
    // Hide this body from the analyzer, otherwise it complains that we're calling
    // a non-safepoint from this function. The 0 arguments guarantees that we do
    // not reach the safepoint, but the analyzer can't figure that out
    jl_spin_mutex_wait(lock, 0);
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

static inline void jl_spin_mutex_lock(jl_spin_mutex_t *lock)
{
    _jl_spin_mutex_lock(jl_current_task, lock);
}

static inline int jl_spin_mutex_trylock_nogc(jl_spin_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
{
    return _jl_spin_mutex_trylock_nogc(jl_current_task, lock);
}

static inline int jl_spin_mutex_trylock(jl_spin_mutex_t *lock)
{
    return _jl_spin_mutex_trylock(jl_current_task, lock);
}

static inline void jl_spin_mutex_unlock(jl_spin_mutex_t *lock)
{
    _jl_spin_mutex_unlock(jl_current_task, lock);
}

static inline void jl_spin_mutex_unlock_nogc(jl_spin_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE
{
    _jl_spin_mutex_unlock_nogc(lock);
}

static inline void jl_spin_mutex_init(jl_spin_mutex_t *lock, const char *name) JL_NOTSAFEPOINT
{
    _jl_spin_mutex_init(lock, name);
}

static inline void jl_sleep_mutex_wait(jl_sleep_mutex_t *lock, int safepoint)
{
    _jl_sleep_mutex_wait(jl_current_task, lock, safepoint);
}

static inline void jl_sleep_mutex_lock(jl_sleep_mutex_t *lock)
{
    _jl_sleep_mutex_lock(jl_current_task, lock);
}

static inline void jl_sleep_mutex_lock_nogc(jl_sleep_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
{
#ifndef __clang_gcanalyzer__
    // Hide this body from the analyzer, otherwise it complains that we're calling
    // a non-safepoint from this function. The 0 arguments guarantees that we do
    // not reach the safepoint, but the analyzer can't figure that out
    jl_sleep_mutex_wait(lock, 0);
#endif
}

static inline int jl_sleep_mutex_trylock_nogc(jl_sleep_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
{
    return _jl_sleep_mutex_trylock_nogc(jl_current_task, lock);
}

static inline int jl_sleep_mutex_trylock(jl_sleep_mutex_t *lock)
{
    return _jl_sleep_mutex_trylock(jl_current_task, lock);
}

static inline void jl_sleep_mutex_unlock(jl_sleep_mutex_t *lock)
{
    _jl_sleep_mutex_unlock(jl_current_task, lock);
}

static inline void jl_sleep_mutex_unlock_nogc(jl_sleep_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE
{
    _jl_sleep_mutex_unlock_nogc(lock);
}

static inline void jl_sleep_mutex_init(jl_sleep_mutex_t *lock, const char *name) JL_NOTSAFEPOINT
{
    _jl_sleep_mutex_init(lock, name);
}

static inline void jl_dyn_mutex_unlock(void *lock)
{
    _jl_dyn_mutex_unlock(jl_current_task, lock);
}

static inline void jl_dyn_mutex_unlock_nogc(void *lock)
{
    _jl_dyn_mutex_unlock_nogc(lock);
}

#define JL_SPIN_MUTEX_INIT(m, name) jl_spin_mutex_init(m, name)
#define JL_SPIN_LOCK(m) jl_spin_mutex_lock(m)
#define JL_SPIN_UNLOCK(m) jl_spin_mutex_unlock(m)
#define JL_SPIN_LOCK_NOGC(m) jl_spin_mutex_lock_nogc(m)
#define JL_SPIN_UNLOCK_NOGC(m) jl_spin_mutex_unlock_nogc(m)

#define JL_SLEEP_MUTEX_INIT(m, name) jl_sleep_mutex_init(m, name)
#define JL_SLEEP_LOCK(m) jl_sleep_mutex_lock(m)
#define JL_SLEEP_UNLOCK(m) jl_sleep_mutex_unlock(m)
#define JL_SLEEP_LOCK_NOGC(m) jl_sleep_mutex_lock_nogc(m)
#define JL_SLEEP_UNLOCK_NOGC(m) jl_sleep_mutex_unlock_nogc(m)

#ifdef __cplusplus
}
#endif

#endif
