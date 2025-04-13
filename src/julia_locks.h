// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LOCKS_H
#define JL_LOCKS_H

#ifdef __cplusplus
extern "C" {
#endif

// Lock acquire and release primitives

// JL_LOCK and jl_mutex_lock are GC safe points, use uv_mutex_t if that is not desired.
// Always use JL_LOCK unless no one holding the lock can trigger a GC or GC
// safepoint. uv_mutex_t should only be needed for GC internal locks.
// The JL_LOCK* and JL_UNLOCK* macros are no-op for non-threading build
// while the jl_mutex_* functions are always locking and unlocking the locks.

JL_DLLEXPORT void _jl_mutex_init(jl_mutex_t *lock, const char *name) JL_NOTSAFEPOINT;
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

static inline void jl_mutex_lock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
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

static inline int jl_mutex_trylock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
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

static inline void jl_mutex_unlock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE
{
    _jl_mutex_unlock_nogc(lock);
}

static inline void jl_mutex_init(jl_mutex_t *lock, const char *name) JL_NOTSAFEPOINT
{
    _jl_mutex_init(lock, name);
}

#define JL_MUTEX_INIT(m, name) jl_mutex_init(m, name)
#define JL_LOCK(m) jl_mutex_lock(m)
#define JL_UNLOCK(m) jl_mutex_unlock(m)
#define JL_LOCK_NOGC(m) jl_mutex_lock_nogc(m)
#define JL_UNLOCK_NOGC(m) jl_mutex_unlock_nogc(m)

JL_DLLEXPORT void jl_lock_value(jl_mutex_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_unlock_value(jl_mutex_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_lock_field(jl_mutex_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_unlock_field(jl_mutex_t *v) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}

#include <mutex>
#include <condition_variable>
// simple C++ shim around a std::unique_lock + gc-safe + disabled finalizers region
// since we nearly always want that combination together
class jl_unique_gcsafe_lock {
public:
    int8_t gc_state;
    std::unique_lock<std::mutex> native;
    explicit jl_unique_gcsafe_lock(std::mutex &native) JL_NOTSAFEPOINT_ENTER
    {
        jl_task_t *ct = jl_current_task;
        gc_state = jl_gc_safe_enter(ct->ptls); // contains jl_gc_safepoint after enter
        this->native = std::unique_lock<std::mutex>(native);
        ct->ptls->engine_nqueued++; // disables finalizers until inference is finished on this method graph
    }
    jl_unique_gcsafe_lock(jl_unique_gcsafe_lock &&native) = delete;
    jl_unique_gcsafe_lock(jl_unique_gcsafe_lock &native) = delete;
    ~jl_unique_gcsafe_lock() JL_NOTSAFEPOINT_LEAVE {
        jl_task_t *ct = jl_current_task;
        native.unlock();
        jl_gc_safe_leave(ct->ptls, gc_state); // contains jl_gc_safepoint after leave
        ct->ptls->engine_nqueued--; // enable finalizers (but don't run them until the next gc)
    }
    void wait(std::condition_variable& cond) JL_NOTSAFEPOINT {
        cond.wait(native);
    }
};
#endif

#endif
