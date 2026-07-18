// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LOCKS_H
#define JL_LOCKS_H

#ifdef _COMPILER_TSAN_ENABLED_
#include <sanitizer/tsan_interface.h>
#endif

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
JL_DLLEXPORT void _jl_mutex_wait(jl_task_t *self, jl_mutex_t *lock, int safepoint) JL_CANSAFEPOINT;
JL_DLLEXPORT void _jl_mutex_lock(jl_task_t *self, jl_mutex_t *lock) JL_CANSAFEPOINT;
JL_DLLEXPORT int _jl_mutex_trylock_nogc(jl_task_t *self, jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER_CONDITIONAL(1);
JL_DLLEXPORT int _jl_mutex_trylock(jl_task_t *self, jl_mutex_t *lock) JL_NOTSAFEPOINT;
JL_DLLEXPORT void _jl_mutex_unlock(jl_task_t *self, jl_mutex_t *lock) JL_CANSAFEPOINT;
JL_DLLEXPORT void _jl_mutex_unlock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;

static inline void jl_mutex_wait(jl_mutex_t *lock, int safepoint) JL_CANSAFEPOINT
{
    _jl_mutex_wait(jl_current_task, lock, safepoint);
}

static inline void jl_mutex_lock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER JL_NO_SAFEPOINT_ANALYSIS
{
#ifndef __clang_gcanalyzer__
    // Hide this body from the analyzer, otherwise it complains that we're calling
    // a non-safepoint from this function. The 0 arguments guarantees that we do
    // not reach the safepoint, but the analyzer can't figure that out
#ifdef _COMPILER_TSAN_ENABLED_
    __tsan_mutex_pre_lock(lock, __tsan_mutex_write_reentrant);
#endif
    jl_mutex_wait(lock, 0);
#ifdef _COMPILER_TSAN_ENABLED_
    __tsan_mutex_post_lock(lock, __tsan_mutex_write_reentrant, 1);
#endif
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

static inline void jl_mutex_lock(jl_mutex_t *lock) JL_CANSAFEPOINT
{
    _jl_mutex_lock(jl_current_task, lock);
}

static inline int jl_mutex_trylock_nogc(jl_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER_CONDITIONAL(1)
{
    return _jl_mutex_trylock_nogc(jl_current_task, lock);
}

static inline int jl_mutex_trylock(jl_mutex_t *lock) JL_NOTSAFEPOINT
{
    return _jl_mutex_trylock(jl_current_task, lock);
}

static inline void jl_mutex_unlock(jl_mutex_t *lock) JL_CANSAFEPOINT
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

JL_DLLEXPORT void jl_lock_value(jl_mutex_t *v) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
JL_DLLEXPORT void jl_unlock_value(jl_mutex_t *v) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;
JL_DLLEXPORT void jl_lock_field(jl_mutex_t *v) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
JL_DLLEXPORT void jl_unlock_field(jl_mutex_t *v) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;

// Redeclare platform locks with NOTSAFEPOINT enter/leave annotations
// n.b. we should add mutex_lock_safe aliases, which assert !jl_gcunsaferegion instead of enter/leave
#ifdef JL_LIBRARY_EXPORTS
UV_EXTERN void uv_mutex_lock(uv_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
UV_EXTERN void uv_mutex_unlock(uv_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;
#ifndef _OS_WINDOWS_
int pthread_mutex_lock(pthread_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
int pthread_mutex_trylock(pthread_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER_CONDITIONAL(0);
int pthread_mutex_unlock(pthread_mutex_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;
int pthread_rwlock_rdlock(pthread_rwlock_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
int pthread_rwlock_tryrdlock(pthread_rwlock_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER_CONDITIONAL(0);
int pthread_rwlock_wrlock(pthread_rwlock_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
int pthread_rwlock_trywrlock(pthread_rwlock_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER_CONDITIONAL(0);
int pthread_rwlock_unlock(pthread_rwlock_t *lock) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;
#endif
#endif

#ifdef __cplusplus
}

#include <mutex>
#include <shared_mutex>
#include <condition_variable>

// n.b. we should add mutex_lock_safe aliases, which assert !jl_gcunsaferegion instead of enter/leave
#ifdef __clang_safetyanalysis__
#define JL_TSA_ANNOTATE_SCOPED_LOCK(Template, Name, Mutex) \
    template JL_NOTSAFEPOINT_ENTER Template<Mutex>::Name(Mutex&); \
    template JL_NOTSAFEPOINT_LEAVE Template<Mutex>::~Name()
#define JL_TSA_ANNOTATE_LOCK(Template, Name, Mutex) \
    JL_TSA_ANNOTATE_SCOPED_LOCK(Template, Name, Mutex); \
    template JL_NOTSAFEPOINT_ENTER void Template<Mutex>::lock(); \
    template JL_NOTSAFEPOINT_LEAVE void Template<Mutex>::unlock()

// The qualified destructor name (e.g. std::lock_guard<std::mutex>::~lock_guard)
// draws a pedantic -Wdtor-name because the name after `::~` is not visible at
// this scope; that is expected for these explicit instantiations, so silence it.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdtor-name"
JL_TSA_ANNOTATE_SCOPED_LOCK(std::lock_guard, lock_guard, std::mutex);
JL_TSA_ANNOTATE_LOCK(std::unique_lock, unique_lock, std::mutex);
JL_TSA_ANNOTATE_LOCK(std::unique_lock, unique_lock, std::shared_mutex);
JL_TSA_ANNOTATE_LOCK(std::shared_lock, shared_lock, std::shared_mutex);
#pragma clang diagnostic pop

#undef JL_TSA_ANNOTATE_SCOPED_LOCK
#undef JL_TSA_ANNOTATE_LOCK
#endif

// simple C++ shim around a std::unique_lock + gc-safe + disabled finalizers region
// since we nearly always want that combination together
class jl_unique_gcsafe_lock {
public:
    int8_t gc_state;
    std::unique_lock<std::mutex> native;
    explicit jl_unique_gcsafe_lock(std::mutex &native) JL_CANSAFEPOINT_LEAVE
    {
        jl_task_t *ct = jl_current_task;
        gc_state = jl_gc_safe_enter(ct->ptls); // contains jl_gc_safepoint after enter
        this->native = std::unique_lock<std::mutex>(native);
        ct->ptls->engine_nqueued++; // disables finalizers until inference is finished on this method graph
    }
    jl_unique_gcsafe_lock(jl_unique_gcsafe_lock &&native) = delete;
    jl_unique_gcsafe_lock(jl_unique_gcsafe_lock &native) = delete;
    ~jl_unique_gcsafe_lock() JL_CANSAFEPOINT_ENTER
    {
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
