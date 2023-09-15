// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  task.c
  lightweight processes (symmetric coroutines)
*/

// need this to get the real definition of ucontext_t,
// if we're going to use the ucontext_t implementation there
//#if defined(__APPLE__) && defined(JL_HAVE_UCONTEXT)
//#pragma push_macro("_XOPEN_SOURCE")
//#define _XOPEN_SOURCE
//#include <ucontext.h>
//#pragma pop_macro("_XOPEN_SOURCE")
//#endif

// this is needed for !COPY_STACKS to work on linux
#ifdef _FORTIFY_SOURCE
// disable __longjmp_chk validation so that we can jump between stacks
// (which would normally be invalid to do with setjmp / longjmp)
#pragma push_macro("_FORTIFY_SOURCE")
#undef _FORTIFY_SOURCE
#include <setjmp.h>
#pragma pop_macro("_FORTIFY_SOURCE")
#endif

#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_COMPILER_ASAN_ENABLED_)
static inline void sanitizer_start_switch_fiber(jl_ptls_t ptls, jl_task_t *from, jl_task_t *to) {
    if (to->copy_stack)
        __sanitizer_start_switch_fiber(&from->ctx.asan_fake_stack, (char*)ptls->stackbase-ptls->stacksize, ptls->stacksize);
    else
        __sanitizer_start_switch_fiber(&from->ctx.asan_fake_stack, to->stkbuf, to->bufsz);
}
static inline void sanitizer_start_switch_fiber_killed(jl_ptls_t ptls, jl_task_t *to) {
    if (to->copy_stack)
        __sanitizer_start_switch_fiber(NULL, (char*)ptls->stackbase-ptls->stacksize, ptls->stacksize);
    else
        __sanitizer_start_switch_fiber(NULL, to->stkbuf, to->bufsz);
}
static inline void sanitizer_finish_switch_fiber(jl_task_t *last, jl_task_t *current) {
    __sanitizer_finish_switch_fiber(current->ctx.asan_fake_stack, NULL, NULL);
        //(const void**)&last->stkbuf,
        //&last->bufsz);
}
#else
static inline void sanitizer_start_switch_fiber(jl_ptls_t ptls, jl_task_t *from, jl_task_t *to) JL_NOTSAFEPOINT {}
static inline void sanitizer_start_switch_fiber_killed(jl_ptls_t ptls, jl_task_t *to) JL_NOTSAFEPOINT {}
static inline void sanitizer_finish_switch_fiber(jl_task_t *last, jl_task_t *current) JL_NOTSAFEPOINT {}
#endif

#if defined(_COMPILER_TSAN_ENABLED_)
// must defined as macros, since the function containing them must not return before the longjmp
#define tsan_destroy_ctx(_ptls, _ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        if (_tsan_macro_ctx != &(_ptls)->root_task->ctx) { \
            __tsan_destroy_fiber(_tsan_macro_ctx->tsan_state); \
        } \
        _tsan_macro_ctx->tsan_state = NULL; \
    } while (0)
#define tsan_switch_to_ctx(_ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        __tsan_switch_to_fiber(_tsan_macro_ctx->tsan_state, 0); \
    } while (0)
#ifdef COPY_STACKS
#define tsan_destroy_copyctx(_ptls, _ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        if (_tsan_macro_ctx != &(_ptls)->root_task->ctx) { \
            __tsan_destroy_fiber(_tsan_macro_ctx->tsan_state); \
        } \
        _tsan_macro_ctx->tsan_state = NULL; \
    } while (0)
#define tsan_switch_to_copyctx(_ctx) do { \
        struct jl_stack_context_t *_tsan_macro_ctx = (_ctx); \
        __tsan_switch_to_fiber(_tsan_macro_ctx->tsan_state, 0); \
    } while (0)
#endif
#else
// just do minimal type-checking on the arguments
#define tsan_destroy_ctx(_ptls, _ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        (void)_tsan_macro_ctx; \
    } while (0)
#define tsan_switch_to_ctx(_ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        (void)_tsan_macro_ctx; \
    } while (0)
#ifdef COPY_STACKS
#define tsan_destroy_copyctx(_ptls, _ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        (void)_tsan_macro_ctx; \
    } while (0)
#define tsan_switch_to_copyctx(_ctx) do { \
        jl_ucontext_t *_tsan_macro_ctx = (_ctx); \
        (void)_tsan_macro_ctx; \
    } while (0)
#endif
#endif

// empirically, jl_finish_task needs about 64k stack space to infer/run
// and additionally, gc-stack reserves 64k for the guard pages
#if defined(MINSIGSTKSZ)
#define MINSTKSZ (MINSIGSTKSZ > 131072 ? MINSIGSTKSZ : 131072)
#else
#define MINSTKSZ 131072
#endif

#ifdef _COMPILER_ASAN_ENABLED_
#define ROOT_TASK_STACK_ADJUSTMENT 0
#else
#define ROOT_TASK_STACK_ADJUSTMENT 3000000
#endif

#ifdef JL_HAVE_ASYNCIFY
// Switching logic is implemented in JavaScript
#define STATIC_OR_JS JL_DLLEXPORT
#else
#define STATIC_OR_JS static
#endif

static char *jl_alloc_fiber(_jl_ucontext_t *t, size_t *ssize, jl_task_t *owner) JL_NOTSAFEPOINT;
STATIC_OR_JS void jl_set_fiber(jl_ucontext_t *t);
STATIC_OR_JS void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t);
STATIC_OR_JS void jl_start_fiber_swap(jl_ucontext_t *savet, jl_ucontext_t *t);
STATIC_OR_JS void jl_start_fiber_set(jl_ucontext_t *t);

#ifdef ALWAYS_COPY_STACKS
# ifndef COPY_STACKS
# error "ALWAYS_COPY_STACKS requires COPY_STACKS"
# endif
static int always_copy_stacks = 1;
#else
static int always_copy_stacks = 0;
#endif

#if defined(_COMPILER_ASAN_ENABLED_)
extern void __asan_get_shadow_mapping(size_t *shadow_scale, size_t *shadow_offset);

JL_NO_ASAN void *memcpy_noasan(void *dest, const void *src, size_t n) {
  char *d = (char*)dest;
  const char *s = (const char *)src;
  for (size_t i = 0; i < n; ++i)
    d[i] = s[i];
  return dest;
}

JL_NO_ASAN void *memcpy_a16_noasan(uint64_t *dest, const uint64_t *src, size_t nb) {
  uint64_t *end = (uint64_t*)((char*)src + nb);
  while (src < end)
    *(dest++) = *(src++);
  return dest;
}

/* Copy stack are allocated as regular bigval objects and do no go through free_stack,
   which would otherwise unpoison it before returning to the GC pool */
static void asan_free_copy_stack(void *stkbuf, size_t bufsz) {
    __asan_unpoison_stack_memory((uintptr_t)stkbuf, bufsz);
}
#else
static void asan_free_copy_stack(void *stkbuf, size_t bufsz) {}
#endif

#ifdef COPY_STACKS
static void JL_NO_ASAN JL_NO_MSAN memcpy_stack_a16(uint64_t *to, uint64_t *from, size_t nb)
{
#if defined(_COMPILER_ASAN_ENABLED_)
    /* Asan keeps shadow memory for everything on the stack. However, in general,
       this function may touch invalid portions of the stack, since it just moves
       the stack around. To keep ASAN's stack tracking capability intact, we need
       to move the shadow memory along with the stack memory itself. */
    size_t shadow_offset;
    size_t shadow_scale;
    __asan_get_shadow_mapping(&shadow_scale, &shadow_offset);
    uintptr_t from_addr = (((uintptr_t)from) >> shadow_scale) + shadow_offset;
    uintptr_t to_addr = (((uintptr_t)to) >> shadow_scale) + shadow_offset;
    // Make sure that the shadow scale is compatible with the alignment, so
    // we can copy whole bytes.
    assert(shadow_scale <= 4);
    size_t shadow_nb = nb >> shadow_scale;
    // Copy over the shadow memory
    memcpy_noasan((char*)to_addr, (char*)from_addr, shadow_nb);
    memcpy_a16_noasan(jl_assume_aligned(to, 16), jl_assume_aligned(from, 16), nb);
#elif defined(_COMPILER_MSAN_ENABLED_)
# warning This function is imcompletely implemented for MSAN (TODO).
    memcpy((char*)jl_assume_aligned(to, 16), (char*)jl_assume_aligned(from, 16), nb);
#else
    memcpy((char*)jl_assume_aligned(to, 16), (char*)jl_assume_aligned(from, 16), nb);
    //uint64_t *end = (uint64_t*)((char*)from + nb);
    //while (from < end)
    //    *(to++) = *(from++);
#endif
}

static void NOINLINE save_stack(jl_ptls_t ptls, jl_task_t *lastt, jl_task_t **pt)
{
    char *frame_addr = (char*)((uintptr_t)jl_get_frame_addr() & ~15);
    char *stackbase = (char*)ptls->stackbase;
    assert(stackbase > frame_addr);
    size_t nb = stackbase - frame_addr;
    void *buf;
    if (lastt->bufsz < nb) {
        asan_free_copy_stack(lastt->stkbuf, lastt->bufsz);
        buf = (void*)jl_gc_alloc_buf(ptls, nb);
        lastt->stkbuf = buf;
        lastt->bufsz = nb;
    }
    else {
        buf = lastt->stkbuf;
    }
    *pt = NULL; // clear the gc-root for the target task before copying the stack for saving
    lastt->copy_stack = nb;
    lastt->sticky = 1;
    memcpy_stack_a16((uint64_t*)buf, (uint64_t*)frame_addr, nb);
    // this task's stack could have been modified after
    // it was marked by an incremental collection
    // move the barrier back instead of walking it again here
    jl_gc_wb_back(lastt);
}

JL_NO_ASAN static void NOINLINE JL_NORETURN restore_stack(jl_task_t *t, jl_ptls_t ptls, char *p)
{
    size_t nb = t->copy_stack;
    char *_x = (char*)ptls->stackbase - nb;
    if (!p) {
        // switch to a stackframe that's beyond the bounds of the last switch
        p = _x;
        if ((char*)&_x > _x) {
            p = (char*)alloca((char*)&_x - _x);
        }
        restore_stack(t, ptls, p); // pass p to ensure the compiler can't tailcall this or avoid the alloca
    }
    void *_y = t->stkbuf;
    assert(_x != NULL && _y != NULL);
    memcpy_stack_a16((uint64_t*)_x, (uint64_t*)_y, nb); // destroys all but the current stackframe

#if defined(_OS_WINDOWS_)
    jl_setcontext(&t->ctx.copy_ctx);
#else
    jl_longjmp(t->ctx.copy_ctx.uc_mcontext, 1);
#endif
    abort(); // unreachable
}

JL_NO_ASAN static void restore_stack2(jl_task_t *t, jl_ptls_t ptls, jl_task_t *lastt)
{
    assert(t->copy_stack && !lastt->copy_stack);
    size_t nb = t->copy_stack;
    char *_x = (char*)ptls->stackbase - nb;
    void *_y = t->stkbuf;
    assert(_x != NULL && _y != NULL);
    memcpy_stack_a16((uint64_t*)_x, (uint64_t*)_y, nb); // destroys all but the current stackframe
#if defined(JL_HAVE_UNW_CONTEXT)
    volatile int returns = 0;
    int r = unw_getcontext(&lastt->ctx.ctx);
    if (++returns == 2) // r is garbage after the first return
        return;
    if (r != 0 || returns != 1)
        abort();
#elif defined(JL_HAVE_ASM) || defined(JL_HAVE_SIGALTSTACK) || defined(_OS_WINDOWS_)
    if (jl_setjmp(lastt->ctx.copy_ctx.uc_mcontext, 0))
        return;
#else
#error COPY_STACKS is incompatible with this platform
#endif
    tsan_switch_to_copyctx(&t->ctx);
#if defined(_OS_WINDOWS_)
    jl_setcontext(&t->ctx.copy_ctx);
#else
    jl_longjmp(t->ctx.copy_ctx.uc_mcontext, 1);
#endif
}
#endif

/* Rooted by the base module */
static _Atomic(jl_function_t*) task_done_hook_func JL_GLOBALLY_ROOTED = NULL;

void JL_NORETURN jl_finish_task(jl_task_t *t)
{
    jl_task_t *ct = jl_current_task;
    JL_PROBE_RT_FINISH_TASK(ct);
    JL_SIGATOMIC_BEGIN();
    if (jl_atomic_load_relaxed(&t->_isexception))
        jl_atomic_store_release(&t->_state, JL_TASK_STATE_FAILED);
    else
        jl_atomic_store_release(&t->_state, JL_TASK_STATE_DONE);
    if (t->copy_stack) { // early free of stkbuf
        asan_free_copy_stack(t->stkbuf, t->bufsz);
        t->stkbuf = NULL;
    }
    // ensure that state is cleared
    ct->ptls->in_finalizer = 0;
    ct->ptls->in_pure_callback = 0;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    // let the runtime know this task is dead and find a new task to run
    jl_function_t *done = jl_atomic_load_relaxed(&task_done_hook_func);
    if (done == NULL) {
        done = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("task_done_hook"));
        if (done != NULL)
            jl_atomic_store_release(&task_done_hook_func, done);
    }
    if (done != NULL) {
        jl_value_t *args[2] = {done, (jl_value_t*)t};
        JL_TRY {
            jl_apply(args, 2);
        }
        JL_CATCH {
            jl_no_exc_handler(jl_current_exception(), ct);
        }
    }
    jl_gc_debug_critical_error();
    abort();
}

JL_DLLEXPORT void *jl_task_stack_buffer(jl_task_t *task, size_t *size, int *ptid)
{
    size_t off = 0;
#ifndef _OS_WINDOWS_
    jl_ptls_t ptls0 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    if (ptls0->root_task == task) {
        // See jl_init_root_task(). The root task of the main thread
        // has its buffer enlarged by an artificial 3000000 bytes, but
        // that means that the start of the buffer usually points to
        // inaccessible memory. We need to correct for this.
        off = ROOT_TASK_STACK_ADJUSTMENT;
    }
#endif
    jl_ptls_t ptls2 = task->ptls;
    *ptid = -1;
    if (ptls2) {
        *ptid = jl_atomic_load_relaxed(&task->tid);
#ifdef COPY_STACKS
        if (task->copy_stack) {
            *size = ptls2->stacksize;
            return (char *)ptls2->stackbase - *size;
        }
#endif
    }
    *size = task->bufsz - off;
    return (void *)((char *)task->stkbuf + off);
}

JL_DLLEXPORT void jl_active_task_stack(jl_task_t *task,
                                       char **active_start, char **active_end,
                                       char **total_start, char **total_end)
{
    if (!task->started) {
        *total_start = *active_start = 0;
        *total_end = *active_end = 0;
        return;
    }

    jl_ptls_t ptls2 = task->ptls;
    if (task->copy_stack && ptls2) {
        *total_start = *active_start = (char*)ptls2->stackbase - ptls2->stacksize;
        *total_end = *active_end = (char*)ptls2->stackbase;
    }
    else if (task->stkbuf) {
        *total_start = *active_start = (char*)task->stkbuf;
#ifndef _OS_WINDOWS_
        jl_ptls_t ptls0 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
        if (ptls0->root_task == task) {
            // See jl_init_root_task(). The root task of the main thread
            // has its buffer enlarged by an artificial 3000000 bytes, but
            // that means that the start of the buffer usually points to
            // inaccessible memory. We need to correct for this.
            *active_start += ROOT_TASK_STACK_ADJUSTMENT;
            *total_start += ROOT_TASK_STACK_ADJUSTMENT;
        }
#endif

        *total_end = *active_end = (char*)task->stkbuf + task->bufsz;
#ifdef COPY_STACKS
        // save_stack stores the stack of an inactive task in stkbuf, and the
        // actual number of used bytes in copy_stack.
        if (task->copy_stack > 1)
            *active_end = (char*)task->stkbuf + task->copy_stack;
#endif
    }
    else {
        // no stack allocated yet
        *total_start = *active_start = 0;
        *total_end = *active_end = 0;
        return;
    }

    if (task == jl_current_task) {
        // scan up to current `sp` for current thread and task
        *active_start = (char*)jl_get_frame_addr();
    }
}

// Marked noinline so we can consistently skip the associated frame.
// `skip` is number of additional frames to skip.
NOINLINE static void record_backtrace(jl_ptls_t ptls, int skip) JL_NOTSAFEPOINT
{
    // storing bt_size in ptls ensures roots in bt_data will be found
    ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE, skip + 1);
}

JL_DLLEXPORT void jl_set_next_task(jl_task_t *task) JL_NOTSAFEPOINT
{
    jl_current_task->ptls->next_task = task;
}

JL_DLLEXPORT jl_task_t *jl_get_next_task(void) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    if (ct->ptls->next_task)
        return ct->ptls->next_task;
    return ct;
}

#ifdef _COMPILER_TSAN_ENABLED_
const char tsan_state_corruption[] = "TSAN state corrupted. Exiting HARD!\n";
#endif

JL_NO_ASAN static void ctx_switch(jl_task_t *lastt)
{
    jl_ptls_t ptls = lastt->ptls;
    jl_task_t **pt = &ptls->next_task;
    jl_task_t *t = *pt;
    assert(t != lastt);
    // none of these locks should be held across a task switch
    assert(ptls->locks.len == 0);

#ifdef _COMPILER_TSAN_ENABLED_
    if (lastt->ctx.tsan_state != __tsan_get_current_fiber()) {
        // Something went really wrong - don't even assume that we can
        // use assert/abort which involve lots of signal handling that
        // looks at the tsan state.
        write(STDERR_FILENO, tsan_state_corruption, sizeof(tsan_state_corruption) - 1);
        _exit(1);
    }
#endif

    int killed = jl_atomic_load_relaxed(&lastt->_state) != JL_TASK_STATE_RUNNABLE;
    if (!t->started && !t->copy_stack) {
        // may need to allocate the stack
        if (t->stkbuf == NULL) {
            t->stkbuf = jl_alloc_fiber(&t->ctx.ctx, &t->bufsz, t);
            if (t->stkbuf == NULL) {
#ifdef COPY_STACKS
                // fall back to stack copying if mmap fails
                t->copy_stack = 1;
                t->sticky = 1;
                t->bufsz = 0;
                if (always_copy_stacks)
                    memcpy(&t->ctx.copy_ctx, &ptls->copy_stack_ctx, sizeof(t->ctx.copy_ctx));
                else
                    memcpy(&t->ctx.ctx, &ptls->base_ctx, sizeof(t->ctx.ctx));
#else
                jl_throw(jl_memory_exception);
#endif
            }
        }
    }

    if (killed) {
        *pt = NULL; // can't fail after here: clear the gc-root for the target task now
        lastt->gcstack = NULL;
        lastt->eh = NULL;
        if (!lastt->copy_stack && lastt->stkbuf) {
            // early free of stkbuf back to the pool
            jl_release_task_stack(ptls, lastt);
        }
    }
    else {
#ifdef COPY_STACKS
        if (lastt->copy_stack) { // save the old copy-stack
            save_stack(ptls, lastt, pt); // allocates (gc-safepoint, and can also fail)
            if (jl_setjmp(lastt->ctx.copy_ctx.uc_mcontext, 0)) {
                sanitizer_finish_switch_fiber(ptls->previous_task, jl_atomic_load_relaxed(&ptls->current_task));
                // TODO: mutex unlock the thread we just switched from
                return;
            }
        }
        else
#endif
        *pt = NULL; // can't fail after here: clear the gc-root for the target task now
    }

    // set up global state for new task and clear global state for old task
    t->ptls = ptls;
    jl_atomic_store_relaxed(&ptls->current_task, t);
    JL_GC_PROMISE_ROOTED(t);
    jl_signal_fence();
    jl_set_pgcstack(&t->gcstack);
    jl_signal_fence();
    lastt->ptls = NULL;
#ifdef MIGRATE_TASKS
    ptls->previous_task = lastt;
#endif

    if (t->started) {
#ifdef COPY_STACKS
        if (t->copy_stack) {
            if (lastt->copy_stack) {
                // Switching from copystack to copystack. Clear any shadow stack
                // memory above the saved shadow stack.
                uintptr_t stacktop = (uintptr_t)ptls->stackbase - t->copy_stack;
                uintptr_t stackbottom = ((uintptr_t)jl_get_frame_addr() & ~15);
                if (stackbottom < stacktop)
                    asan_unpoison_stack_memory(stackbottom, stacktop-stackbottom);
            }
            if (!killed && !lastt->copy_stack) {
                sanitizer_start_switch_fiber(ptls, lastt, t);
                restore_stack2(t, ptls, lastt);
            } else {
                tsan_switch_to_copyctx(&t->ctx);
                if (killed) {
                    sanitizer_start_switch_fiber_killed(ptls, t);
                    tsan_destroy_copyctx(ptls, &lastt->ctx);
                } else {
                    sanitizer_start_switch_fiber(ptls, lastt, t);
                }

                if (lastt->copy_stack) {
                    restore_stack(t, ptls, NULL); // (doesn't return)
                }
                else {
                    restore_stack(t, ptls, (char*)1); // (doesn't return)
                }
            }
        }
        else
#endif
        {
            if (lastt->copy_stack) {
                // Switching away from a copystack to a non-copystack. Clear
                // the whole shadow stack now, because otherwise we won't know
                // how much stack memory to clear the next time we switch to
                // a copystack.
                uintptr_t stacktop = (uintptr_t)ptls->stackbase;
                uintptr_t stackbottom = ((uintptr_t)jl_get_frame_addr() & ~15);
                // We're not restoring the stack, but we still need to unpoison the
                // stack, so it starts with a pristine stack.
                asan_unpoison_stack_memory(stackbottom, stacktop-stackbottom);
            }
            if (killed) {
                sanitizer_start_switch_fiber_killed(ptls, t);
                tsan_switch_to_ctx(&t->ctx);
                tsan_destroy_ctx(ptls, &lastt->ctx);
                jl_set_fiber(&t->ctx); // (doesn't return)
                abort(); // unreachable
            }
            else {
                sanitizer_start_switch_fiber(ptls, lastt, t);
                if (lastt->copy_stack) {
                    // Resume at the jl_setjmp earlier in this function,
                    // don't do a full task swap
                    tsan_switch_to_ctx(&t->ctx);
                    jl_set_fiber(&t->ctx); // (doesn't return)
                }
                else {
                    jl_swap_fiber(&lastt->ctx, &t->ctx);
                }
            }
        }
    }
    else {
        if (lastt->copy_stack) {
            uintptr_t stacktop = (uintptr_t)ptls->stackbase;
            uintptr_t stackbottom = ((uintptr_t)jl_get_frame_addr() & ~15);
            // We're not restoring the stack, but we still need to unpoison the
            // stack, so it starts with a pristine stack.
            asan_unpoison_stack_memory(stackbottom, stacktop-stackbottom);
        }
        if (t->copy_stack && always_copy_stacks) {
            tsan_switch_to_ctx(&t->ctx);
            if (killed) {
                sanitizer_start_switch_fiber_killed(ptls, t);
                tsan_destroy_ctx(ptls, &lastt->ctx);
            } else {
                sanitizer_start_switch_fiber(ptls, lastt, t);
            }
#ifdef COPY_STACKS
#if defined(_OS_WINDOWS_)
            jl_setcontext(&t->ctx.copy_ctx);
#else
            jl_longjmp(t->ctx.copy_ctx.uc_mcontext, 1);
#endif
#endif
            abort(); // unreachable
        }
        else {
            if (killed) {
                sanitizer_start_switch_fiber_killed(ptls, t);
                tsan_switch_to_ctx(&t->ctx);
                tsan_destroy_ctx(ptls, &lastt->ctx);
                jl_start_fiber_set(&t->ctx); // (doesn't return)
                abort();
            }
            sanitizer_start_switch_fiber(ptls, lastt, t);
            if (lastt->copy_stack) {
                // Resume at the jl_setjmp earlier in this function
                tsan_switch_to_ctx(&t->ctx);
                jl_start_fiber_set(&t->ctx); // (doesn't return)
                abort();
            }
            else {
                jl_start_fiber_swap(&lastt->ctx, &t->ctx);
            }
        }
    }
    sanitizer_finish_switch_fiber(ptls->previous_task, jl_atomic_load_relaxed(&ptls->current_task));
}

JL_DLLEXPORT void jl_switch(void) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    jl_task_t *t = ptls->next_task;
    if (t == ct) {
        return;
    }
    int8_t gc_state = jl_gc_unsafe_enter(ptls);
    if (t->started && t->stkbuf == NULL)
        jl_error("attempt to switch to exited task");
    if (ptls->in_finalizer)
        jl_error("task switch not allowed from inside gc finalizer");
    if (ptls->in_pure_callback)
        jl_error("task switch not allowed from inside staged nor pure functions");
    if (!jl_set_task_tid(t, jl_atomic_load_relaxed(&ct->tid))) // manually yielding to a task
        jl_error("cannot switch to task running on another thread");

    JL_PROBE_RT_PAUSE_TASK(ct);

    // Store old values on the stack and reset
    sig_atomic_t defer_signal = ptls->defer_signal;
    int finalizers_inhibited = ptls->finalizers_inhibited;
    ptls->finalizers_inhibited = 0;

    jl_timing_block_t *blk = jl_timing_block_task_exit(ct, ptls);
    ctx_switch(ct);

#ifdef MIGRATE_TASKS
    ptls = ct->ptls;
    t = ptls->previous_task;
    ptls->previous_task = NULL;
    assert(t != ct);
    assert(jl_atomic_load_relaxed(&t->tid) == ptls->tid);
    if (!t->sticky && !t->copy_stack)
        jl_atomic_store_release(&t->tid, -1);
#else
    assert(ptls == ct->ptls);
#endif

    // Pop old values back off the stack
    assert(ct == jl_current_task &&
           0 != ct->ptls &&
           0 == ptls->finalizers_inhibited);
    ptls->finalizers_inhibited = finalizers_inhibited;
    jl_timing_block_task_enter(ct, ptls, blk); (void)blk;

    sig_atomic_t other_defer_signal = ptls->defer_signal;
    ptls->defer_signal = defer_signal;
    if (other_defer_signal && !defer_signal)
        jl_sigint_safepoint(ptls);

    JL_PROBE_RT_RUN_TASK(ct);
    jl_gc_unsafe_leave(ptls, gc_state);
}

JL_DLLEXPORT void jl_switchto(jl_task_t **pt) JL_NOTSAFEPOINT_ENTER // n.b. this does not actually enter a safepoint
{
    jl_set_next_task(*pt);
    jl_switch();
}

JL_DLLEXPORT JL_NORETURN void jl_no_exc_handler(jl_value_t *e, jl_task_t *ct)
{
    // NULL exception objects are used when rethrowing. we don't have a handler to process
    // the exception stack, so at least report the exception at the top of the stack.
    if (!e)
        e = jl_current_exception();

    jl_printf((JL_STREAM*)STDERR_FILENO, "fatal: error thrown and no exception handler available.\n");
    jl_static_show((JL_STREAM*)STDERR_FILENO, e);
    jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
    jlbacktrace(); // written to STDERR_FILENO
    if (ct == NULL)
        jl_raise(6);
    jl_exit(1);
}

/* throw_internal - yield to exception handler */

#ifdef ENABLE_TIMINGS
#define pop_timings_stack()                                                    \
        jl_timing_block_t *cur_block = ptls->timing_stack;                     \
        while (cur_block && eh->timing_stack != cur_block) {                   \
            cur_block = jl_timing_block_pop(cur_block);                        \
        }                                                                      \
        assert(cur_block == eh->timing_stack);
#else
#define pop_timings_stack() /* Nothing */
#endif

#define throw_internal_body(altstack)                                          \
    assert(!jl_get_safe_restore());                                            \
    jl_ptls_t ptls = ct->ptls;                                                 \
    ptls->io_wait = 0;                                                         \
    jl_gc_unsafe_enter(ptls);                                                  \
    if (exception) {                                                           \
        /* The temporary ptls->bt_data is rooted by special purpose code in the\
           GC. This exists only for the purpose of preserving bt_data until we \
           set ptls->bt_size=0 below. */                                       \
        jl_push_excstack(ct, &ct->excstack, exception,                             \
                          ptls->bt_data, ptls->bt_size);                       \
        ptls->bt_size = 0;                                                     \
    }                                                                          \
    assert(ct->excstack && ct->excstack->top);                                 \
    jl_handler_t *eh = ct->eh;                                                 \
    if (eh != NULL) {                                                          \
        if (altstack) ptls->sig_exception = NULL;                              \
        pop_timings_stack()                                                    \
        asan_unpoison_task_stack(ct, &eh->eh_ctx);                             \
        jl_longjmp(eh->eh_ctx, 1);                                             \
    }                                                                          \
    else {                                                                     \
        jl_no_exc_handler(exception, ct);                                      \
    }                                                                          \
    assert(0);

static void JL_NORETURN throw_internal(jl_task_t *ct, jl_value_t *exception JL_MAYBE_UNROOTED)
{
CFI_NORETURN
    JL_GC_PUSH1(&exception);
    throw_internal_body(0);
    jl_unreachable();
}

/* On the signal stack, we don't want to create any asan frames, but we do on the
   normal, stack, so we split this function in two, depending on which context
   we're calling it in. This also lets us avoid making a GC frame on the altstack,
   which might end up getting corrupted if we recur here through another signal. */
JL_NO_ASAN static void JL_NORETURN throw_internal_altstack(jl_task_t *ct, jl_value_t *exception)
{
CFI_NORETURN
    throw_internal_body(1);
    jl_unreachable();
}

// record backtrace and raise an error
JL_DLLEXPORT void jl_throw(jl_value_t *e JL_MAYBE_UNROOTED)
{
    assert(e != NULL);
    jl_jmp_buf *safe_restore = jl_get_safe_restore();
    jl_task_t *ct = jl_get_current_task();
    if (safe_restore) {
        asan_unpoison_task_stack(ct, safe_restore);
        jl_longjmp(*safe_restore, 1);
    }
    if (ct == NULL) // During startup, or on other threads
        jl_no_exc_handler(e, ct);
    record_backtrace(ct->ptls, 1);
    throw_internal(ct, e);
}

// rethrow with current excstack state
JL_DLLEXPORT void jl_rethrow(void)
{
    jl_task_t *ct = jl_current_task;
    jl_excstack_t *excstack = ct->excstack;
    if (!excstack || excstack->top == 0)
        jl_error("rethrow() not allowed outside a catch block");
    throw_internal(ct, NULL);
}

// Special case throw for errors detected inside signal handlers.  This is not
// (cannot be) called directly in the signal handler itself, but is returned to
// after the signal handler exits.
JL_DLLEXPORT JL_NO_ASAN void JL_NORETURN jl_sig_throw(void)
{
CFI_NORETURN
    jl_jmp_buf *safe_restore = jl_get_safe_restore();
    jl_task_t *ct = jl_current_task;
    if (safe_restore) {
        asan_unpoison_task_stack(ct, safe_restore);
        jl_longjmp(*safe_restore, 1);
    }
    jl_ptls_t ptls = ct->ptls;
    jl_value_t *e = ptls->sig_exception;
    JL_GC_PROMISE_ROOTED(e);
    throw_internal_altstack(ct, e);
}

JL_DLLEXPORT void jl_rethrow_other(jl_value_t *e JL_MAYBE_UNROOTED)
{
    // TODO: Should uses of `rethrow(exc)` be replaced with a normal throw, now
    // that exception stacks allow root cause analysis?
    jl_task_t *ct = jl_current_task;
    jl_excstack_t *excstack = ct->excstack;
    if (!excstack || excstack->top == 0)
        jl_error("rethrow(exc) not allowed outside a catch block");
    // overwrite exception on top of stack. see jl_excstack_exception
    jl_excstack_raw(excstack)[excstack->top-1].jlvalue = e;
    JL_GC_PROMISE_ROOTED(e);
    throw_internal(ct, NULL);
}

/* This is xoshiro256++ 1.0, used for tasklocal random number generation in Julia.
   This implementation is intended for embedders and internal use by the runtime, and is
   based on the reference implementation at https://prng.di.unimi.it

   Credits go to David Blackman and Sebastiano Vigna for coming up with this PRNG.
   They described xoshiro256++ in "Scrambled Linear Pseudorandom Number Generators",
   ACM Trans. Math. Softw., 2021.

   There is a pure Julia implementation in stdlib that tends to be faster when used from
   within Julia, due to inlining and more aggressive architecture-specific optimizations.
*/
uint64_t jl_genrandom(uint64_t rngState[4]) JL_NOTSAFEPOINT
{
    uint64_t s0 = rngState[0];
    uint64_t s1 = rngState[1];
    uint64_t s2 = rngState[2];
    uint64_t s3 = rngState[3];

    uint64_t t = s1 << 17;
    uint64_t tmp = s0 + s3;
    uint64_t res = ((tmp << 23) | (tmp >> 41)) + s0;
    s2 ^= s0;
    s3 ^= s1;
    s1 ^= s2;
    s0 ^= s3;
    s2 ^= t;
    s3 = (s3 << 45) | (s3 >> 19);

    rngState[0] = s0;
    rngState[1] = s1;
    rngState[2] = s2;
    rngState[3] = s3;
    return res;
}

/*
The jl_rng_split function forks a task's RNG state in a way that is essentially
guaranteed to avoid collisions between the RNG streams of all tasks. The main
RNG is the xoshiro256++ RNG whose state is stored in rngState[0..3]. There is
also a small internal RNG used for task forking stored in rngState[4]. This
state is used to iterate a LCG (linear congruential generator), which is then
put through four different variations of the strongest PCG output function,
referred to as PCG-RXS-M-XS-64 [1]. This output function is invertible: it maps
a 64-bit state to 64-bit output; which is one of the reasons it's not
recommended for general purpose RNGs unless space is at a premium, but in our
usage invertibility is actually a benefit, as is explained below.

The goal of jl_rng_split is to perturb the state of each child task's RNG in
such a way each that for an entire tree of tasks spawned starting with a given
state in a root task, no two tasks have the same RNG state. Moreover, we want to
do this in a way that is deterministic and repeatable based on (1) the root
task's seed, (2) how many random numbers are generated, and (3) the task tree
structure. The RNG state of a parent task is allowed to affect the initial RNG
state of a child task, but the mere fact that a child was spawned should not
alter the RNG output of the parent. This second requirement rules out using the
main RNG to seed children -- some separate state must be maintained and changed
upon forking a child task while leaving the main RNG state unchanged.

The basic approach is that used by the DotMix [2] and SplitMix [3] RNG systems:
each task is uniquely identified by a sequence of "pedigree" numbers, indicating
where in the task tree it was spawned. This vector of pedigree coordinates is
then reduced to a single value by computing a dot product with a common vector
of random weights. The DotMix paper provides a proof that this dot product hash
value (referred to as a "compression function") is collision resistant in the
sense the the pairwise collision probability of two distinct tasks is 1/N where
N is the number of possible weight values. Both DotMix and SplitMix use a prime
value of N because the proof requires that the difference between two distinct
pedigree coordinates must be invertible, which is guaranteed by N being prime.
We take a different approach: we instead limit pedigree coordinates to being
binary instead -- when a task spawns a child, both tasks share the same pedigree
prefix, with the parent appending a zero and the child appending a one. This way
a binary pedigree vector uniquely identifies each task. Moreover, since the
coordinates are binary, the difference between coordinates is always one which
is its own inverse regardless of whether N is prime or not. This allows us to
compute the dot product modulo 2^64 using native machine arithmetic, which is
considerably more efficient and simpler to implement than arithmetic in a prime
modulus. It also means that when accumulating the dot product incrementally, as
described in SplitMix, we don't need to multiply weights by anything, we simply
add the random weight for the current task tree depth to the parent's dot
product to derive the child's dot product.

We use the LCG in rngState[4] to derive generate pseudorandom weights for the
dot product. Each time a child is forked, we update the LCG in both parent and
child tasks. In the parent, that's all we have to do -- the main RNG state
remains unchanged (recall that spawning a child should *not* affect subsequence
RNG draws in the parent). The next time the parent forks a child, the dot
product weight used will be different, corresponding to being a level deeper in
the binary task tree. In the child, we use the LCG state to generate four
pseudorandom 64-bit weights (more below) and add each weight to one of the
xoshiro256 state registers, rngState[0..3]. If we assume the main RNG remains
unused in all tasks, then each register rngState[0..3] accumulates a different
Dot/SplitMix dot product hash as additional child tasks are spawned. Each one is
collision resistant with a pairwise collision chance of only 1/2^64. Assuming
that the four pseudorandom 64-bit weight streams are sufficiently independent,
the pairwise collision probability for distinct tasks is 1/2^256. If we somehow
managed to spawn a trillion tasks, the probability of a collision would be on
the order of 1/10^54. Practically impossible. Put another way, this is the same
as the probability of two SHA256 hash values accidentally colliding, which we
generally consider so unlikely as not to be worth worrying about.

What about the random "junk" that's in the xoshiro256 state registers from
normal use of the RNG? For a tree of tasks spawned with no intervening samples
taken from the main RNG, all tasks start with the same junk which doesn't affect
the chance of collision. The Dot/SplitMix papers even suggest adding a random
base value to the dot product, so we can consider whatever happens to be in the
xoshiro256 registers to be that. What if the main RNG gets used between task
forks? In that case, the initial state registers will be different. The DotMix
collision resistance proof doesn't apply without modification, but we can
generalize the setup by adding a different base constant to each compression
function and observe that we still have a 1/N chance of the weight value
matching that exact difference. This proves collision resistance even between
tasks whose dot product hashes are computed with arbitrary offsets. We can
conclude that this scheme provides collision resistance even in the face of
different starting states of the main RNG. Does this seem too good to be true?
Perhaps another way of thinking about it will help. Suppose we seeded each task
completely randomly. Then there would also be a 1/2^256 chance of collision,
just as the DotMix proof gives. Essentially what the proof is telling us is that
if the weights are chosen uniformly and uncorrelated with the rest of the
compression function, then the dot product construction is a good enough way to
pseudorandomly seed each task. From that perspective, it's easier to believe
that adding an arbitrary constant to each seed doesn't worsen its randomness.

This leaves us with the question of how to generate four pseudorandom weights to
add to the rngState[0..3] registers at each depth of the task tree. The scheme
used here is that a single 64-bit LCG state is iterated in both parent and child
at each task fork, and four different variations of the PCG-RXS-M-XS-64 output
function are applied to that state to generate four different pseudorandom
weights. Another obvious way to generate four weights would be to iterate the
LCG four times per task split. There are two main reasons we've chosen to use
four output variants instead:

1. Advancing four times per fork reduces the set of possible weights that each
   register can be perturbed by from 2^64 to 2^60. Since collision resistance is
   proportional to the number of possible weight values, that would reduce
   collision resistance.

2. It's easier to compute four PCG output variants in parallel. Iterating the
   LCG is inherently sequential. Each PCG variant can be computed independently
   from the LCG state. All four can even be computed at once with SIMD vector
   instructions, but the compiler doesn't currently choose to do that.

A key question is whether the approach of using four variations of PCG-RXS-M-XS
is sufficiently random both within and between streams to provide the collision
resistance we expect. We obviously can't test that with 256 bits, but we have
tested it with a reduced state analogue using four PCG-RXS-M-XS-8 output
variations applied to a common 8-bit LCG. Test results do indicate sufficient
independence: a single register has collisions at 2^5 while four registers only
start having collisions at 2^20, which is actually better scaling of collision
resistance than we expect in theory. In theory, with one byte of resistance we
have a 50% chance of some collision at 20, which matches, but four bytes gives a
50% chance of collision at 2^17 and our (reduced size analogue) construction is
still collision free at 2^19. This may be due to the next observation, which guarantees collision avoidance for certain shapes of task trees as a result of using an
invertible RNG to generate weights.

In the specific case where a parent task spawns a sequence of child tasks with
no intervening usage of its main RNG, the parent and child tasks are actually
_guaranteed_ to have different RNG states. This is true because the four PCG
streams each produce every possible 2^64 bit output exactly once in the full
2^64 period of the LCG generator. This is considered a weakness of PCG-RXS-M-XS
when used as a general purpose RNG, but is quite beneficial in this application.
Since each of up to 2^64 children will be perturbed by different weights, they
cannot have hash collisions. What about parent colliding with child? That can
only happen if all four main RNG registers are perturbed by exactly zero. This
seems unlikely, but could it occur? Consider this part of each output function:

    p ^= p >> ((p >> 59) + 5);
    p *= m[i];
    p ^= p >> 43

It's easy to check that this maps zero to zero. An unchanged parent RNG can only
happen if all four `p` values are zero at the end of this, which implies that
they were all zero at the beginning. However, that is impossible since the four
`p` values differ from `x` by different additive constants, so they cannot all
be zero. Stated more generally, this non-collision property: assuming the main
RNG isn't used between task forks, sibling and parent tasks cannot have RNG
collisions. If the task tree structure is more deeply nested or if there are
intervening uses of the main RNG, we're back to relying on "merely" 256 bits of
collision resistance, but it's nice to know that in what is likely the most
common case, RNG collisions are actually impossible. This fact may also explain
better-than-theoretical collision resistance observed in our experiment with a
reduced size analogue of our hashing system.

[1]: https://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf

[2]: http://supertech.csail.mit.edu/papers/dprng.pdf

[3]: https://gee.cs.oswego.edu/dl/papers/oopsla14.pdf
*/
void jl_rng_split(uint64_t dst[JL_RNG_SIZE], uint64_t src[JL_RNG_SIZE]) JL_NOTSAFEPOINT
{
    // load and advance the internal LCG state
    uint64_t x = src[4];
    src[4] = dst[4] = x * 0xd1342543de82ef95 + 1;
    // high spectrum multiplier from https://arxiv.org/abs/2001.05304

    static const uint64_t a[4] = {
        0xe5f8fa077b92a8a8, // random additive offsets...
        0x7a0cd918958c124d,
        0x86222f7d388588d4,
        0xd30cbd35f2b64f52
    };
    static const uint64_t m[4] = {
        0xaef17502108ef2d9, // standard PCG multiplier
        0xf34026eeb86766af, // random odd multipliers...
        0x38fd70ad58dd9fbb,
        0x6677f9b93ab0c04d
    };

    // PCG-RXS-M-XS output with four variants
    for (int i = 0; i < 4; i++) {
        uint64_t p = x + a[i];
        p ^= p >> ((p >> 59) + 5);
        p *= m[i];
        p ^= p >> 43;
        dst[i] = src[i] + p; // SplitMix dot product
    }
}

JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t *start, jl_value_t *completion_future, size_t ssize)
{
    jl_task_t *ct = jl_current_task;
    jl_task_t *t = (jl_task_t*)jl_gc_alloc(ct->ptls, sizeof(jl_task_t), jl_task_type);
    jl_set_typetagof(t, jl_task_tag, 0);
    JL_PROBE_RT_NEW_TASK(ct, t);
    t->copy_stack = 0;
    if (ssize == 0) {
        // stack size unspecified; use default
        if (always_copy_stacks) {
            t->copy_stack = 1;
            t->bufsz = 0;
        }
        else {
            t->bufsz = JL_STACK_SIZE;
        }
        t->stkbuf = NULL;
    }
    else {
        // user requested dedicated stack of a certain size
        if (ssize < MINSTKSZ)
            ssize = MINSTKSZ;
        t->bufsz = ssize;
        t->stkbuf = jl_alloc_fiber(&t->ctx.ctx, &t->bufsz, t);
        if (t->stkbuf == NULL)
            jl_throw(jl_memory_exception);
    }
    t->next = jl_nothing;
    t->queue = jl_nothing;
    t->tls = jl_nothing;
    jl_atomic_store_relaxed(&t->_state, JL_TASK_STATE_RUNNABLE);
    t->start = start;
    t->result = jl_nothing;
    t->donenotify = completion_future;
    jl_atomic_store_relaxed(&t->_isexception, 0);
    // Inherit logger state from parent task
    t->logstate = ct->logstate;
    // Fork task-local random state from parent
    jl_rng_split(t->rngState, ct->rngState);
    // there is no active exception handler available on this stack yet
    t->eh = NULL;
    t->sticky = 1;
    t->gcstack = NULL;
    t->excstack = NULL;
    t->started = 0;
    t->priority = 0;
    jl_atomic_store_relaxed(&t->tid, t->copy_stack ? jl_atomic_load_relaxed(&ct->tid) : -1); // copy_stacks are always pinned since they can't be moved
    t->threadpoolid = ct->threadpoolid;
    t->ptls = NULL;
    t->world_age = ct->world_age;
    t->reentrant_timing = 0;
    jl_timing_task_init(t);

#ifdef COPY_STACKS
    if (!t->copy_stack) {
#if defined(JL_DEBUG_BUILD)
        memset(&t->ctx, 0, sizeof(t->ctx));
#endif
    }
    else {
        if (always_copy_stacks)
            memcpy(&t->ctx.copy_ctx, &ct->ptls->copy_stack_ctx, sizeof(t->ctx.copy_ctx));
        else
            memcpy(&t->ctx.ctx, &ct->ptls->base_ctx, sizeof(t->ctx.ctx));
    }
#endif
#ifdef _COMPILER_TSAN_ENABLED_
    t->ctx.tsan_state = __tsan_create_fiber(0);
#endif
#ifdef _COMPILER_ASAN_ENABLED_
    t->ctx.asan_fake_stack = NULL;
#endif
    return t;
}

// a version of jl_current_task safe for unmanaged threads
JL_DLLEXPORT jl_task_t *jl_get_current_task(void)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    return pgcstack == NULL ? NULL : container_of(pgcstack, jl_task_t, gcstack);
}


#ifdef JL_HAVE_ASYNCIFY
JL_DLLEXPORT jl_ucontext_t *task_ctx_ptr(jl_task_t *t)
{
    return &t->ctx.ctx;
}

JL_DLLEXPORT jl_value_t *jl_get_root_task(void)
{
    jl_task_t *ct = jl_current_task;
    return (jl_value_t*)ct->ptls->root_task;
}

JL_DLLEXPORT void jl_task_wait()
{
    static jl_function_t *wait_func = NULL;
    if (!wait_func) {
        wait_func = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("wait"));
    }
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_get_world_counter();
    jl_apply(&wait_func, 1);
    ct->world_age = last_age;
}

JL_DLLEXPORT void jl_schedule_task(jl_task_t *task)
{
    static jl_function_t *sched_func = NULL;
    if (!sched_func) {
        sched_func = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("schedule"));
    }
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_get_world_counter();
    jl_value_t *args[] = {(jl_value_t*)sched_func, (jl_value_t*)task};
    jl_apply(args, 2);
    ct->world_age = last_age;
}
#endif

// Do one-time initializations for task system
void jl_init_tasks(void) JL_GC_DISABLED
{
    char *acs = getenv("JULIA_COPY_STACKS");
    if (acs) {
        if (!strcmp(acs, "1") || !strcmp(acs, "yes"))
            always_copy_stacks = 1;
        else if (!strcmp(acs, "0") || !strcmp(acs, "no"))
            always_copy_stacks = 0;
        else {
            jl_safe_printf("invalid JULIA_COPY_STACKS value: %s\n", acs);
            exit(1);
        }
    }
#ifndef COPY_STACKS
    if (always_copy_stacks) {
        jl_safe_printf("Julia built without COPY_STACKS support");
        exit(1);
    }
#endif
}

#if defined(_COMPILER_ASAN_ENABLED_)
STATIC_OR_JS void NOINLINE JL_NORETURN _start_task(void);
#endif

STATIC_OR_JS void NOINLINE JL_NORETURN JL_NO_ASAN start_task(void)
{
CFI_NORETURN
#if defined(_COMPILER_ASAN_ENABLED_)
    // First complete the fiber switch, otherwise ASAN will be confused
    // when it unpoisons the stack in _start_task
#ifdef __clang_gcanalyzer__
    jl_task_t *ct = jl_get_current_task();
#else
    jl_task_t *ct = jl_current_task;
#endif
    jl_ptls_t ptls = ct->ptls;
    sanitizer_finish_switch_fiber(ptls->previous_task, ct);
    _start_task();
}

STATIC_OR_JS void NOINLINE JL_NORETURN _start_task(void)
{
CFI_NORETURN
#endif
    // this runs the first time we switch to a task
#ifdef __clang_gcanalyzer__
    jl_task_t *ct = jl_get_current_task();
#else
    jl_task_t *ct = jl_current_task;
#endif
    jl_ptls_t ptls = ct->ptls;
    jl_value_t *res;
    assert(ptls->finalizers_inhibited == 0);

#ifdef MIGRATE_TASKS
    jl_task_t *pt = ptls->previous_task;
    ptls->previous_task = NULL;
    if (!pt->sticky && !pt->copy_stack)
        jl_atomic_store_release(&pt->tid, -1);
#endif

    ct->started = 1;
    JL_PROBE_RT_START_TASK(ct);
    jl_timing_block_task_enter(ct, ptls, NULL);
    if (jl_atomic_load_relaxed(&ct->_isexception)) {
        record_backtrace(ptls, 0);
        jl_push_excstack(ct, &ct->excstack, ct->result,
                         ptls->bt_data, ptls->bt_size);
        res = ct->result;
    }
    else {
        JL_TRY {
            if (ptls->defer_signal) {
                ptls->defer_signal = 0;
                jl_sigint_safepoint(ptls);
            }
            JL_TIMING(ROOT, ROOT);
            res = jl_apply(&ct->start, 1);
        }
        JL_CATCH {
            res = jl_current_exception();
            jl_atomic_store_relaxed(&ct->_isexception, 1);
            goto skip_pop_exception;
        }
skip_pop_exception:;
    }
    ct->result = res;
    jl_gc_wb(ct, ct->result);
    jl_finish_task(ct);
    jl_gc_debug_critical_error();
    abort();
}


#if defined(JL_HAVE_UCONTEXT)
#ifdef _OS_WINDOWS_
#define setcontext jl_setcontext
#define swapcontext jl_swapcontext
#define makecontext jl_makecontext
#endif
static char *jl_alloc_fiber(_jl_ucontext_t *t, size_t *ssize, jl_task_t *owner) JL_NOTSAFEPOINT
{
#ifndef _OS_WINDOWS_
    int r = getcontext(t);
    if (r != 0)
        jl_error("getcontext failed");
#endif
    void *stk = jl_malloc_stack(ssize, owner);
    if (stk == NULL)
        return NULL;
    t->uc_stack.ss_sp = stk;
    t->uc_stack.ss_size = *ssize;
#ifdef _OS_WINDOWS_
    makecontext(t, &start_task);
#else
    t->uc_link = NULL;
    makecontext(t, &start_task, 0);
#endif
    return (char*)stk;
}
static void jl_start_fiber_set(jl_ucontext_t *t)
{
    setcontext(&t->ctx);
}
static void jl_start_fiber_swap(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    assert(lastt);
    tsan_switch_to_ctx(t);
    swapcontext(&lastt->ctx, &t->ctx);
}
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    tsan_switch_to_ctx(t);
    swapcontext(&lastt->ctx, &t->ctx);
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    setcontext(&t->ctx);
}
#endif

#if defined(JL_HAVE_UNW_CONTEXT) || defined(JL_HAVE_ASM)
static char *jl_alloc_fiber(_jl_ucontext_t *t, size_t *ssize, jl_task_t *owner)
{
    char *stkbuf = (char*)jl_malloc_stack(ssize, owner);
    if (stkbuf == NULL)
        return NULL;
#ifndef __clang_gcanalyzer__
    ((char**)t)[0] = stkbuf; // stash the stack pointer somewhere for start_fiber
    ((size_t*)t)[1] = *ssize; // stash the stack size somewhere for start_fiber
#endif
    return stkbuf;
}
#endif

#if defined(JL_HAVE_UNW_CONTEXT)
static inline void jl_unw_swapcontext(unw_context_t *old, unw_cursor_t *c)
{
    volatile int returns = 0;
    int r = unw_getcontext(old);
    if (++returns == 2) // r is garbage after the first return
        return;
    if (r != 0 || returns != 1)
        abort();
    unw_resume(c);
}
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    unw_cursor_t c;
    int r = unw_init_local(&c, &t->ctx);
    if (r < 0)
        abort();
    jl_unw_swapcontext(&lastt->ctx, &c);
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    unw_cursor_t c;
    int r = unw_init_local(&c, &t->ctx);
    if (r < 0)
        abort();
    unw_resume(&c);
}
#elif defined(JL_HAVE_ASM)
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (jl_setjmp(lastt->ctx.uc_mcontext, 0))
        return;
    tsan_switch_to_ctx(t);
    jl_set_fiber(t); // doesn't return
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    jl_longjmp(t->ctx.uc_mcontext, 1);
}
#endif

#if defined(JL_HAVE_UNW_CONTEXT) && !defined(JL_HAVE_ASM)
#if defined(_CPU_X86_) || defined(_CPU_X86_64_)
#define PUSH_RET(ctx, stk) \
    do { \
        stk -= sizeof(uintptr_t); \
        *(uintptr_t*)stk = 0; /* push null RIP/EIP onto the stack */ \
    } while (0)
#elif defined(_CPU_ARM_)
#define PUSH_RET(ctx, stk) \
    if (unw_set_reg(ctx, UNW_ARM_R14, 0)) /* put NULL into the LR */ \
        abort();
#else
#error please define how to simulate a CALL on this platform
#endif
static void jl_start_fiber_set(jl_ucontext_t *t)
{
    unw_cursor_t c;
    char *stk = ((char**)&t->ctx)[0];
    size_t ssize = ((size_t*)&t->ctx)[1];
    uintptr_t fn = (uintptr_t)&start_task;
    stk += ssize;
    int r = unw_getcontext(&t->ctx);
    if (r)
        abort();
    if (unw_init_local(&c, &t->ctx))
        abort();
    PUSH_RET(&c, stk);
#if defined __linux__
#error savannah nongnu libunwind is incapable of setting UNW_REG_SP, as required
#endif
    if (unw_set_reg(&c, UNW_REG_SP, (uintptr_t)stk))
        abort();
    if (unw_set_reg(&c, UNW_REG_IP, fn))
        abort();
    unw_resume(&c); // (doesn't return)
}
static void jl_start_fiber_swap(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    assert(lastt);
    unw_cursor_t c;
    char *stk = ((char**)&t->ctx)[0];
    size_t ssize = ((size_t*)&t->ctx)[1];
    uintptr_t fn = (uintptr_t)&start_task;
    stk += ssize;
    volatile int returns = 0;
    int r = unw_getcontext(&lastt->ctx);
    if (++returns == 2) // r is garbage after the first return
        return;
    if (r != 0 || returns != 1)
        abort();
    r = unw_getcontext(&t->ctx);
    if (r != 0)
        abort();
    if (unw_init_local(&c, &t->ctx))
        abort();
    PUSH_RET(&c, stk);
    if (unw_set_reg(&c, UNW_REG_SP, (uintptr_t)stk))
        abort();
    if (unw_set_reg(&c, UNW_REG_IP, fn))
        abort();
    jl_unw_swapcontext(&lastt->ctx, &c);
}
#endif

#if defined(JL_HAVE_ASM)
JL_NO_ASAN static void jl_start_fiber_swap(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    assert(lastt);
#ifdef JL_HAVE_UNW_CONTEXT
    volatile int returns = 0;
    int r = unw_getcontext(&lastt->ctx);
    if (++returns == 2) // r is garbage after the first return
        return;
    if (r != 0 || returns != 1)
        abort();
#else
    if (jl_setjmp(lastt->ctx.uc_mcontext, 0))
        return;
#endif
    tsan_switch_to_ctx(t);
    jl_start_fiber_set(t); // doesn't return
}
JL_NO_ASAN static void jl_start_fiber_set(jl_ucontext_t *t)
{
    char *stk = ((char**)&t->ctx)[0];
    size_t ssize = ((size_t*)&t->ctx)[1];
    uintptr_t fn = (uintptr_t)&start_task;
    stk += ssize;
#ifdef _CPU_X86_64_
    asm volatile (
        " movq %0, %%rsp;\n"
        " movq %1, %%rax;\n"
        " xorq %%rbp, %%rbp;\n"
        " push %%rbp;\n" // instead of RSP
        " jmpq *%%rax;\n" // call `fn` with fake stack frame
        " ud2"
        : : "r"(stk), "r"(fn) : "memory" );
#elif defined(_CPU_X86_)
    asm volatile (
        " movl %0, %%esp;\n"
        " movl %1, %%eax;\n"
        " xorl %%ebp, %%ebp;\n"
        " push %%ebp;\n" // instead of ESP
        " jmpl *%%eax;\n" // call `fn` with fake stack frame
        " ud2"
        : : "r"(stk), "r"(fn) : "memory" );
#elif defined(_CPU_AARCH64_)
    asm volatile(
        " mov sp, %0;\n"
        " mov x29, xzr;\n" // Clear link register (x29) and frame pointer
        " mov x30, xzr;\n" // (x30) to terminate unwinder.
        " br %1;\n" // call `fn` with fake stack frame
        " brk #0x1" // abort
        : : "r" (stk), "r"(fn) : "memory" );
#elif defined(_CPU_ARM_)
    // A "i" constraint on `&start_task` works only on clang and not on GCC.
    asm(" mov sp, %0;\n"
        " mov lr, #0;\n" // Clear link register (lr) and frame pointer
        " mov fp, #0;\n" // (fp) to terminate unwinder.
        " bx %1;\n" // call `fn` with fake stack frame.  While `bx` can change
                    // the processor mode to thumb, this will never happen
                    // because all our addresses are word-aligned.
        " udf #0" // abort
        : : "r" (stk), "r"(fn) : "memory" );
#elif defined(_CPU_PPC64_)
    // N.B.: There is two iterations of the PPC64 ABI.
    // v2 is current and used here. Make sure you have the
    // correct version of the ABI reference when working on this code.
    asm volatile(
        // Move stack (-0x30 for initial stack frame) to stack pointer
        " addi 1, %0, -0x30;\n"
        // Build stack frame
        // Skip local variable save area
        " std 2, 0x28(1);\n" // Save TOC
        // Clear link editor/compiler words
        " std 0, 0x20(1);\n"
        " std 0, 0x18(1);\n"
        // Clear LR/CR save area
        " std 0, 0x10(1);\n"
        " std 0, 0x8(1);\n"
        " std 0, 0x0(1); \n" // Clear back link to terminate unwinder
        " mtlr 0; \n"        // Clear link register
        " mr 12, %1; \n"     // Set up target global entry point
        " mtctr 12; \n"      // Move jump target to counter register
        " bctr; \n"          // branch to counter (lr update disabled)
        " trap; \n"
        : : "r"(stk), "r"(fn) : "memory");
#else
#error JL_HAVE_ASM defined but not implemented for this CPU type
#endif
    __builtin_unreachable();
}
#endif

#if defined(JL_HAVE_SIGALTSTACK)
#if defined(_COMPILER_TSAN_ENABLED_)
#error TSAN support not currently implemented for this tasking model
#endif

static void start_basefiber(int sig)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    if (jl_setjmp(ptls->base_ctx.uc_mcontext, 0))
        start_task(); // sanitizer_finish_switch_fiber is part of start_task
}
static char *jl_alloc_fiber(_jl_ucontext_t *t, size_t *ssize, jl_task_t *owner)
{
    stack_t uc_stack, osigstk;
    struct sigaction sa, osa;
    sigset_t set, oset;
    void *stk = jl_malloc_stack(ssize, owner);
    if (stk == NULL)
        return NULL;
    // setup
    jl_ptls_t ptls = jl_current_task->ptls;
    _jl_ucontext_t base_ctx;
    memcpy(&base_ctx, &ptls->base_ctx, sizeof(base_ctx));
    sigfillset(&set);
    if (pthread_sigmask(SIG_BLOCK, &set, &oset) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("pthread_sigmask failed");
    }
    uc_stack.ss_sp = stk;
    uc_stack.ss_size = *ssize;
    uc_stack.ss_flags = 0;
    if (sigaltstack(&uc_stack, &osigstk) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaltstack failed");
    }
    memset(&sa, 0, sizeof(sa));
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = start_basefiber;
    sa.sa_flags = SA_ONSTACK;
    if (sigaction(SIGUSR2, &sa, &osa) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaction failed");
    }
    // emit signal
    pthread_kill(pthread_self(), SIGUSR2); // initializes jl_basectx
    sigdelset(&set, SIGUSR2);
    sigsuspend(&set);
    // cleanup
    if (sigaction(SIGUSR2, &osa, NULL) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaction failed");
    }
    if (osigstk.ss_size < MINSTKSZ && (osigstk.ss_flags | SS_DISABLE))
       osigstk.ss_size = MINSTKSZ;
    if (sigaltstack(&osigstk, NULL) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("sigaltstack failed");
    }
    if (pthread_sigmask(SIG_SETMASK, &oset, NULL) != 0) {
       jl_free_stack(stk, *ssize);
       jl_error("pthread_sigmask failed");
    }
    if (&ptls->base_ctx != t) {
        memcpy(&t, &ptls->base_ctx, sizeof(base_ctx));
        memcpy(&ptls->base_ctx, &base_ctx, sizeof(base_ctx)); // restore COPY_STACKS context
    }
    return (char*)stk;
}
static void jl_start_fiber_set(jl_ucontext_t *t) {
    jl_longjmp(t->ctx.uc_mcontext, 1); // (doesn't return)
}
static void jl_start_fiber_swap(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    assert(lastt);
    if (lastt && jl_setjmp(lastt->ctx.uc_mcontext, 0))
        return;
    tsan_switch_to_ctx(t);
    jl_start_fiber_set(t);
}
static void jl_swap_fiber(jl_ucontext_t *lastt, jl_ucontext_t *t)
{
    if (jl_setjmp(lastt->ctx.uc_mcontext, 0))
        return;
    tsan_switch_to_ctx(t);
    jl_start_fiber_set(t); // doesn't return
}
static void jl_set_fiber(jl_ucontext_t *t)
{
    jl_longjmp(t->ctx.uc_mcontext, 1);
}
#endif

#if defined(JL_HAVE_ASYNCIFY)
#if defined(_COMPILER_TSAN_ENABLED_)
#error TSAN support not currently implemented for this tasking model
#endif

static char *jl_alloc_fiber(_jl_ucontext_t *t, size_t *ssize, jl_task_t *owner) JL_NOTSAFEPOINT
{
    void *stk = jl_malloc_stack(ssize, owner);
    if (stk == NULL)
        return NULL;
    t->stackbottom = stk;
    t->stacktop = ((char*)stk) + *ssize;
    return (char*)stk;
}
// jl_*_fiber implemented in js
#endif

// Initialize a root task using the given stack.
jl_task_t *jl_init_root_task(jl_ptls_t ptls, void *stack_lo, void *stack_hi)
{
    assert(ptls->root_task == NULL);
    // We need `gcstack` in `Task` to allocate Julia objects; *including* the `Task` type.
    // However, to allocate a `Task` via `jl_gc_alloc` as done in `jl_init_root_task`,
    // we need the `Task` type itself. We use stack-allocated "raw" `jl_task_t` struct to
    // workaround this chicken-and-egg problem. Note that this relies on GC to be turned
    // off as GC fails because we don't/can't allocate the type tag.
    struct {
        jl_value_t *type;
        jl_task_t value;
    } bootstrap_task = {0};
    jl_set_pgcstack(&bootstrap_task.value.gcstack);
    bootstrap_task.value.ptls = ptls;
    if (jl_nothing == NULL) // make a placeholder
        jl_nothing = jl_gc_permobj(0, jl_nothing_type);
    jl_task_t *ct = (jl_task_t*)jl_gc_alloc(ptls, sizeof(jl_task_t), jl_task_type);
    jl_set_typetagof(ct, jl_task_tag, 0);
    memset(ct, 0, sizeof(jl_task_t));
    void *stack = stack_lo;
    size_t ssize = (char*)stack_hi - (char*)stack_lo;
#ifndef _OS_WINDOWS_
    if (ptls->tid == 0) {
        stack = (void*)((char*)stack - ROOT_TASK_STACK_ADJUSTMENT); // offset our guess of the address of the bottom of stack to cover the guard pages too
        ssize += ROOT_TASK_STACK_ADJUSTMENT; // sizeof stack is known exactly, but not where we are in that stack
    }
#endif
    if (always_copy_stacks) {
        ct->copy_stack = 1;
        ct->stkbuf = NULL;
        ct->bufsz = 0;
    }
    else {
        ct->copy_stack = 0;
        ct->stkbuf = stack;
        ct->bufsz = ssize;
    }

#ifdef USE_TRACY
    char *unique_string = (char *)malloc(strlen("Root") + 1);
    strcpy(unique_string, "Root");
    ct->name = unique_string;
#endif
    ct->started = 1;
    ct->next = jl_nothing;
    ct->queue = jl_nothing;
    ct->tls = jl_nothing;
    jl_atomic_store_relaxed(&ct->_state, JL_TASK_STATE_RUNNABLE);
    ct->start = NULL;
    ct->result = jl_nothing;
    ct->donenotify = jl_nothing;
    jl_atomic_store_relaxed(&ct->_isexception, 0);
    ct->logstate = jl_nothing;
    ct->eh = NULL;
    ct->gcstack = NULL;
    ct->excstack = NULL;
    jl_atomic_store_relaxed(&ct->tid, ptls->tid);
    ct->threadpoolid = jl_threadpoolid(ptls->tid);
    ct->sticky = 1;
    ct->ptls = ptls;
    ct->world_age = 1; // OK to run Julia code on this task
    ct->reentrant_timing = 0;
    ptls->root_task = ct;
    jl_atomic_store_relaxed(&ptls->current_task, ct);
    JL_GC_PROMISE_ROOTED(ct);
    jl_set_pgcstack(&ct->gcstack);
    assert(jl_current_task == ct);

#ifdef _COMPILER_TSAN_ENABLED_
    ct->ctx.tsan_state = __tsan_get_current_fiber();
#endif
#ifdef _COMPILER_ASAN_ENABLED_
    ct->ctx.asan_fake_stack = NULL;
#endif

    jl_timing_block_task_enter(ct, ptls, NULL);

#ifdef COPY_STACKS
    // initialize the base_ctx from which all future copy_stacks will be copies
    if (always_copy_stacks) {
        // when this is set, we will attempt to corrupt the process stack to switch tasks,
        // although this is unreliable, and thus not recommended
        ptls->stackbase = stack_hi;
        ptls->stacksize = ssize;
#ifdef _OS_WINDOWS_
        ptls->copy_stack_ctx.uc_stack.ss_sp = stack_hi;
        ptls->copy_stack_ctx.uc_stack.ss_size = ssize;
#endif
        if (jl_setjmp(ptls->copy_stack_ctx.uc_mcontext, 0))
            start_task(); // sanitizer_finish_switch_fiber is part of start_task
    }
    else {
        ssize = JL_STACK_SIZE;
        char *stkbuf = jl_alloc_fiber(&ptls->base_ctx, &ssize, NULL);
        if (stkbuf != NULL) {
            ptls->stackbase = stkbuf + ssize;
            ptls->stacksize = ssize;
        }
    }
#endif

    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_thread_signal_handler(ptls);

    return ct;
}

JL_DLLEXPORT int jl_is_task_started(jl_task_t *t) JL_NOTSAFEPOINT
{
    return t->started;
}

JL_DLLEXPORT int16_t jl_get_task_tid(jl_task_t *t) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&t->tid);
}

JL_DLLEXPORT int8_t jl_get_task_threadpoolid(jl_task_t *t)
{
    return t->threadpoolid;
}


#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_)
extern DWORD32 __readgsdword(int);
extern DWORD32 __readgs(void);
#endif
JL_DLLEXPORT void jl_gdb_dump_threadinfo(void)
{
#if defined(_CPU_X86_64_)
    DWORD64 gs0 = __readgsqword(0x0);
    DWORD64 gs8 = __readgsqword(0x8);
    DWORD64 gs16 = __readgsqword(0x10);
    jl_safe_printf("ThreadId: %u, Stack: %p -- %p to %p, SEH: %p\n",
                   (unsigned)GetCurrentThreadId(),
                   jl_get_frame_addr(),
                   (void*)gs8, (void*)gs16, (void*)gs0);
#elif defined(_CPU_X86_)
    DWORD32 fs0 = __readfsdword(0x0);
    DWORD32 fs4 = __readfsdword(0x4);
    DWORD32 fs8 = __readfsdword(0x8);
    jl_safe_printf("ThreadId: %u, Stack: %p -- %p to %p, SEH: %p\n",
                   (unsigned)GetCurrentThreadId(),
                   jl_get_frame_addr(),
                   (void*)fs4, (void*)fs8, (void*)fs0);
    if (__readgs()) { // WoW64 if GS is non-zero
        DWORD32 gs0 = __readgsdword(0x0);
        DWORD32 gs4 = __readgsdword(0x4);
        DWORD32 gs8 = __readgsdword(0x8);
        DWORD32 gs12 = __readgsdword(0xc);
        DWORD32 gs16 = __readgsdword(0x10);
        DWORD32 gs20 = __readgsdword(0x14);
        jl_safe_printf("Stack64: %p%p to %p%p, SEH64: %p%p\n",
                       (void*)gs12, (void*)gs8,
                       (void*)gs20, (void*)gs16,
                       (void*)gs4, (void*)gs0);
    }
#else
    jl_safe_printf("ThreadId: %u, Stack: %p\n",
                   (unsigned)GetCurrentThreadId(),
                   jl_get_frame_addr());
#endif
}
#endif


#ifdef __cplusplus
}
#endif
