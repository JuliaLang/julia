// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_COMMON_H
#define JL_GC_COMMON_H

#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

// =========================================================================== //
// GC Callbacks
// =========================================================================== //

typedef void (*jl_gc_cb_func_t)(void);

typedef struct _jl_gc_callback_list_t {
    struct _jl_gc_callback_list_t *next;
    jl_gc_cb_func_t func;
} jl_gc_callback_list_t;

extern jl_gc_callback_list_t *gc_cblist_root_scanner;
extern jl_gc_callback_list_t *gc_cblist_task_scanner;
extern jl_gc_callback_list_t *gc_cblist_pre_gc;
extern jl_gc_callback_list_t *gc_cblist_post_gc;
extern jl_gc_callback_list_t *gc_cblist_notify_external_alloc;
extern jl_gc_callback_list_t *gc_cblist_notify_external_free;
extern jl_gc_callback_list_t *gc_cblist_notify_gc_pressure;

#define gc_invoke_callbacks(ty, list, args) \
    do { \
        for (jl_gc_callback_list_t *cb = list; \
                cb != NULL; \
                cb = cb->next) \
        { \
            ((ty)(cb->func)) args; \
        } \
    } while (0)

#ifdef __cplusplus
}
#endif

// =========================================================================== //
// malloc wrappers, aligned allocation
// =========================================================================== //

#if defined(_OS_WINDOWS_)
STATIC_INLINE void *jl_malloc_aligned(size_t sz, size_t align)
{
    return _aligned_malloc(sz ? sz : 1, align);
}
STATIC_INLINE void *jl_realloc_aligned(void *p, size_t sz, size_t oldsz,
                                       size_t align)
{
    (void)oldsz;
    return _aligned_realloc(p, sz ? sz : 1, align);
}
STATIC_INLINE void jl_free_aligned(void *p) JL_NOTSAFEPOINT
{
    _aligned_free(p);
}
#else
STATIC_INLINE void *jl_malloc_aligned(size_t sz, size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return malloc(sz);
#endif
    void *ptr;
    if (posix_memalign(&ptr, align, sz))
        return NULL;
    return ptr;
}
STATIC_INLINE void *jl_realloc_aligned(void *d, size_t sz, size_t oldsz,
                                       size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return realloc(d, sz);
#endif
    void *b = jl_malloc_aligned(sz, align);
    if (b != NULL) {
        memcpy(b, d, oldsz > sz ? sz : oldsz);
        free(d);
    }
    return b;
}
STATIC_INLINE void jl_free_aligned(void *p) JL_NOTSAFEPOINT
{
    free(p);
}
#endif
#define malloc_cache_align(sz) jl_malloc_aligned(sz, JL_CACHE_BYTE_ALIGNMENT)
#define realloc_cache_align(p, sz, oldsz) jl_realloc_aligned(p, sz, oldsz, JL_CACHE_BYTE_ALIGNMENT)

// =========================================================================== //
// Pointer tagging
// =========================================================================== //

STATIC_INLINE int gc_marked(uintptr_t bits) JL_NOTSAFEPOINT
{
    return (bits & GC_MARKED) != 0;
}

STATIC_INLINE int gc_old(uintptr_t bits) JL_NOTSAFEPOINT
{
    return (bits & GC_OLD) != 0;
}

STATIC_INLINE uintptr_t gc_set_bits(uintptr_t tag, int bits) JL_NOTSAFEPOINT
{
    return (tag & ~(uintptr_t)3) | bits;
}

STATIC_INLINE uintptr_t gc_ptr_tag(void *v, uintptr_t mask) JL_NOTSAFEPOINT
{
    return ((uintptr_t)v) & mask;
}

STATIC_INLINE void *gc_ptr_clear_tag(void *v, uintptr_t mask) JL_NOTSAFEPOINT
{
    return (void*)(((uintptr_t)v) & ~mask);
}

// =========================================================================== //
// GC Metrics
// =========================================================================== //

extern jl_gc_num_t gc_num;

// =========================================================================== //
// Stop-the-world for GC
// =========================================================================== //
void jl_gc_wait_for_the_world(jl_ptls_t* gc_all_tls_states, int gc_n_threads);

// =========================================================================== //
// Finalization
// =========================================================================== //

// Protect all access to `finalizer_list_marked` and `to_finalize`.
// For accessing `ptls->finalizers`, the lock is needed if a thread
// is going to realloc the buffer (of its own list) or accessing the
// list of another thread
extern jl_mutex_t finalizers_lock;
// `ptls->finalizers` and `finalizer_list_marked` might have tagged pointers.
// If an object pointer has the lowest bit set, the next pointer is an unboxed c function pointer.
// If an object pointer has the second lowest bit set, the current pointer is a c object pointer.
//   It must be aligned at least 4, and it finalized immediately (at "quiescence").
// `to_finalize` should not have tagged pointers.
extern arraylist_t finalizer_list_marked;
extern arraylist_t to_finalize;

void schedule_finalization(void *o, void *f) JL_NOTSAFEPOINT;
void run_finalizer(jl_task_t *ct, void *o, void *ff);
void run_finalizers(jl_task_t *ct, int finalizers_thread);
JL_DLLEXPORT void jl_gc_add_finalizer_th(jl_ptls_t ptls, jl_value_t *v, jl_function_t *f) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_finalize_th(jl_task_t *ct, jl_value_t *o);


// =========================================================================== //
// Threading
// =========================================================================== //

extern int gc_n_threads;
extern jl_ptls_t* gc_all_tls_states;

#endif // JL_GC_COMMON_H
