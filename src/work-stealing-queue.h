// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef WORK_STEALING_QUEUE_H
#define WORK_STEALING_QUEUE_H

#include <stdalign.h>

#include "julia_atomics.h"
#include "assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// =======
// Chase and Lev's work-stealing queue, optimized for
// weak memory models by Le et al.
//
// * Chase D., Lev Y. Dynamic Circular Work-Stealing queue
// * Le N. M. et al. Correct and Efficient Work-Stealing for
//   Weak Memory Models
// =======

typedef struct {
    char *buffer;
    int32_t capacity;
    int32_t mask;
} ws_array_t;

static inline ws_array_t *create_ws_array(size_t capacity, int32_t eltsz) JL_NOTSAFEPOINT
{
    ws_array_t *a = (ws_array_t *)malloc_s(sizeof(ws_array_t));
    a->buffer = (char *)malloc_s(capacity * eltsz);
    a->capacity = capacity;
    a->mask = capacity - 1;
    return a;
}

static inline void free_ws_array(ws_array_t *a)
{
    free(a->buffer);
    free(a);
}

typedef struct {
    // align to JL_CACHE_BYTE_ALIGNMENT
    alignas(JL_CACHE_BYTE_ALIGNMENT) _Atomic(int64_t) top;
    alignas(JL_CACHE_BYTE_ALIGNMENT) _Atomic(int64_t) bottom;
    alignas(JL_CACHE_BYTE_ALIGNMENT) _Atomic(ws_array_t *) array;
} ws_queue_t;

static inline ws_array_t *ws_queue_push(ws_queue_t *q, void *elt, int32_t eltsz) JL_NOTSAFEPOINT
{
    int64_t b = jl_atomic_load_relaxed(&q->bottom);
    int64_t t = jl_atomic_load_acquire(&q->top);
    ws_array_t *ary = jl_atomic_load_relaxed(&q->array);
    ws_array_t *old_ary = NULL;
    if (__unlikely(b - t > ary->capacity - 1)) {
        ws_array_t *new_ary = create_ws_array(2 * ary->capacity, eltsz);
        for (int i = 0; i < ary->capacity; i++) {
            memcpy(new_ary->buffer + ((t + i) & new_ary->mask) * eltsz, ary->buffer + ((t + i) & ary->mask) * eltsz, eltsz);
        }
        jl_atomic_store_release(&q->array, new_ary);
        old_ary = ary;
        ary = new_ary;
    }
    memcpy(ary->buffer + (b & ary->mask) * eltsz, elt, eltsz);
    jl_fence_release();
    jl_atomic_store_relaxed(&q->bottom, b + 1);
    return old_ary;
}

static inline void ws_queue_pop(ws_queue_t *q, void *dest, int32_t eltsz) JL_NOTSAFEPOINT
{
    int64_t b = jl_atomic_load_relaxed(&q->bottom) - 1;
    ws_array_t *ary = jl_atomic_load_relaxed(&q->array);
    jl_atomic_store_relaxed(&q->bottom, b);
    jl_fence();
    int64_t t = jl_atomic_load_relaxed(&q->top);
    if (__likely(t <= b)) {
        memcpy(dest, ary->buffer + (b & ary->mask) * eltsz, eltsz);
        if (t == b) {
            if (!jl_atomic_cmpswap(&q->top, &t, t + 1))
                memset(dest, 0, eltsz);
            jl_atomic_store_relaxed(&q->bottom, b + 1);
        }
    }
    else {
        memset(dest, 0, eltsz);
        jl_atomic_store_relaxed(&q->bottom, b + 1);
    }
}

static inline void ws_queue_steal_from(ws_queue_t *q, void *dest, int32_t eltsz) JL_NOTSAFEPOINT
{
    int64_t t = jl_atomic_load_acquire(&q->top);
    jl_fence();
    int64_t b = jl_atomic_load_acquire(&q->bottom);
    if (t < b) {
        ws_array_t *ary = jl_atomic_load_relaxed(&q->array);
        memcpy(dest, ary->buffer + (t & ary->mask) * eltsz, eltsz);
        if (!jl_atomic_cmpswap(&q->top, &t, t + 1))
            memset(dest, 0, eltsz);
    }
}

#ifdef __cplusplus
}
#endif

#endif
