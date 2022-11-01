// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include "julia.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    void **buffer;
    size_t capacity;
} ws_array_t;

STATIC_INLINE ws_array_t *create_ws_array(size_t capacity, size_t eltsz)
{
    ws_array_t *a = (ws_array_t *)malloc_s(sizeof(ws_array_t));
    a->buffer = (void **)malloc_s(capacity * eltsz);
    a->capacity = capacity;
    return a;
}

// =======
// Idempotent work-stealing deque
//
// * Michael M. M. et al. Idempotent Work Stealing
// =======

typedef struct {
    int32_t tail;
    int32_t tag;
} ws_anchor_t;

typedef struct {
    _Atomic(ws_anchor_t) anchor;
    _Atomic(ws_array_t *) array;
} idemp_ws_queue_t;

STATIC_INLINE int idemp_ws_queue_push(idemp_ws_queue_t *iwsq, void *elt)
{
    ws_anchor_t anc = jl_atomic_load_acquire(&iwsq->anchor);
    ws_array_t *ary = jl_atomic_load_relaxed(&iwsq->array);
    if (anc.tail == ary->capacity)
        // Queue overflow
        return 0;
    ary->buffer[anc.tail] = elt;
    anc.tail++;
    anc.tag++;
    jl_atomic_store_release(&iwsq->anchor, anc);
    return 1;
}

STATIC_INLINE void *idemp_ws_queue_pop(idemp_ws_queue_t *iwsq)
{
    ws_anchor_t anc = jl_atomic_load_acquire(&iwsq->anchor);
    ws_array_t *ary = jl_atomic_load_relaxed(&iwsq->array);
    if (anc.tail == 0)
        // Empty queue
        return NULL;
    anc.tail--;
    void *elt = ary->buffer[anc.tail];
    jl_atomic_store_release(&iwsq->anchor, anc);
    return elt;
}

STATIC_INLINE void *idemp_ws_queue_steal_from(idemp_ws_queue_t *iwsq)
{
    ws_anchor_t anc = jl_atomic_load_acquire(&iwsq->anchor);
    ws_array_t *ary = jl_atomic_load_acquire(&iwsq->array);
    if (anc.tail == 0)
        // Empty queue
        return NULL;
    void *elt = ary->buffer[anc.tail - 1];
    ws_anchor_t anc2 = {anc.tail - 1, anc.tag};
    if (!jl_atomic_cmpswap(&iwsq->anchor, &anc, anc2))
        // Steal failed
        return NULL;
    return elt;
}

#ifdef __cplusplus
}
#endif