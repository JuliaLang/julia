// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include "julia.h"
#include "wsqueue.h"

#ifdef __cplusplus
extern "C" {
#endif

ws_array_t *create_ws_array(size_t capacity)
{
    ws_array_t *a = (ws_array_t *)malloc(sizeof(ws_array_t));
    a->buffer = (void **)malloc(capacity * sizeof(void *));
    a->capacity = capacity;
    return a;
}

// ---------- Chase-Lev work-stealing queue

int ws_queue_push(ws_queue_t *q, void *v)
{
    int64_t b = jl_atomic_load_relaxed(&q->bottom);
    int64_t t = jl_atomic_load_acquire(&q->top);
    ws_array_t *a = jl_atomic_load_relaxed(&q->array);
    if (__unlikely(b - t > a->capacity - 1)) {
        // Queue is full
        return 0;
    }
    jl_atomic_store_relaxed((_Atomic(void *) *)&a->buffer[b % a->capacity], v);
    jl_fence_release();
    jl_atomic_store_relaxed(&q->bottom, b + 1);
    return 1;
}

ws_array_t *ws_queue_resize(ws_queue_t *q)
{
    size_t old_top = jl_atomic_load_relaxed(&q->top);
    ws_array_t *old_a = jl_atomic_load_relaxed(&q->array);
    ws_array_t *new_a = create_ws_array(2 * old_a->capacity);
    // Copy elements into new buffer
    for (size_t i = 0; i < old_a->capacity; ++i) {
        new_a->buffer[(old_top + i) % (2 * old_a->capacity)] =
            old_a->buffer[(old_top + i) % old_a->capacity];
    }
    jl_atomic_store_relaxed(&q->array, new_a);
    return new_a;
}

void *ws_queue_pop(ws_queue_t *q)
{
    int64_t b = jl_atomic_load_relaxed(&q->bottom) - 1;
    ws_array_t *a = jl_atomic_load_relaxed(&q->array);
    jl_atomic_store_relaxed(&q->bottom, b);
#if defined(_CPU_X86_64_)
    __asm__ volatile ("lock orq $0, (%rsp)");
#else
    jl_fence();
#endif
    int64_t t = jl_atomic_load_relaxed(&q->top);
    void *v;
    if (__likely(t <= b)) {
        v = jl_atomic_load_relaxed((_Atomic(void *) *)&a->buffer[b % a->capacity]);
        if (t == b) {
            if (!jl_atomic_cmpswap(&q->top, &t, t + 1))
                v = NULL;
            jl_atomic_store_relaxed(&q->bottom, b + 1);
        }
    }
    else {
        v = NULL;
        jl_atomic_store_relaxed(&q->bottom, b + 1);
    }
    return v;
}

void *ws_queue_steal_from(ws_queue_t *q)
{
    int64_t t = jl_atomic_load_acquire(&q->top);
#if defined(_CPU_X86_64_)
    __asm__ volatile ("lock orq $0, (%rsp)");
#else
    jl_fence();
#endif
    int64_t b = jl_atomic_load_acquire(&q->bottom);
    void *v = NULL;
    if (t < b) {
        ws_array_t *a = jl_atomic_load_relaxed(&q->array);
        v = jl_atomic_load_relaxed((_Atomic(void *) *)&a->buffer[t % a->capacity]);
        if (!jl_atomic_cmpswap(&q->top, &t, t + 1))
            return NULL;
    }
    return v;
}

// ---------- Idempotent work-stealing queue

int idemp_ws_queue_push(idemp_ws_queue_t *iwsq, void *elt)
{
    ws_anchor_t anc = jl_atomic_load_acquire(&iwsq->anchor);
    ws_array_t *ary = jl_atomic_load_relaxed(&iwsq->array);
    if (anc.tail == ary->capacity)
        // Queue overflow
        return 0;
    memcpy(&ary->buffer[anc.tail * iwsq->eltsize], elt, iwsq->eltsize);
    anc.tail++;
    anc.tag++;
    jl_atomic_store_release(&iwsq->anchor, anc);
    return 1;
}

void *idemp_ws_queue_pop(idemp_ws_queue_t *iwsq)
{
    ws_anchor_t anc = jl_atomic_load_acquire(&iwsq->anchor);
    if (anc.tail == 0)
        // Empty queue
        return NULL;
    ws_array_t *ary = jl_atomic_load_relaxed(&iwsq->array);
    void *elt = &ary->buffer[(anc.tail - 1) * iwsq->eltsize];
    anc.tail--;
    jl_atomic_store_release(&iwsq->anchor, anc);
    return elt;
}

void *idemp_ws_queue_steal_from(idemp_ws_queue_t *iwsq)
{
    ws_anchor_t anc = jl_atomic_load_acquire(&iwsq->anchor);
    if (anc.tail == 0)
        // Empty queue
        return NULL;
    ws_array_t *ary = jl_atomic_load_acquire(&iwsq->array);
    void *elt = &ary->buffer[(anc.tail - 1) * iwsq->eltsize];
    ws_anchor_t anc2 = {anc.tail - 1, anc.tag};
    if (!jl_atomic_cmpswap(&iwsq->anchor, &anc, anc2))
        // Steal failed
        return NULL;
    return elt;
}

#ifdef __cplusplus
}
#endif
