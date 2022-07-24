// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef WS_QUEUE_H
#define WS_QUEUE_H

#include "julia_atomics.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    void **buffer;
    size_t capacity;
} ws_array_t;

ws_array_t *create_ws_array(size_t capacity, size_t eltsz) JL_NOTSAFEPOINT;

// =======
// Chase and Lev's work-stealing queue, optimized for
// weak memory models by Le et al.
//
// * Chase D., Lev Y. Dynamic Circular Work-Stealing queue
// * Le N. M. et al. Correct and Efficient Work-Stealing for
//   Weak Memory Models
// =======

typedef struct {
    _Atomic(int64_t) top;
    _Atomic(int64_t) bottom;
    _Atomic(ws_array_t *) array;
} ws_queue_t;

int ws_queue_push(ws_queue_t *dq, void *elt) JL_NOTSAFEPOINT;

void *ws_queue_pop(ws_queue_t *dq) JL_NOTSAFEPOINT;

void *ws_queue_steal_from(ws_queue_t *dq) JL_NOTSAFEPOINT;

// =======
// Idempotent work-stealing deque
//
// * Michael M. M. et al. Idempotent Work Stealing
// =======

typedef struct {
    uint16_t tail;
    uint16_t tag;
} ws_anchor_t;

typedef struct {
    _Atomic(ws_anchor_t) anchor;
    _Atomic(ws_array_t *) array;
} idemp_ws_queue_t;

int idemp_ws_queue_push(idemp_ws_queue_t *iwsq, void *elt) JL_NOTSAFEPOINT;

void *idemp_ws_queue_pop(idemp_ws_queue_t *iwsq) JL_NOTSAFEPOINT;

void *idemp_ws_queue_steal_from(idemp_ws_queue_t *iwsq) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
