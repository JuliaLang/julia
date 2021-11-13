// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_HTABLE_H
#define JL_HTABLE_H

#define HT_N_INLINE 32

#include "analyzer_annotations.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Power-of-two hash table with linear probing.
//
// Keys and values are stored as in consecutive elements
//   key   = table[2*i]
//   value = table[2*i+1]
// where `2*i < size`. An empty slot at index `i` is indicated with
// `value == HT_NOTFOUND`.
//
// `_space` is reserved space for efficiently allocating small tables.
typedef struct {
    size_t size;
    void **table;
    void *_space[HT_N_INLINE];
} htable_t;

// define this to be an invalid key/value
#define HT_NOTFOUND ((void*)1)

// initialize hash table, reserving space for `size` expected number of
// elements. (Expect `h->size > size` for efficient occupancy factor.)
htable_t *htable_new(htable_t *h, size_t size) JL_NOTSAFEPOINT;
void htable_free(htable_t *h);

// clear and (possibly) change size
void htable_reset(htable_t *h, size_t sz);

// Lookup and mutation. See htable.inc for detail.
#define HTPROT(HTNAME)                                                  \
void *HTNAME##_get(htable_t *h, void *key) JL_NOTSAFEPOINT;             \
void HTNAME##_put(htable_t *h, void *key, void *val) JL_NOTSAFEPOINT;   \
void HTNAME##_adjoin(htable_t *h, void *key, void *val) JL_NOTSAFEPOINT;\
int HTNAME##_has(htable_t *h, void *key) JL_NOTSAFEPOINT;               \
int HTNAME##_remove(htable_t *h, void *key) JL_NOTSAFEPOINT;            \
void **HTNAME##_bp(htable_t *h, void *key) JL_NOTSAFEPOINT;

#define HTPROT_R(HTNAME)                                                \
void *HTNAME##_get_r(htable_t *h, void *key, void *ctx);                \
void HTNAME##_put_r(htable_t *h, void *key, void *val, void *ctx);      \
void HTNAME##_adjoin_r(htable_t *h, void *key, void *val, void *ctx);   \
int HTNAME##_has_r(htable_t *h, void *key, void *ctx);                  \
int HTNAME##_remove_r(htable_t *h, void *key, void *ctx);               \
void **HTNAME##_bp_r(htable_t *h, void *key, void *ctx);

#ifdef __cplusplus
}
#endif

#endif
