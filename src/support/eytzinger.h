// This file is a part of Julia. License is MIT: https://julialang.org/license

// Thread-safe Eytzinger tree implementation for fast address range lookups.
// See https://algorithmica.org/en/eytzinger

#ifndef JL_EYTZINGER_H
#define JL_EYTZINGER_H

#include <uv.h>
#include "dtypes.h"
#include "arraylist.h"

#ifdef __cplusplus
extern "C" {
#endif

// Search an Eytzinger tree for the predecessor boundary of `addr`.
// Returns the tree index (0-based) of the predecessor, or `n`
// (the sentinel) if `addr` is outside all ranges.
// `items` has `n + 1` entries (n boundaries + 1 sentinel).
static inline size_t _eyt_obj_idx(uintptr_t addr, uintptr_t *tree, size_t n,
                                  uintptr_t min_addr, uintptr_t max_addr) JL_NOTSAFEPOINT
{
    if (n == 0)
        return n;
    assert(n % 2 == 0 && "Eytzinger tree not even length!");
    if (addr <= min_addr || addr > max_addr)
        return n;
    size_t k = 1;
    while (k <= n) {
        int greater = (addr > tree[k - 1]);
        k <<= 1;
        k |= greater;
    }
    k >>= (__builtin_ctzll(k) + 1);
    assert(k != 0);
    assert(k <= n && "Eytzinger tree index out of bounds!");
    assert(tree[k - 1] < addr && "Failed to find lower bound for object!");
    return k - 1;
}

typedef struct eyt_tree_t {
    uv_rwlock_t rwlock;

    size_t n; // 2 * n_ranges (tree/idxs have n+1 entries)
    arraylist_t tree;
    arraylist_t idxs;
    uintptr_t min_addr;
    uintptr_t max_addr;

    arraylist_t blobs;
} eyt_tree_t;

JL_DLLEXPORT void eyt_tree_init(eyt_tree_t *t) JL_NOTSAFEPOINT;

// Add a [start, end) range and rebuild the tree.
// Thread-safe for concurrent access.
JL_DLLEXPORT void eyt_tree_add_range(eyt_tree_t *t, uintptr_t start, uintptr_t end) JL_NOTSAFEPOINT;

// Returns whether `addr` is inside any registered range.
// Thread-safe for concurrent readers and writers.
static inline int eyt_tree_is_in_range(eyt_tree_t *t, uintptr_t addr) JL_NOTSAFEPOINT
{
    uv_rwlock_rdlock(&t->rwlock);
    size_t idx = _eyt_obj_idx(addr, (uintptr_t*)t->tree.items, t->n, t->min_addr, t->max_addr);
    int result = ((uintptr_t)t->tree.items[idx] & 1) == 0;
    uv_rwlock_rdunlock(&t->rwlock);
    return result;
}

// Returns which registered range `addr` is inside, if any.
// Thread-safe for concurrent readers and writers.
static inline size_t eyt_tree_find_range_idx(eyt_tree_t *t, uintptr_t addr) JL_NOTSAFEPOINT
{
    uv_rwlock_rdlock(&t->rwlock);
    size_t idx = _eyt_obj_idx(addr, (uintptr_t*)t->tree.items, t->n, t->min_addr, t->max_addr);
    size_t result = (uintptr_t)t->idxs.items[idx];
    uv_rwlock_rdunlock(&t->rwlock);
    return result;
}

#ifdef __cplusplus
}
#endif

#endif // JL_EYTZINGER_H
